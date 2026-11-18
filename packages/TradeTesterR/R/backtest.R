#' Run a backtest on a trading strategy
#' @description Processes dated entries and exits on supplied execution bars.
#' @param dat Merged execution OHLC and strategy columns, with unique POSIXct date.
#' @param exit_fun Exit callback; NULL uses exit_TP_SL_signal.
#' @param exit_args Additional exit callback arguments.
#' @param CloseTradeOnOppositeSignal Close/cancel at the next opposite activation.
#' @param AddToPosition Evaluate overlapping entries independently; no aggregate netting or entry OCO.
#' @param TradeTimeLimit Maximum duration from activation, including pending time.
#' @param verbose Print completed trades.
#' @param end_time Exclusive execution horizon; incomplete crossing bars are excluded.
#' @details Limit/stop entries expire at the next strategy boundary after activation.
#' The clock uses sparse NextOpenTime/NextOpen markers, or the execution bars when
#' no strategy clock is supplied. Optional EntryValidUntil supplies explicit expiry.
#' Fixed protective exits survive entry expiry. Opening fills precede touches;
#' unresolved ties use instruction order and are flagged. Stop or limit entries
#' executable at an eligible opening use its ask (buy) or bid (sell); intrabar
#' touches retain threshold prices. With no supplied strategy clock, off-grid
#' activations remain valid through the first eligible execution bar.
#' Opposite signals are defined by signed size and finite activation time, even
#' when their entry price is invalid; set CloseTradeOnOppositeSignal=FALSE to disable.
#' Instructions while holding are
#' ignored in single-position mode; an intrabar exit cannot admit same-bar re-entry.
#' Callbacks must conserve quantity and use valid execution times; missing
#' CurrentPosSize is derived from chronologically ordered exit quantities.
#' @return List with results, data and one-row-per-instruction orders audit.
#' @examples
#' if (requireNamespace("TTR", quietly = TRUE)) {
#'   demo <- new.env(parent = globalenv())
#'   sys.source(system.file("examples", "csv_demo_setup.R",
#'                          package = "TradeTesterR", mustWork = TRUE), envir = demo)
#'   ex <- demo$ex
#'   bt <- backtest(ex$dat, CloseTradeOnOppositeSignal=FALSE, verbose=FALSE)
#'   head(bt$orders)
#' }
#'
#' @import data.table
#' @export
backtest <- function(dat, exit_fun=NULL, exit_args=list(),
                     CloseTradeOnOppositeSignal=TRUE, AddToPosition=FALSE,
                     TradeTimeLimit=lubridate::weeks(4), verbose=TRUE, end_time=NULL) {
  dat <- data.table::copy(data.table::as.data.table(dat))
  if (!inherits(dat$date, "POSIXct") || anyNA(dat$date) || anyDuplicated(dat$date))
    stop("dat$date must contain unique, nonmissing POSIXct timestamps.")
  if ("AmbiguousEntry" %in% names(dat)) stop("AmbiguousEntry is reserved for backtest results.")
  data.table::setorder(dat, date)
  span <- as.numeric(diff(dat$date), units="secs")
  spacing <- if (length(span)) stats::median(span) else 0
  if (!is.finite(spacing) || spacing < 0) stop("Invalid execution timestamps.")
  scoped <- .execution_context$end
  if (!is.null(scoped)) end_time <- if (is.null(end_time)) scoped else min(end_time, scoped)
  if (!is.null(end_time)) {
    if (!inherits(end_time,"POSIXct") || length(end_time)!=1L || !is.finite(end_time))
      stop("end_time must be one finite POSIXct timestamp.")
    dat <- dat[date + spacing <= end_time & date < end_time]
  }
  if (!nrow(dat)) stop("No complete execution bars before end_time.")
  times <- as.numeric(dat$date)
  tz <- attr(dat$date,"tzone"); if (is.null(tz) || !length(tz)) tz <- "UTC"
  posix <- function(x) as.POSIXct(x,origin="1970-01-01",tz=tz[1])
  final_time <- tail(times,1)+spacing
  if (!is.null(end_time)) final_time <- min(final_time,as.numeric(end_time))
  # Clock events are independent of whether a row carries an entry signal.
  clock <- numeric()
  if ("NextOpenTime" %in% names(dat)) {
    marks <- which(is.finite(as.numeric(dat$NextOpenTime)))
    clock <- sort(unique(c(times[marks],as.numeric(dat$NextOpenTime[marks]))))
  }
  if (!length(clock) && "NextOpen" %in% names(dat)) clock <- times[is.finite(dat$NextOpen)]
  execution_clock <- !length(clock)
  if (execution_clock) clock <- times
  next_boundary <- function(t) {
    j <- findInterval(t,clock)+1L
    out <- rep(final_time,length(t))
    ok <- !is.na(j) & j<=length(clock)
    out[ok] <- clock[j[ok]]
    out[is.na(j)] <- NA_real_
    out
  }
  entry <- dat[!is.na(OrderSize) & OrderSize!=0]
  entry[, `:=`(Order_ID=seq_len(.N), Side=sign(OrderSize),
                prev_order=shift(OrderSize), next_order=shift(OrderSize,type="lead"))]
  if (!"EntryTime" %in% names(entry))
    entry[, EntryTime := posix(next_boundary(as.numeric(date)))]
  if (!"EntryType" %in% names(entry)) entry[,EntryType:="Market"]
  if (!"EntryPrice" %in% names(entry)) {
    at <- findInterval(as.numeric(entry$EntryTime),times)
    at <- at + (at==0L | times[pmax(at,1L)] < as.numeric(entry$EntryTime))
    entry[, EntryPrice := ifelse(Side>0,dat$Open.a[at],dat$Open.b[at])]
  }
  empty_results <- function() {
    r <- copy(entry[0])
    r[, `:=`(ExitTime=posix(numeric()),ExitPrice=numeric(),ExitAmount=numeric(),
      ExitReason=character(),CurrentPosSize=numeric(),CurrentSize=numeric(),
      Returns=numeric(),AmbiguousBar=logical(),AmbiguousEntry=logical())]
    r
  }
  act <- as.numeric(entry$EntryTime); n <- nrow(entry)
  orders <- data.table(Order_ID=entry$Order_ID,SourceTime=entry$date,
    ActivationTime=entry$EntryTime,EntryType=entry$EntryType,Side=entry$Side,
    EntryPrice=entry$EntryPrice,OrderSize=entry$OrderSize,
    EntryValidUntil=posix(rep(NA_real_,n)),Disposition=rep("pending",n),
    DispositionTime=posix(rep(NA_real_,n)),WinnerOrder_ID=rep(NA_integer_,n),
    AmbiguousEntry=rep(FALSE,n),CompetingOrderIDs=rep("",n))
  if ("OrderRuleTime" %in% names(entry)) orders[,SourceTime:=entry$OrderRuleTime]
  if (!n) return(list(results=empty_results(),data=dat,orders=orders))
  cancel_times <- if ("CancelOrder" %in% names(dat)) times[which(dat$CancelOrder %in% TRUE)] else numeric()
  ttl <- as.numeric(entry$EntryTime + TradeTimeLimit)
  cutoff <- pmin(ttl,if(is.null(end_time)) Inf else as.numeric(end_time))
  terminal_reason <- ifelse(cutoff==ttl,"Trade time limit","Execution horizon")
  terminal_reason[cutoff>final_time & is.null(end_time)] <- "End of data"
  opposite <- rep(Inf,n)
  for (s in c(-1,1)) {
    ids <- which(entry$Side==s & is.finite(act))
    other <- sort(unique(act[entry$Side == -s & is.finite(act)]))
    j <- findInterval(act[ids],other)+1L
    valid <- j<=length(other)
    opposite[ids[valid]] <- other[j[valid]]
  }
  if (CloseTradeOnOppositeSignal) {
    terminal_reason[opposite < cutoff] <- "Opposite signal"
    cutoff <- pmin(cutoff,opposite)
  }
  fill_index <- rep(NA_integer_,n); phase <- rep(1L,n)
  expires <- rep(final_time,n); expiry_reason <- rep("unfilled_at_horizon",n)
  valid <- is.finite(act) & is.finite(entry$OrderSize) & is.finite(entry$EntryPrice) &
    entry$EntryType %in% c("Market","Limit","Stop")
  orders[!valid,`:=`(Disposition="invalid_instruction",DispositionTime=SourceTime)]
  boundaries <- next_boundary(act)
  # Only the inferred execution clock may extend an off-grid activation to its
  # first observed bar. Explicit strategy boundaries and expiry still win.
  first_rows <- findInterval(act,times)
  first_rows <- first_rows + (first_rows==0L | times[pmax(first_rows,1L)]<act)
  if (execution_clock) {
    observed <- which(is.finite(act) & first_rows<=length(times))
    boundaries[observed] <- next_boundary(times[first_rows[observed]])
  }
  if ("EntryValidUntil" %in% names(entry)) {
    supplied <- as.numeric(entry$EntryValidUntil)
    boundaries[is.finite(supplied)] <- supplied[is.finite(supplied)]
  }
  cancel_index <- findInterval(act,cancel_times)+1L
  orders[valid,EntryValidUntil:=posix(boundaries[valid])]
  for (i in which(valid)) {
    boundary <- boundaries[i]
    explicit <- Inf
    if (length(cancel_times)) {
      j <- cancel_index[i]
      if (j<=length(cancel_times)) explicit <- cancel_times[j]
    }
    # Entry expiry never limits the lifetime of the filled position.
    limits <- c(explicit,if(CloseTradeOnOppositeSignal) opposite[i] else Inf,
                ttl[i],boundary,final_time)
    reasons <- c("cancelled_explicit","cancelled_opposite_signal","expired_ttl",
                 "expired_entry_bar","unfilled_at_horizon")
    k <- which.min(limits); expires[i] <- limits[k]; expiry_reason[i] <- reasons[k]
  }
  # Search each sorted clock in bulk, avoiding a full sortedness scan per order.
  last_rows <- findInterval(expires,times)
  last_rows <- last_rows - (last_rows>0L & times[pmax(last_rows,1L)]>=expires)
  exit_rows <- findInterval(cutoff,times)
  exit_rows <- exit_rows - (exit_rows>0L & times[pmax(exit_rows,1L)]>=cutoff)
  candle_markers <- if("NextOpen" %in% names(dat)) which(!is.na(dat$NextOpen)) else integer()
  for (i in which(valid)) {
    if (expires[i] <= act[i]) next
    lo <- first_rows[i]; hi <- last_rows[i]
    if (lo>hi || lo>length(times)) next
    rows <- seq.int(lo,hi)
    price <- entry$EntryPrice[i]; side <- entry$Side[i]; type <- entry$EntryType[i]
    hits <- if(type=="Market") TRUE else if(type=="Limit") {
      if(side>0) dat$Low.a[rows]<=price else dat$High.b[rows]>=price
    } else if(side>0) dat$High.a[rows]>=price else dat$Low.b[rows]<=price
    j <- which(hits %in% TRUE)[1]
    if (!is.na(j)) {
      f <- rows[j]; fill_index[i] <- f
      opening <- if(side>0) dat$Open.a[f] else dat$Open.b[f]
      at_open <- type=="Market" || if(type=="Limit") {
        if(side>0) opening<=price else opening>=price
      } else if(side>0) opening>=price else opening<=price
      phase[i] <- if(isTRUE(at_open)) 0L else 1L
    }
  }
  # At each timestamp: expiry/cancellation, admission, opening fill, touch.
  events <- rbindlist(list(
    data.table(t=expires[valid],kind=0L,id=which(valid)),
    data.table(t=act[valid],kind=1L,id=which(valid)),
    data.table(t=times[fill_index[!is.na(fill_index)]],kind=2L+phase[!is.na(fill_index)],id=which(!is.na(fill_index)))))
  setorder(events,t,kind,id)
  active <- rep(FALSE,n); until <- -Inf; until_intrabar <- FALSE
  trades <- list(); total <- 0
  finish_order <- function(ids,reason,t,winner=NA_integer_) {
    if (!length(ids)) return(invisible(NULL))
    orders[ids,`:=`(Disposition=reason,DispositionTime=posix(t),WinnerOrder_ID=winner)]
    active[ids] <<- FALSE
  }
  for (event in seq_len(nrow(events))) {
    i <- events$id[event]; t <- events$t[event]; kind <- events$kind[event]
    if (orders$Disposition[i] != "pending") next
    if (kind==0L) {
      finish_order(i,expiry_reason[i],t)
    } else if (kind==1L) {
      if (!AddToPosition && (t<until || (t==until && until_intrabar)))
        finish_order(i,"ignored_holding",t)
      else active[i] <- TRUE
    } else if (active[i]) {
      competitors <- if(AddToPosition) i else which(active & !is.na(fill_index) &
        fill_index==fill_index[i] & phase==phase[i])
      winner <- min(competitors)
      if(i!=winner) next
      ambiguous <- length(competitors)>1L
      this.entry <- copy(entry[i]); fill <- fill_index[i]
      this.entry[,`:=`(EntryTime=dat$date[fill],AmbiguousEntry=ambiguous)]
      # The order audit retains the requested threshold; the ledger uses the fill.
      if (this.entry$EntryType %in% c("Stop","Limit") && phase[i] == 0L)
        this.entry[,EntryPrice:=if(Side>0) dat$Open.a[fill] else dat$Open.b[fill]]
      end <- cutoff[i]; reason <- terminal_reason[i]
      hi <- min(exit_rows[i],length(times))
      if ("CLOSE_CANDLES_SINCE_ENTRY" %in% names(entry) && "NextOpen" %in% names(dat)) {
        count <- entry$CLOSE_CANDLES_SINCE_ENTRY[i]
        if (is.finite(count) && count>0) {
          marks <- candle_markers[candle_markers>=fill & candle_markers<=hi]
          if (length(marks)>=count+1L) {
            end <- times[marks[count+1L]]; hi <- marks[count+1L]-1L; reason <- "Candle time limit"
          }
        }
      }
      if (hi<fill) { finish_order(i,"unfilled_at_horizon",t); next }
      this.dat <- copy(dat[seq.int(fill,hi)])
      this.dat[,`:=`(Side=this.entry$Side,OrderSize=this.entry$OrderSize,
        EntryTime=this.entry$EntryTime,EntryPrice=this.entry$EntryPrice,Order_ID=this.entry$Order_ID)]
      this.dat[,TakeProfit:=if("TakeProfit" %in% names(this.entry)) this.entry$TakeProfit else Inf*this.entry$Side]
      this.dat[,StopLoss:=if("StopLoss" %in% names(this.entry)) this.entry$StopLoss else -Inf*this.entry$Side]
      terminal <- min(times[hi]+spacing,end,final_time)
      next_row <- if(hi<length(times) && times[hi+1L]<=end) dat[hi+1L] else NULL
      attr(this.dat,"terminal_time") <- posix(terminal)
      attr(this.dat,"terminal_reason") <- reason
      attr(this.dat,"next_execution_bar") <- next_row
      attr(this.dat,"entry_phase") <- if(phase[i]==0L) "open" else "touch"
      fun <- if(is.null(exit_fun)) exit_TP_SL_signal else exit_fun
      exit <- data.table::as.data.table(do.call(fun,c(list(copy(this.dat)),exit_args)))
      exit_phase <- attr(exit,"exit_phase")
      blocks_entry_bar <- attr(exit,"blocks_entry_bar")
      allowed_times <- c(this.dat$date,posix(terminal),if(!is.null(next_row))next_row$date)
      if (!nrow(exit) || anyNA(exit$ExitTime) || any(!is.finite(exit$ExitTime)) ||
          any(exit$ExitTime < this.entry$EntryTime) || any(!exit$ExitTime %in% allowed_times))
        stop("Exit function returned missing or out-of-horizon exit times.")
      setorder(exit,ExitTime)
      remaining <- this.entry$OrderSize+cumsum(exit$ExitAmount)
      if (!"CurrentPosSize" %in% names(exit)) exit[,CurrentPosSize:=remaining]
      if (any(!is.finite(exit$ExitAmount)) || any(sign(exit$ExitAmount)!= -this.entry$Side) ||
          any(remaining*this.entry$Side < -1e-8) || abs(tail(remaining,1))>1e-8*max(1,abs(this.entry$OrderSize)) ||
          any(!is.finite(exit$CurrentPosSize)) || any(abs(exit$CurrentPosSize-remaining)>1e-8*max(1,abs(this.entry$OrderSize))))
        stop("Exit function did not conserve the entered position.")
      until <- as.numeric(max(exit$ExitTime))
      until_intrabar <- if(!is.null(blocks_entry_bar)) isTRUE(blocks_entry_bar) else
        if(!is.null(exit_phase)) identical(exit_phase,"touch") else
        !(until==terminal || (!is.null(next_row) && until==as.numeric(next_row$date)))
      finish_order(i,"filled",t,i)
      orders[i,`:=`(AmbiguousEntry=ambiguous,CompetingOrderIDs=paste(setdiff(competitors,i),collapse=","))]
      if (!AddToPosition) finish_order(which(active),"cancelled_competing_fill",t,i)
      thistrade <- cbind(this.entry[rep(1L,nrow(exit))],exit[,!names(exit) %in% names(this.entry),with=FALSE])
      thistrade[,Returns:=Side*(ExitPrice-EntryPrice)*abs(ExitAmount)]
      trades[[length(trades)+1L]] <- thistrade
      total <- total+sum(thistrade$Returns,na.rm=TRUE)
      if(verbose) message("Trade ",i,": ",round(sum(thistrade$Returns,na.rm=TRUE),1)," | Total: ",round(total,1))
    }
  }
  results <- if(length(trades)) rbindlist(trades,fill=TRUE) else empty_results()
  if (nrow(results)) dat <- merge(dat,results[,c("date",setdiff(names(results),names(dat))),with=FALSE],all=TRUE)
  list(results=results,data=dat,orders=orders)
}

# A synchronous strategy call owns this context; restore it even after an error.
.execution_context <- new.env(parent=emptyenv())
.with_execution_end <- function(end,fun,args) {
  previous <- .execution_context$end
  on.exit(.execution_context$end <- previous)
  .execution_context$end <- if(is.null(previous)) end else min(previous,end)
  do.call(fun,args)
}
