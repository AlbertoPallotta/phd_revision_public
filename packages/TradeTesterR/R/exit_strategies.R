#' Default exit function with fixed, trailing and signal exits
#' @param dis Execution rows for one filled trade, including its fixed levels.
#' @param ambiguity Fixed TP/SL collision policy: unknown (default) or stop_first.
#' @details Numbered dynamic exits are fractions of the remaining position.
#' Signals use their scheduled observed opening; late signals fall back to the
#' permitted terminal close. Exit legs are ordered by execution time before
#' position quantities are calculated. This does not reconstruct an intrabar path.
#' Fixed/dynamic stops and targets use bid to trigger and close a long position,
#' and ask to trigger and close a short position. Orders already executable at
#' an eligible opening use that quote; later touches retain their threshold.
#' A position first entered intrabar cannot exit at that bar's earlier opening.
#' @return A data.table of exit legs with remaining quantities and ambiguity flags.
#' @examples
#' if (requireNamespace("TTR", quietly = TRUE)) {
#'   demo <- new.env(parent = globalenv())
#'   sys.source(system.file("examples", "csv_demo_setup.R",
#'                          package = "TradeTesterR", mustWork = TRUE), envir = demo)
#'   ex <- demo$ex
#'   bt <- backtest(ex$dat, exit_fun=exit_TP_SL_signal,
#'                  CloseTradeOnOppositeSignal=FALSE, verbose=FALSE)
#'   table(bt$results$ExitReason)
#' }
#'
#' @import data.table
#' @export
exit_TP_SL_signal <- function(dis, ambiguity=c("unknown","stop_first")) {
  ambiguity <- match.arg(ambiguity)
  terminal_time <- attr(dis,"terminal_time")
  terminal_reason <- attr(dis,"terminal_reason")
  next_bar <- attr(dis,"next_execution_bar")
  entry_phase <- attr(dis,"entry_phase")
  dis <- data.table::copy(dis)
  if (!nrow(dis)) stop("Exit data is empty.")
  if (is.null(terminal_time)) terminal_time <- tail(dis$date,1)
  if (is.null(terminal_reason)) terminal_reason <- "End of data"
  side <- dis$Side[1]; size <- dis$OrderSize[1]
  # Both trigger and execution use the quote needed to close the position.
  opening_hit <- function(row,level,kind) {
    if (!length(row) || is.na(row) || !is.finite(level)) return(FALSE)
    if (row==1L && identical(entry_phase,"touch")) return(FALSE)
    hit <- if(kind=="stop") {
      if(side>0) dis$Open.b[row]<=level else dis$Open.a[row]>=level
    } else if(side>0) dis$Open.b[row]>=level else dis$Open.a[row]<=level
    isTRUE(hit)
  }
  protective_price <- function(row,level,at_open) {
    if(at_open) { if(side>0) dis$Open.b[row] else dis$Open.a[row] } else level
  }
  observed <- dis[,.(date,Open.a,Open.b)]
  if (!is.null(next_bar)) observed <- rbind(observed,next_bar[,.(date,Open.a,Open.b)])
  candidates <- list()
  add <- function(row,time,price,fraction,reason,priority,phase="touch",ambiguous=FALSE,dynamic=FALSE) {
    if (!length(row) || is.na(row) || !length(time) || is.na(time)) return(invisible(NULL))
    if (!is.finite(fraction) || fraction<=0 || fraction>1) stop("Exit fraction must be in (0, 1].")
    candidates[[length(candidates)+1L]] <<- data.table(row=as.integer(row),
      ExitTime=time,ExitPrice=as.numeric(price),ExitPerc=fraction,ExitReason=reason,
      priority=priority,phase=phase,AmbiguousBar=ambiguous,dynamic=dynamic)
  }
  # Preserve each numbered order's first-hit and remaining-fraction convention.
  direction <- if(side>0) "long" else "short"
  for (kind in c("target","stop")) {
    cols <- grep(paste0("^",kind,"order_",direction,"_[0-9]+$"),names(dis),value=TRUE)
    if (length(cols)) cols <- cols[order(as.integer(sub(".*_","",cols)))]
    for (k in seq_along(cols)) {
      col <- cols[k]; amt <- paste0(col,"_amount")
      if (!all(c(amt,"NextOpenTime") %in% names(dis))) stop("Dynamic exits require amount and NextOpenTime columns.")
      updates <- data.table(time=dis$NextOpenTime,level=dis[[col]],amount=dis[[amt]])
      updates <- updates[!is.na(time) & is.finite(level) & is.finite(amount)]
      updates <- unique(updates,by="time",fromLast=TRUE)
      level <- rep(NA_real_,nrow(dis)); amount <- level
      matched <- match(dis$date,updates$time)
      level <- updates$level[matched]; amount <- updates$amount[matched]
      level <- data.table::nafill(level,"locf"); amount <- data.table::nafill(amount,"locf")
      data.table::set(dis,j=col,value=level); data.table::set(dis,j=amt,value=amount)
      hit <- if(kind=="target") {
        if(side>0) dis$High.b>=level else dis$Low.a<=level
      } else if(side>0) dis$Low.b<=level else dis$High.a>=level
      row <- which(hit %in% TRUE)[1]
      if (!is.na(row)) {
        reason <- paste0(if(kind=="target") "Trailing Target " else "Trailing stop ",
                         if(side>0) "Long: " else "Short: ",100*amount[row],"%")
        at_open <- opening_hit(row,level[row],kind)
        price <- protective_price(row,level[row],at_open)
        add(row,dis$date[row],price,amount[row],reason,10L+k,
            phase=if(at_open) "open" else "touch",dynamic=TRUE)
      }
    }
  }
  # Keep genuine opening timestamps distinct from the last evaluated bar's close.
  signal_cols <- grep(paste0("^exit_",direction,"($|_[0-9]+$)"),names(dis),value=TRUE)
  for (k in seq_along(signal_cols)) {
    col <- signal_cols[k]
    for (row in which(dis[[col]]>0)) {
      when <- dis$NextOpenTime[row]
      if (!length(when) || is.na(when) || when>max(c(observed$date,terminal_time))) next
      if (when < dis$EntryTime[1]) next
      j <- match(when,observed$date)
      if (is.na(j)) stop("Scheduled exit must match an observed opening time and price.")
      price <- dis$NextOpen[row]
      quotes <- c(observed$Open.a[j],observed$Open.b[j])
      if (!is.finite(price) || !any(abs(quotes-price)<=1e-8*pmax(1,abs(price))))
        stop("Scheduled exit must match an observed opening time and price.")
      add(row,when,price,dis[[col]][row],paste0("Exiting ",if(side>0)"Long: " else "Short: ",
          100*dis[[col]][row],"%"),100L+k,"open")
      break
    }
  }
  tp <- if("TakeProfit" %in% names(dis)) {
    which((if(side>0) dis$High.b>=dis$TakeProfit else dis$Low.a<=dis$TakeProfit) %in% TRUE)[1]
  } else NA_integer_
  sl <- if("StopLoss" %in% names(dis)) {
    which((if(side>0) dis$Low.b<=dis$StopLoss else dis$High.a>=dis$StopLoss) %in% TRUE)[1]
  } else NA_integer_
  tp_open <- if(!is.na(tp)) opening_hit(tp,dis$TakeProfit[tp],"target") else FALSE
  sl_open <- if(!is.na(sl)) opening_hit(sl,dis$StopLoss[sl],"stop") else FALSE
  sl_price <- if(!is.na(sl)) protective_price(sl,dis$StopLoss[sl],sl_open) else NA_real_
  tp_price <- if(!is.na(tp)) protective_price(tp,dis$TakeProfit[tp],tp_open) else NA_real_
  if (!is.na(tp) && !is.na(sl) && tp==sl && tp_open==sl_open) {
    add(sl,dis$date[sl],if(ambiguity=="stop_first")sl_price else NA_real_,1,
      if(ambiguity=="stop_first")"StopLoss (same-bar assumption)" else "Exit Price Unknown. TP/SL hit on same candle",
      0L,phase=if(sl_open) "open" else "touch",ambiguous=TRUE)
  } else {
    if(!is.na(tp)) add(tp,dis$date[tp],tp_price,1,"TakeProfit",0L,
                      phase=if(tp_open) "open" else "touch")
    if(!is.na(sl)) add(sl,dis$date[sl],sl_price,1,"StopLoss",0L,
                      phase=if(sl_open) "open" else "touch")
  }
  legs <- list(); remaining <- size; final_phase <- "boundary"; last_exit <- NULL
  blocks_entry_bar <- FALSE
  if(length(candidates)) {
    c <- rbindlist(candidates)
    c[,phase_order:=ifelse(phase=="open",0L,1L)]
    # Retain one dynamic exit per bar. An opening precedes any later touch;
    # within the same phase, retain the legacy last-component convention.
    ix <- which(c$dynamic)
    keep <- rep(TRUE,nrow(c))
    dynamic_order <- ix[order(c$ExitTime[ix],c$phase_order[ix],-ix)]
    keep[ix] <- FALSE
    keep[dynamic_order[!duplicated(c$ExitTime[dynamic_order])]] <- TRUE
    c <- c[keep]
    setorder(c,ExitTime,phase_order,priority,row)
    for (j in seq_len(nrow(c))) {
      if(abs(remaining)<1e-12) break
      leg <- copy(dis[c$row[j]])
      amount <- -remaining*c$ExitPerc[j]; remaining <- remaining+amount
      leg[,`:=`(ExitTime=c$ExitTime[j],ExitPrice=c$ExitPrice[j],ExitPerc=c$ExitPerc[j],
        ExitReason=c$ExitReason[j],ExitAmount=amount,CurrentPosSize=remaining,
        CurrentSize=remaining,OrigOrder=Order_ID,AmbiguousBar=c$AmbiguousBar[j])]
      legs[[length(legs)+1L]] <- leg
      final_phase <- c$phase[j]; last_exit <- c$ExitTime[j]
      # Repricing a protective opening must not admit a new entry on its bar.
      blocks_entry_bar <- c$priority[j]<100L || c$phase[j]=="touch"
    }
  }
  if(abs(remaining)>=1e-12) {
    when <- terminal_time
    price <- if(side>0) tail(dis$Close.b,1) else tail(dis$Close.a,1)
    # A partial scheduled exit at the next observed opening cannot be followed
    # by a residual liquidation retrospectively stamped before that opening.
    if(!is.null(last_exit) && last_exit>when) {
      when <- last_exit; j <- match(when,observed$date)
      price <- if(side>0) observed$Open.b[j] else observed$Open.a[j]
    }
    leg <- copy(dis[.N])
    leg[,`:=`(ExitTime=when,ExitPrice=price,ExitPerc=1,ExitReason=terminal_reason,
      ExitAmount=-remaining,CurrentPosSize=0,CurrentSize=0,OrigOrder=Order_ID,AmbiguousBar=FALSE)]
    legs[[length(legs)+1L]] <- leg; final_phase <- "boundary"
    blocks_entry_bar <- FALSE
  }
  out <- rbindlist(legs,fill=TRUE)
  attr(out,"exit_phase") <- final_phase
  attr(out,"blocks_entry_bar") <- blocks_entry_bar
  out
}
