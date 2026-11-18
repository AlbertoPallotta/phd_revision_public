# Historical Section 3.6 study; preserve its original strategy rules.
# Rscript run_section36.R <new-output-folder> <historical-TT-source> <historical-R-library> <reporting-capital>
# Requires the 14 October executable identified in manifests/historical-access.csv.
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=4L) stop("Missing configuration: supply output folder, historical TT source and R library plus reporting capital 10000.",call.=FALSE)
out <- normalizePath(args[1],winslash="/",mustWork=FALSE)
source_root <- normalizePath(args[2],winslash="/",mustWork=TRUE)
historical_library <- normalizePath(args[3],winslash="/",mustWork=TRUE)
if (file.exists(out)) stop("Output exists; choose a new folder. No evidence will be overwritten.")
if (!file.exists(file.path(source_root,"DESCRIPTION")) || !dir.exists(file.path(historical_library,"TradeTesterR"))) stop("Missing historical TT source/library.")
.libPaths(c(historical_library,.libPaths()))
capital <- as.numeric(args[4])
stopifnot(identical(capital,10000))
root <- source_root
dir.create(file.path(out,"evaluations"),recursive=TRUE,showWarnings=FALSE)
Sys.setenv(TZ="UTC")
suppressPackageStartupMessages({
  library(TradeTesterR); library(data.table); library(xts); library(TTR)
})
setDTthreads(1L)
stopifnot(normalizePath(find.package("TradeTesterR"),winslash="/") ==
          normalizePath(file.path(historical_library,"TradeTesterR"),winslash="/"))
for (pair in list(c("optimizeStrategy","optimization.R"),c("compareISOS","optimization.R"),
                  c("analyzeParameterStability","optimization.R"),c("backtest","backtest.R"),
                  c("exit_TP_SL_signal","exit_strategies.R"),c("analyze_performance","performance_analysis.R"))) {
  expr <- parse(file.path(root,"R",pair[2]),keep.source=FALSE)
  match <- Filter(function(e) is.call(e) && identical(e[[1]],as.name("<-")) &&
                    identical(e[[2]],as.name(pair[1])),as.list(expr))
  stopifnot(length(match)==1L)
  source_fun <- eval(match[[1]][[3]],envir=asNamespace("TradeTesterR"))
  installed <- get(pair[1],envir=asNamespace("TradeTesterR"))
  stopifnot(identical(formals(source_fun),formals(installed)),
            identical(paste(deparse(body(source_fun)),collapse="\n"),
                      paste(deparse(body(installed)),collapse="\n")))
}
input <- file.path(root,"inst/extdata/crude_wti_futures.csv")
df <- fread(input)
setnames(df,c("date","Open","High","Low","Close","Volume"))
stopifnot(inherits(df$date,"POSIXct"),!anyNA(df$date),!anyDuplicated(df$date))
setorder(df,date)
price_small <- df[,.(date,Open,High,Low,Close)]
daily_xts <- to.period(xts(price_small[,.(Open,High,Low,Close)],order.by=price_small$date),
                       period="days",k=1,indexAt="startof",name=NULL,drop.time=FALSE)
daily <- data.table(date=index(daily_xts),coredata(daily_xts))
setkey(daily,date)
fwrite(daily,file.path(out,"daily-ohlc.csv"))
saveRDS(daily,file.path(out,"daily-ohlc.rds"))
writeLines(capture.output(sessionInfo()),file.path(out,"session-info.txt"))

# Retain original parameter order: Bollinger period varies fastest.
grid <- expand.grid(boll_n=seq(20,100,10),stop_loss_amount=seq(1000,4000,1000))
fwrite(grid,file.path(out,"parameter-grid.csv"))
plan <- list()
begin <- min(daily$date)
while (begin<max(daily$date)) {
  train_end <- begin+lubridate::weeks(52)
  test_end <- train_end+lubridate::weeks(26)
  test <- daily[date>=train_end & date<test_end]
  if (nrow(test)<100) break
  plan[[length(plan)+1L]] <- data.table(
    window=length(plan)+1L,train_start=begin,train_end=train_end,
    test_start=train_end,test_end=test_end,train_rows=sum(daily$date>=begin & daily$date<train_end),
    test_rows=nrow(test),last_observed_test_row=max(test$date),
    data_cutoff=max(df$date),scheduled_end_after_data=test_end>max(df$date)+900)
  begin <- begin+lubridate::weeks(26)
}
plan <- rbindlist(plan)
fwrite(plan,file.path(out,"planned-windows.csv"))
cat("Reporting capital",capital,";",nrow(grid),"candidates;",nrow(plan),"windows.\n")

# Evaluate the exact original strategy body, with a clean per-call input.
expressions <- parse(file.path(root,"inst/examples/boilerplate.R"),keep.source=FALSE)
definition <- Filter(function(e) is.call(e) && identical(e[[1]],as.name("<-")) &&
                       identical(e[[2]],as.name("my_strat")),as.list(expressions))
stopifnot(length(definition)==1)
instrument_type <- "Futures"
is_trade_in_shares <- FALSE
trade_size_dollars <- 10000
trade_size_shares <- 100
trade_size_contracts <- 4
futures_multiplier <- 50
spread <- .04
callback_env <- new.env(parent=globalenv())
callback_env$backtest <- function(dat,...) {
  instructions <- dat[!is.na(OrderSize) & OrderSize!=0]
  stopifnot(all(instructions$BuySignal %in% TRUE),
            all(is.finite(instructions$lowerBB)),
            all(is.finite(instructions$upperBB)),
            all(instructions$OrderSize==200))
  TradeTesterR::backtest(dat,...)
}
original_callback <- eval(definition[[1]][[3]],envir=callback_env)
writeLines(deparse(original_callback),file.path(out,"executed-callback-body.R"))
counter <- 0L
evaluation_records <- list()
all_warnings <- character()
strategy <- function(price,stop_loss_amount=2000,boll_n=20) {
  counter <<- counter+1L
  window <- ((counter-1L) %/% (nrow(grid)+1L))+1L
  within <- ((counter-1L) %% (nrow(grid)+1L))+1L
  stage <- if(within<=nrow(grid)) "IS" else "OOS"
  bound <- attr(price,"execution_end")
  start <- if(stage=="IS") plan$train_start[window] else plan$test_start[window]
  expected_end <- if(stage=="IS") plan$train_end[window] else plan$test_end[window]
  stopifnot(identical(as.numeric(bound),as.numeric(expected_end)))
  fresh <- copy(price[,.(date,Open,High,Low,Close)])
  attr(fresh,"execution_end") <- bound
  stopifnot(all(fresh$date>=start & fresh$date<bound))
  before <- copy(fresh)
  bt <- original_callback(fresh,stop_loss_amount=stop_loss_amount,boll_n=boll_n)
  r <- bt$results
  if (nrow(r)) {
    stopifnot(all(is.finite(r$Returns)),all(r$EntryTime>=start),
              all(r$ExitTime<=bound),all(r$ExitTime>=r$EntryTime),
              all(abs(r$Returns - (-r$ExitAmount)*(r$ExitPrice-r$EntryPrice))<1e-6))
    for (id in unique(r$Order_ID)) {
      legs <- r[Order_ID==id][order(ExitTime)]
      remaining <- legs$OrderSize[1]
      for (j in seq_len(nrow(legs))) {
        stopifnot(abs(legs$ExitAmount[j]+remaining*legs$ExitPerc[j])<1e-7)
        remaining <- remaining+legs$ExitAmount[j]
        stopifnot(abs(remaining-legs$CurrentPosSize[j])<1e-7)
      }
      stopifnot(abs(remaining)<1e-7)
    }
    first <- r[,.(actual_entry=first(EntryTime)),by=Order_ID]
    c <- merge(first,bt$orders,by="Order_ID")
    stopifnot(nrow(c)==nrow(first),all(c$actual_entry>=c$ActivationTime),
              all(c$actual_entry<c$EntryValidUntil))
    m10 <- analyze_performance(bt,initial_equity=10000,risk_free=NULL,start=start,end=bound)
    m100 <- analyze_performance(bt,initial_equity=100000,risk_free=NULL,start=start,end=bound)
    stopifnot(identical(m10$total_pnl,m100$total_pnl))
    nonpositive <- m10$nonpositive_equity
    sh10 <- m10$sharpe; sh100 <- m100$sharpe
  } else {
    nonpositive <- FALSE; sh10 <- NA_real_; sh100 <- NA_real_
  }
  # Repeated execution OHLC is reconstructible from frozen input + callback.
  # Preserve complete order/trade records for every evaluation, without 629
  # duplicate copies of the full intraday feed.
  bt$data <- NULL
  identity <- sprintf("%03d-%s-boll%03d-stop%04d",window,stage,boll_n,stop_loss_amount)
  saveRDS(list(identity=identity,stage=stage,window=window,start=start,end=bound,
               parameters=list(boll_n=boll_n,stop_loss_amount=stop_loss_amount),
               strategy_input=before,backtest=bt),
          file.path(out,"evaluations",paste0(identity,".rds")),compress="gzip")
  evaluation_records[[counter]] <<- data.table(
    call=counter,window=window,stage=stage,boll_n=boll_n,stop_loss_amount=stop_loss_amount,
    start=start,end=bound,instructions=nrow(bt$orders),
    trades=if(nrow(r))uniqueN(r$Order_ID) else 0L,legs=nrow(r),
    pnl=if(nrow(r))sum(r$Returns) else 0,
    nonpositive_equity_10000=nonpositive,sharpe_10000=sh10,sharpe_100000=sh100,
    ambiguous_rows=if(nrow(r))sum(r$AmbiguousBar %in% TRUE | r$AmbiguousEntry %in% TRUE) else 0)
  if (within==1L || stage=="OOS") {
    fwrite(rbindlist(evaluation_records),file.path(out,"evaluation-audit.csv"))
    cat("Window",window,stage,"call",counter,"of",nrow(plan)*(nrow(grid)+1L),
        "P&L",if(nrow(r))sum(r$Returns) else 0,"\n")
    flush.console()
  }
  bt
}
started <- Sys.time()
result <- withCallingHandlers(
  optimizeStrategy(daily,grid,initial_equity=capital,optimize_on="net_profit",
                   train_set_weeks=52,test_set_weeks=26,
                   return_optimization_details=TRUE,strategy=strategy,risk_free=NULL,verbose=FALSE),
  warning=function(w) { all_warnings <<- c(all_warnings,conditionMessage(w)); invokeRestart("muffleWarning") })
stopifnot(counter==nrow(plan)*37L,length(result$test_results)==nrow(plan),
          all(vapply(result$optimization_history,nrow,integer(1))==36L))
saveRDS(result,file.path(out,"X10-optimization.rds"),compress="xz")
fwrite(rbindlist(evaluation_records),file.path(out,"evaluation-audit.csv"))
fwrite(result$windows,file.path(out,"windows.csv"))
history <- rbindlist(lapply(seq_along(result$optimization_history),function(i)
  cbind(data.table(window=i,grid_row=seq_len(36)),result$optimization_history[[i]])))
fwrite(history,file.path(out,"optimization-history.csv"))
stability <- analyzeParameterStability(result)
comparison <- compareISOS(result)
saveRDS(stability[setdiff(names(stability),"plots")],file.path(out,"parameter-stability.rds"))
saveRDS(comparison[setdiff(names(comparison),"plots")],file.path(out,"IS-OOS-comparison.rds"))
fwrite(stability$selected_parameters,file.path(out,"selected-parameters.csv"))
fwrite(stability$stability_metrics,file.path(out,"stability-metrics.csv"))
fwrite(comparison$performance_comparison,file.path(out,"IS-OOS-comparison.csv"))
fwrite(as.data.table(comparison$summary_stats),file.path(out,"IS-OOS-summary.csv"))
for (i in seq_along(result$test_results)) {
  fwrite(result$test_results[[i]]$results,file.path(out,sprintf("OOS-%02d-trades.csv",i)))
  fwrite(result$test_results[[i]]$orders,file.path(out,sprintf("OOS-%02d-orders.csv",i)))
}
writeLines(all_warnings,file.path(out,"strategy-warnings.txt"))
writeLines(c(paste("Started:",started),paste("Completed:",Sys.time()),
             paste("Seconds:",as.numeric(difftime(Sys.time(),started,units="secs"))),
             paste("Evaluations:",counter),"Capital:10000; fixed-size P&L also checked at100000.",
             "No section3.5 reruns. No package changes. All inputs fresh per evaluation."),
           file.path(out,"RUN_COMPLETE.txt"))
cat("Completed",counter,"evaluations.\n")
print(comparison$summary_stats)
