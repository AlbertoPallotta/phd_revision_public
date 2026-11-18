source(file.path(Sys.getenv("PT_BUILDER_ROOT"),"thesis-reproduction/scripts/common.R"))
stopifnot(file.exists(file.path(repdir,"evidence/synthetic-checks.csv")),
          all(fread(file.path(repdir,"evidence/synthetic-checks.csv"))$status=="PASSED"),
          all(fread(file.path(repdir,"evidence/identity-checks.csv"))$status=="PASSED"))
out <- file.path(repdir,"results/study")
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
warnings_seen <- list();calls <- list();completed <- list();phase <- "A1"
flush_calls <- function() {
  if(length(calls)) fwrite(rbindlist(calls,fill=TRUE),file.path(out,"evaluation-attempts.csv"),na="NA")
  if(length(completed)) fwrite(rbindlist(completed,fill=TRUE),file.path(out,"candidate-scores.csv"),na="NA")
}
load_samples <- new.env()
data(list=c("sample_prices_weekly","sample_prices_daily"),package="PortfolioTesteR",envir=load_samples)
weekly <- as.data.frame(copy(load_samples$sample_prices_weekly))
daily <- as.data.frame(copy(load_samples$sample_prices_daily))
original_weekly <- copy(weekly);original_daily <- copy(daily)
cfg <- recipes$thesis_b3_config()
saveRDS(cfg,file.path(out,"configuration.rds"))
dput(cfg,file=file.path(out,"configuration.R"))
saveRDS(list(weekly=weekly,daily=daily),file.path(out,"bundled-input-panels.rds"))
fwrite(data.frame(symbol=names(weekly)[-1]),file.path(out,"symbols.csv"))
set.seed(1) # No stochastic strategy component; record a fixed process seed.
withCallingHandlers({
  a1 <- recipes$thesis_multifactor_a1(weekly,daily,execute=FALSE)
  expected_dates <- read.csv(file.path(root,"thesis-reproduction/evidence/bundled-grouped-date-grid.csv"))
  stopifnot(identical(as.character(a1$prices$Date),as.character(expected_dates$grouped_date)),
            identical(names(a1$prices),names(weekly)), nrow(a1$prices)==39,
            identical(weekly,original_weekly),identical(daily,original_daily))
  calls[[1]] <- data.frame(id="A1",phase="A1",window=NA_integer_,lookback=12,n_top=5,
                          price_rows=39,first=as.character(a1$prices$Date[1]),last=as.character(tail(a1$prices$Date,1)))
  flush_calls()
  a1$backtest <- run_backtest(copy(a1$prices),copy(a1$weights),initial_capital=100000,name="Thesis A1 multifactor",verbose=FALSE)
  saveRDS(a1,file.path(out,"A1.rds"))
  completed[[1]] <- data.frame(id="A1",phase="A1",window=NA_integer_,lookback=12,n_top=5,
                               score=NA_real_,total_return=a1$backtest$total_return,error="")
  flush_calls();cat("Completed A1\n")
  phase <- "grid"; phase_count <- 0L; pending <- NULL
  recording_builder <- function(prices,params,...) {
    phase_count <<- phase_count+1L
    window <- if(phase=="grid") NA_integer_ else ((phase_count-1L)%/%7L)+1L
    role <- if(phase=="grid") "grid" else if((phase_count-1L)%%7L<6L) "IS" else "OOS"
    id <- if(phase=="grid") sprintf("grid_%02d",phase_count) else sprintf("WF%02d_%s_%02d",window,role,(phase_count-1L)%%7L+1L)
    stopifnot((phase=="grid" && phase_count<=12) || (phase=="WF" && phase_count<=28))
    calls[[length(calls)+1L]] <<- data.frame(id=id,phase=role,window=window,
      lookback=params$lookback,n_top=params$n_top,price_rows=nrow(prices),
      first=as.character(prices$Date[1]),last=as.character(tail(prices$Date,1)))
    flush_calls()
    panels <- recipes$thesis_b3_panels(prices,params)
    pending <<- list(id=id,phase=role,window=window,params=params,
                    supplied_prices=copy(prices),panels=panels)
    panels$weights
  }
  recording_metric <- function(bt) {
    # Delegate exactly to the package default; the wrapper only records evidence.
    score <- PortfolioTesteR::metric_sharpe(bt)
    record <- pending;record$backtest <- bt;record$score <- score
    saveRDS(record,file.path(out,paste0(record$id,".rds")))
    completed[[length(completed)+1L]] <<- data.frame(id=record$id,phase=record$phase,window=record$window,
      lookback=record$params$lookback,n_top=record$params$n_top,score=score,total_return=bt$total_return,error="")
    flush_calls()
    cat("Completed",record$id,"score",format(score,digits=16),"\n")
    score
  }
  grid <- run_param_grid(prices=copy(weekly),grid=cfg$grid,builder=recording_builder,
    metric=recording_metric,name_prefix="B3_grid",verbose=TRUE,
    light_mode=cfg$light_mode,precompute_returns=TRUE)
  saveRDS(grid,file.path(out,"B3-grid.rds"))
  stopifnot(grid$n_tested==12,grid$n_valid==12,phase_count==12)
  phase <- "WF";phase_count <- 0L
  wf <- run_walk_forward(prices=copy(weekly),grid=cfg$wf_grid,builder=recording_builder,
    metric=recording_metric,is_periods=cfg$is_periods,oos_periods=cfg$oos_periods,
    step=cfg$step,warmup_periods=cfg$warmup_periods,verbose=TRUE,
    light_mode=cfg$light_mode,precompute_all=cfg$precompute_all,builder_args=list())
  saveRDS(wf,file.path(out,"B3-WF.rds"))
  fwrite(wf$optimization_summary,file.path(out,"WF-paired-scores.csv"))
  fwrite(wf$oos_stitched,file.path(out,"WF-raw-stitched.csv"))
  display <- copy(wf$oos_stitched)
  display$Date[1] <- as.Date(wf$oos_results[[1]]$dates[1])
  stopifnot(identical(display$Value,wf$oos_stitched$Value))
  fwrite(display,file.path(out,"WF-display-equity.csv"))
  stopifnot(phase_count==28,length(calls)==41,length(completed)==41,
            nrow(wf$optimization_summary)==4,
            identical(weekly,original_weekly),identical(daily,original_daily))
},warning=function(w) {
  warnings_seen[[length(warnings_seen)+1L]] <<- data.frame(phase=phase,attempt=length(calls),warning=conditionMessage(w))
  # Keep normal warning output in the full log as well as this structured record.
})
fwrite(if(length(warnings_seen))rbindlist(warnings_seen)else data.frame(phase=character(),attempt=integer(),warning=character()),file.path(out,"warnings.csv"))
capture.output(sessionInfo(),file=file.path(repdir,"environment/study-session.txt"))
writeLines(c("Completed 41 study evaluations: A1=1, full-grid=12, WF IS=24, WF OOS=4.",
             paste("Study warning count:",length(warnings_seen)),"No B1/Chapter5/ML study or historical equivalence rerun."),file.path(out,"RUN_COMPLETE.txt"))
