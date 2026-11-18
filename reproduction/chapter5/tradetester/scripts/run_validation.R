# Run from the original project root with its historical package snapshot in the chosen R library.
# Rscript reproducibility/run_validation.R /path/to/es.txt results illustration
# Rscript reproducibility/run_validation.R /path/to/es.txt results walkforward 3
library(TradeTesterR)
library(data.table)
library(xts)
library(TTR)
setDTthreads(1L)
stopifnot(as.character(getNamespaceVersion("TradeTesterR")) == "0.1.2.9001")
args <- commandArgs(TRUE)
if (length(args) < 3) stop("Supply ES input, output directory and illustration/walkforward mode.")
es_file <- normalizePath(args[1], winslash="/", mustWork=TRUE)
if (unname(tools::md5sum(es_file)) != "e648ac342c9fd918dfa6749dac5571c3")
  stop("This reproduction requires the frozen longer ES input from viva_tradetester.zip; input hash differs.")
out <- args[2]; dir.create(out,recursive=TRUE,showWarnings=FALSE)
out <- normalizePath(out,winslash="/")
mode <- match.arg(args[3],c("illustration","walkforward"))
workers <- if (length(args) >= 4) as.integer(args[4]) else 1L
phase <- if (length(args) >= 5) match.arg(args[5],c("all","train","finish")) else "all"
shard <- if (length(args) >= 6) as.integer(args[6]) else 1L
shards <- if (length(args) >= 7) as.integer(args[7]) else 1L
source("reproducibility/es_strategy.R")
df <- fread(es_file,showProgress=FALSE)
df[, date:=as.POSIXct(paste(Date,Time),format="%m/%d/%Y %H:%M",tz="America/New_York")]
stopifnot(!anyNA(df$date),!anyDuplicated(df$date),all(diff(df$date)>0))
stopifnot(all(is.finite(as.matrix(df[,.(Open,High,Low,Close)]))))
stopifnot(df[High<pmax(Open,Close,Low)|Low>pmin(Open,Close,High),.N]==0)
bar5 <- xts(df[,.(Open,High,Low,Close)],order.by=df$date)
px <- to.period(bar5,period="days",k=1,indexAt="startof",name=NULL,drop.time=FALSE)
price <- data.table(date=as.POSIXct(index(px),tz="America/New_York"),coredata(px))
setnames(price,c("date","Open","High","Low","Close"));setkey(price,date)
strategy <- make_es_strategy(bar5)
rf_path <- "reproducibility/input/riskfree-tqx-default.rds"
stopifnot(unname(tools::md5sum(rf_path))=="d103ae88f460bfa94aea44b682fd463e")
rf <- readRDS(rf_path)
audit_bt <- function(b,label) {
  r <- b$results
  z <- r[,.(entered=abs(first(OrderSize)),exited=sum(abs(ExitAmount)),remaining=last(CurrentPosSize)),by=Order_ID]
  stopifnot(all(abs(z$entered-z$exited)<1e-8),all(abs(z$remaining)<1e-8),all(is.finite(r$Returns)),
            all(r$ExitTime <= b$audit$execution_end),all(r$EntryTime <= r$ExitTime))
  data.table(run=label,trades=uniqueN(r$Order_ID),exit_rows=nrow(r),pnl_usd=sum(r$Returns),
    pnl_contract_points=sum(r$Returns_points),ambiguous_bars=sum(r$AmbiguousBar),
    invalid_orders=nrow(b$audit$invalid_orders),unplaced_events=nrow(b$audit$unplaced_events),
    missing_markers=length(b$audit$missing_markers),state_rows=b$audit$state_rows,
    conservation_violations=0L)
}
if (mode == "illustration") {
  summaries <- list()
  for (sma in c(FALSE,TRUE)) {
    name <- if (sma) "sma200" else "base"
    cat("START",name,format(Sys.time()),"\n");flush.console()
    b <- strategy(copy(price),use_sma200=sma)
    summaries[[name]] <- audit_bt(b,name)
    fwrite(b$results,file.path(out,paste0(name,"-trades.csv")),dateTimeAs="ISO")
    saveRDS(b$results,file.path(out,paste0(name,"-trades.rds")))
    fwrite(b$orders,file.path(out,paste0(name,"-orders.csv")),dateTimeAs="ISO")
    fwrite(b$audit$invalid_orders,file.path(out,paste0(name,"-excluded-orders.csv")))
    fwrite(analyze_performance(b,initial_equity=100000,risk_free=rf),file.path(out,paste0(name,"-metrics.csv")))
    print(summaries[[name]])
    rm(b);gc()
  }
  fwrite(rbindlist(summaries),file.path(out,"illustration-summary.csv"))
} else {
  dir.create(file.path(out,"cache"),showWarnings=FALSE)
  windows <- list(); start <- min(price$date)
  while(start < max(price$date)) {
    end <- start+lubridate::weeks(156)
    test_end <- end+lubridate::weeks(26)
    train <- price[date>=start & date<end]; test <- price[date>=end & date<test_end]
    if(nrow(test)<100) break
    windows[[length(windows)+1L]] <- list(train=train,test=test,end=end,test_end=test_end)
    start <- start+lubridate::weeks(26)
  }
  grids <- list(atr=data.frame(atr_n=c(10L,14L,20L),kTP=1.5),
                atr_ktp=expand.grid(atr_n=c(5L,20L),kTP=c(1,1.5,2),KEEP.OUT.ATTRS=FALSE))
  combined <- unique(rbindlist(grids))
  key <- function(p,atr_n,kTP,horizon) paste(as.numeric(first(p$date)),as.numeric(horizon),atr_n,kTP,sep="_")
  job <- function(p,atr_n,kTP,horizon) {
    p <- copy(p);attr(p,"execution_end") <- horizon
    list(price=p,atr_n=atr_n,kTP=kTP,path=file.path(out,"cache",paste0(key(p,atr_n,kTP,horizon),".rds")))
  }
  jobs <- list()
  for(w in windows) for(j in seq_len(nrow(combined)))
    jobs[[length(jobs)+1L]] <- job(w$train,combined$atr_n[j],combined$kTP[j],w$end)
  run_job <- function(j) {
    b <- strategy(j$price,atr_n=j$atr_n,kTP=j$kTP)
    audit_bt(b,basename(j$path))
    b$data <- NULL
    saveRDS(b,j$path)
    cat("COMPLETED",basename(j$path),"\n");flush.console()
    j$path
  }
  cl <- if(workers>1) parallel::makePSOCKcluster(workers,outfile=file.path(out,"workers.log"),rscript_args="--vanilla") else NULL
  if(!is.null(cl)) {
    parallel::clusterEvalQ(cl,{library(TradeTesterR);library(data.table);library(xts);library(TTR);setDTthreads(1L);NULL})
    parallel::clusterExport(cl,c("strategy","audit_bt","run_job"),envir=.GlobalEnv)
  }
  run_jobs <- function(js) {
    if(!length(js)) return(invisible(NULL))
    # Do not reuse files from other runs; all calls are recomputed for this validation.
    if(is.null(cl)) lapply(js,run_job) else parallel::parLapplyLB(cl,js,run_job,chunk.size=1L)
    invisible(NULL)
  }
  cat("TRAINING_CALLS",length(jobs),"WORKERS",workers,"\n");flush.console()
  tryCatch({
    if (phase != "finish") run_jobs(jobs[(seq_along(jobs)-1L) %% shards == shard-1L])
    if (phase != "train") for(name in names(grids)) {
      grid <- grids[[name]];test_jobs <- list()
      for(w in windows) {
        profits <- vapply(seq_len(nrow(grid)),function(j) {
          b <- readRDS(job(w$train,grid$atr_n[j],grid$kTP[j],w$end)$path)
          sum(b$results$Returns)
        },numeric(1))
        best <- grid[which.max(profits),]
        test_jobs[[length(test_jobs)+1L]] <- job(w$test,best$atr_n,best$kTP,w$test_end)
      }
      run_jobs(test_jobs)
      cached_strategy <- function(price,atr_n,kTP) {
        readRDS(job(price,atr_n,kTP,attr(price,"execution_end"))$path)
      }
      # Replay the actual package optimiser over the independently calculated
      # ledgers, and verify that its selected test paths exist in this grid.
      o <- optimizeStrategy(price,grid,initial_equity=100000,train_set_weeks=156,test_set_weeks=26,
                            strategy=cached_strategy,risk_free=rf,verbose=FALSE)
      saveRDS(o,file.path(out,paste0(name,"-optimizer.rds")))
      history <- rbindlist(o$optimization_history,idcol="window")
      fwrite(history,file.path(out,paste0(name,"-history.csv")))
      fwrite(history[,.SD[which.max(net_profit)],by=window],file.path(out,paste0(name,"-selected.csv")))
      ledger <- rbindlist(lapply(o$test_results,`[[`,"results"),idcol="window",fill=TRUE)
      fwrite(ledger,file.path(out,paste0(name,"-oos-trades.csv")),dateTimeAs="ISO")
      d <- compareISOS(o);st <- analyzeParameterStability(o)
      fwrite(d$performance_comparison,file.path(out,paste0(name,"-comparison.csv")))
      saveRDS(d,file.path(out,paste0(name,"-compareISOS.rds")))
      saveRDS(st,file.path(out,paste0(name,"-stability.rds")))
      for(p in c(d$plots,st$plots)) plotly::plotly_build(p)
      if(name=="atr_ktp") {
        h <- o$optimization_history[[1]]
        plots <- plotMultiParameter(h,h$net_profit,"atr_n","kTP")
        for(p in plots) plotly::plotly_build(p)
      }
      fwrite(data.table(grid=name,windows=length(o$test_results),trades=nrow(ledger),pnl_usd=sum(ledger$Returns),
        ambiguous_bars=sum(ledger$AmbiguousBar),plot_builds_passed=TRUE),file.path(out,paste0(name,"-summary.csv")))
      cat("DONE_GRID",name,"WINDOWS",length(o$test_results),"PNL_USD",sum(ledger$Returns),"\n");flush.console()
    }
  },finally={if(!is.null(cl))parallel::stopCluster(cl)})
}
capture.output(sessionInfo(),file=file.path(out,paste0(mode,"-session.txt")))
cat("VALIDATION_COMPLETED",mode,"\n")
