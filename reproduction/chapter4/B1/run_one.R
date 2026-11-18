# Execute one Word-derived core in a fresh R process.
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
id <- args[2]
options(warn = 1)
library(PortfolioTesteR)
library(data.table)
stopifnot(id %in% readLines(file.path(root, "scripts/batch-ids.txt")),
 as.character(packageVersion("PortfolioTesteR")) == Sys.getenv("PT_EXPECT_VERSION"),
 normalizePath(find.package("PortfolioTesteR"), winslash = "/") == normalizePath(Sys.getenv("PT_EXPECT_PATH"), winslash = "/"))
out <- file.path(root, "local-outputs", id)
if (dir.exists(out)) stop("Preserve earlier attempts: output directory already exists.")
dir.create(out)
capture.output(sessionInfo(), file = file.path(out, "session-info.txt"))
capture.output(.libPaths(), file = file.path(out, "library-paths.txt"))
started <- format(Sys.time(), tz = "UTC", usetz = TRUE)
phase <- "setup"; warning_rows <- list(); error_text <- ""; checks <- list()
check <- function(name, ok, evidence) {
  checks[[length(checks)+1L]] <<- data.table(example_id = id, check = name, passed = isTRUE(ok), evidence = paste(evidence, collapse = "; "))
}
e <- new.env(parent = globalenv())
source(file.path(root, "scripts/shared_setup.R"), local = e)
phase <- "source_core"
core_ok <- tryCatch(withCallingHandlers({
  source(file.path(root, "scripts", paste0(id, ".R")), local = e, encoding = "UTF-8")
  TRUE
}, warning = function(w) {
  warning_rows[[length(warning_rows)+1L]] <<- data.table(phase = phase, text = conditionMessage(w), call = paste(deparse(conditionCall(w)), collapse = " "))
}), error = function(err) {
  error_text <<- conditionMessage(err)
  cat("SOURCE ERROR:", error_text, "\n", file = stderr())
  capture.output(sys.calls(), file = file.path(out, "error-call-stack.txt"))
  FALSE
})
saveRDS(as.list(e, all.names = TRUE), file.path(out, "internal-source-objects.rds"))
check("original_core_completed", core_ok, if (core_ok) "All non-plot expressions completed." else error_text)

result_name <- switch(id, "C4-45-BM4" = "bench", "C4-45-02" = "result_rsi",
 "C4-45-03" = "result_sector", "C4-45-04" = "result_vol", "C4-45-05" = "result_regime", "result")
summaries <- list()
inspect_result <- function(bt, result_id) {
  stopifnot(inherits(bt, "backtest_result"))
  syms <- bt$symbols
  W <- as.matrix(bt$weights[, ..syms]); Px <- as.matrix(bt$prices[, ..syms])
  pos <- as.matrix(bt$positions[, ..syms])
  cash <- as.numeric(bt$cash); pv <- as.numeric(bt$portfolio_values)
  v_rebuilt <- rowSums(pos * Px, na.rm = TRUE) + cash
  max_ledger_gap <- max(abs(v_rebuilt - pv))
  check(paste0(result_id, ": dates and lengths"), inherits(bt$dates, "Date") &&
    identical(bt$dates, bt$prices$Date) && identical(bt$dates, bt$weights$Date) &&
    length(pv) == nrow(W) && !anyDuplicated(bt$dates), paste(length(pv), "rows"))
  check(paste0(result_id, ": finite positive equity"), length(pv) > 1 && all(is.finite(pv)) && all(pv > 0), range(pv))
  check(paste0(result_id, ": finite long-only target weights"), all(is.finite(W)) && all(W >= -1e-10) && all(rowSums(W) <= 1+1e-8), range(rowSums(W)))
  check(paste0(result_id, ": cash nonnegative within tolerance"), all(is.finite(cash)) && all(cash >= -0.01), min(cash))
  check(paste0(result_id, ": positions plus cash reconcile"), max_ledger_gap < 0.01, max_ledger_gap)
  check(paste0(result_id, ": return-series consistency"), max(abs(bt$returns - c(0, diff(pv)/head(pv,-1)))) < 1e-12, "Compared stored returns with saved equity changes.")
  check(paste0(result_id, ": final/full-return consistency"),
    abs(bt$final_value-tail(pv,1)) < 1e-8 && abs(bt$total_return_with_warmup-(tail(pv,1)/bt$initial_capital-1)) < 1e-12,
    paste(bt$final_value, bt$total_return_with_warmup))
  check(paste0(result_id, ": nonempty trading outcome"), bt$n_transactions > 0 && any(pos > 0), bt$n_transactions)
  data <- data.table(Date = bt$dates, portfolio_value = pv, period_return = bt$returns,
    drawdown_pct = (pv/cummax(pv)-1)*100, target_exposure = rowSums(W),
    target_positions = rowSums(W > 0), holdings = rowSums(pos > 0), cash = cash,
    cash_fraction = cash/pv)
  fwrite(data, file.path(out, paste0(result_id, "-figure-data.csv")))
  saveRDS(bt, file.path(out, paste0(result_id, "-backtest.rds")))
  fwrite(bt$weights, file.path(out, paste0(result_id, "-target-weights.csv")))
  capture.output(print(bt), summary(bt), file = file.path(out, paste0(result_id, "-package-summary.txt")))
  summary <- data.table(example_id = result_id, original_result_name = bt$name,
    rows = bt$n_periods, start_date = bt$start_date, end_date = bt$end_date,
    first_trade_date = bt$first_trade_date, active_periods = bt$active_periods,
    warmup_periods = bt$warmup_periods, initial_capital = bt$initial_capital,
    final_value = bt$final_value, total_return_active = bt$total_return,
    total_return_full = bt$total_return_with_warmup, max_drawdown_pct = min(data$drawdown_pct),
    transactions = bt$n_transactions, min_target_exposure = min(data$target_exposure),
    max_target_exposure = max(data$target_exposure), max_holdings = max(data$holdings),
    max_ledger_gap = max_ledger_gap, gross_or_net = "gross",
    annual_metrics_note = "Version 0.1.4.9004: elapsed-date annual return and purchase turnover; reported sampling-rate risk scaling.")
  summaries[[length(summaries)+1L]] <<- summary
  invisible(data)
}
validation_error <- ""
if (exists(result_name, e, inherits = FALSE)) {
  phase <- "validation"
  tryCatch(withCallingHandlers({
    bt <- e[[result_name]]
    primary <- inspect_result(bt, id)
    if (id == "C4-45-10") inspect_result(e$bench, "C4-45-BMW")
    cap <- switch(id, "C4-45-01"=5, "C4-45-04"=8, "C4-45-05"=5,
      "C4-45-07"=10, "C4-45-08"=8, "C4-45-09"=10, "C4-45-10"=8, Inf)
    check("target-position cap", max(primary$target_positions) <= cap, paste("observed",max(primary$target_positions),"cap",cap))
    if (id == "C4-45-05") {
      r <- data.table(Date = bt$dates, bullish = e$bullish_regime, target_exposure = primary$target_exposure,
         holdings = primary$holdings, cash_fraction = primary$cash_fraction)
      fwrite(r, file.path(out, "regime-exposure.csv"))
      check("false regime targets zero", all(r$target_exposure[!r$bullish] == 0), sum(!r$bullish))
      check("false regime actually in cash", all(abs(r$cash_fraction[!r$bullish]-1) < 1e-8), range(r$cash_fraction[!r$bullish]))
    }
    if (id == "C4-45-08") {
      syms <- bt$symbols
      actual <- as.matrix(e$final_weights[, ..syms]); a <- as.matrix(e$weights_signal[, ..syms]); b <- as.matrix(e$weights_equal[, ..syms])
      expected <- a; expected[e$use_equal,] <- b[e$use_equal,]
      check("switch chooses the printed weighting branch", max(abs(actual-expected)) < 1e-10, max(abs(actual-expected)))
      fwrite(data.table(Date = bt$dates, market_vol = e$market_vol, full_vector_median = e$vol_threshold,
        use_equal = e$use_equal, target_exposure = primary$target_exposure), file.path(out, "adaptive-weight-regime.csv"))
      cat("Switch rows: equal",sum(e$use_equal),"rank",sum(!e$use_equal),"\n")
    }
    if (id == "C4-45-03") {
      syms <- bt$symbols
      sc <- e$sample_sp500_sectors
      sector <- sc$Sector[match(syms,sc$Symbol)]; sector[is.na(sector)] <- "UNMAPPED"
      totals <- data.table(Date = bt$dates)
      for (g in unique(sector)) totals[[g]] <- rowSums(as.matrix(bt$weights[, ..syms])[,sector==g,drop=FALSE])
      fwrite(totals, file.path(out, "sector-exposure.csv"))
    }
    if (id == "C4-45-01") {
      # The source debug ladder uses these same numerical intermediates.
      for (stage in c("momentum","selected","weights")) {
        x <- e[[stage]]; z <- x[x$Date %in% e$monthly_prices$Date[c(5,20)],1:7]
        fwrite(z,file.path(out,paste0("C4-46-DEBUG-",stage,".csv")))
      }
      check("debug slice dates", identical(e$monthly_prices$Date[c(5,20)],bt$dates[c(5,20)]), as.character(bt$dates[c(5,20)]))
    }
    if (id == "C4-45-09" && exists("perf",e,inherits=FALSE)) {
      perf <- e$perf; saveRDS(perf,file.path(out,"C4-45-09-daily-analysis.rds"))
      d <- perf$daily; v <- d$values; r <- d$returns
      vol <- rep(NA_real_,length(r)); if(length(r)>=30)for(i in 30:length(r))vol[i]<-sd(r[(i-29):i])*sqrt(252)*100
      daily <- data.table(Date=d$dates,portfolio_value=v,period_return=r,drawdown_pct=(v/cummax(v)-1)*100,
        rolling_vol_30d_pct=vol,cumulative_return_pct=(cumprod(1+r)-1)*100)
      fwrite(daily,file.path(out,"C4-45-09-daily-figure-data.csv"))
      check("daily analysis finite output", length(v)>30 && all(is.finite(v)) && all(v>0), length(v))
      common <- match(bt$dates,d$dates); keep <- !is.na(common)
      gap <- max(abs(bt$portfolio_values[keep]-v[common[keep]]))
      check("daily equity agrees on common decision dates", gap<0.01, gap)
      capture.output(print(perf),file=file.path(out,"C4-45-09-daily-summary.txt"))
      # Preserve values drawn by the original package overlay for comparison.
      if (!is.null(perf$benchmark)) {
        original_curve <- (cumprod(1+perf$benchmark$excess_returns+r)-1)*100
        benchmark_curve <- (cumprod(1+r-perf$benchmark$excess_returns)-1)*100
        fwrite(data.table(Date=d$dates,original_plot_benchmark_pct=original_curve,
          implied_benchmark_from_excess_pct=benchmark_curve),file.path(out,"original-benchmark-overlay-review.csv"))
      }
    }
  }, warning=function(w) {
    warning_rows[[length(warning_rows)+1L]] <<- data.table(phase=phase,text=conditionMessage(w),call=paste(deparse(conditionCall(w)),collapse=" "))
  }), error=function(err) {
    validation_error <<- conditionMessage(err)
    check("validation completed",FALSE,validation_error)
    cat("VALIDATION ERROR:",validation_error,"\n",file=stderr())
  })
}
checks_dt <- rbindlist(checks)
fwrite(checks_dt,file.path(out,"checks.csv"))
if(length(summaries))fwrite(rbindlist(summaries),file.path(out,"summary.csv"))
warn_dt <- if(length(warning_rows)) rbindlist(warning_rows) else data.table(phase=character(),text=character(),call=character())
fwrite(warn_dt,file.path(out,"warnings.csv"))
status <- if(core_ok && all(checks_dt$passed) && !nzchar(validation_error)) "PASS" else "FAIL"
record <- data.table(example_id=id,status=status,started_utc=started,ended_utc=format(Sys.time(),tz="UTC",usetz=TRUE),
 requested_backend="none: rule-based",actual_backend="none: rule-based",seed="not applicable: deterministic rules",
 core_completed=core_ok,checks_passed=sum(checks_dt$passed),checks_failed=sum(!checks_dt$passed),
 warnings=nrow(warn_dt),source_error=error_text,validation_error=validation_error,
 script=file.path(root,"scripts",paste0(id,".R")),output_dir=out,
 package_path=find.package("PortfolioTesteR"),package_version=as.character(packageVersion("PortfolioTesteR")))
fwrite(record,file.path(out,"run-record.csv"))
print(record[,.(example_id,status,checks_passed,checks_failed,warnings)])
if(status!="PASS")quit(status=1L)
