# Delivery support. Arguments: persistent-library, NEW log directory, topic.
# topic is README or one of the nine topics in TT_DOCUMENTATION_DESTINATIONS.csv.
a <- commandArgs(TRUE)
stopifnot(length(a) == 3L, dir.exists(a[1]), !dir.exists(a[2]))
.libPaths(c(normalizePath(a[1], winslash = "/"), .libPaths()))
dir.create(a[2], recursive = TRUE)
sink(file.path(a[2], "check.log"), split = TRUE)
# A failure stops this fresh process before the final PASS file can be written.
print(R.version); print(.libPaths()); print(sessionInfo())
stopifnot(requireNamespace("TTR", quietly = TRUE),
          requireNamespace("plotly", quietly = TRUE))
suppressPackageStartupMessages(library(TradeTesterR))
stopifnot(normalizePath(find.package("TradeTesterR"), winslash = "/") ==
          normalizePath(file.path(a[1], "TradeTesterR"), winslash = "/"))
print(find.package("TradeTesterR"))
topics <- c("backtest", "exit_TP_SL_signal", "analyze_performance",
            "plotCumReturns", "plotEquityCurve", "plotPerformanceTable",
            "plotReturnsDistribution", "plotSingleTrade", "plotTrades")
stopifnot(a[3] %in% c("README", topics))
demo <- new.env(parent = globalenv())
sys.source(system.file("examples", "csv_demo_setup.R", package = "TradeTesterR",
                       mustWork = TRUE), envir = demo)
ex <- demo$ex
bt <- backtest(ex$dat, exit_fun = exit_TP_SL_signal,
               CloseTradeOnOppositeSignal = FALSE, AddToPosition = FALSE,
               verbose = FALSE)
r <- as.data.frame(bt$results)
stopifnot(nrow(r) == 6L, length(unique(r$Order_ID)) == 3L,
          all(is.finite(r$Returns)), all(is.finite(r$EntryPrice)),
          all(is.finite(r$ExitPrice)))
for (id in unique(r$Order_ID)) {
  z <- r[r$Order_ID == id, , drop = FALSE]
  stopifnot(nrow(z) == 2L, abs(sum(abs(z$ExitAmount)) - 2) < 1e-12,
            tail(z$CurrentPosSize, 1) == 0)
}
print(analyze_performance(bt, initial_equity = 10000))
plots <- list(plotEquityCurve(bt, initial_equity = 10000),
              plotTrades(bt, ex$price, trades = 1:3,
                         indicators = c("upperBB", "lowerBB", "midBB")),
              plotSingleTrade(bt, ex$price, trade_num = 1, window_days = 10,
                              indicators = c("upperBB", "lowerBB")))
invisible(lapply(plots, plotly::plotly_build))
if (a[3] != "README") {
  utils::example(a[3], package = "TradeTesterR", character.only = TRUE,
                 local = new.env(parent = globalenv()), ask = FALSE,
                 echo = TRUE, run.dontrun = FALSE, run.donttest = TRUE)
}
print(sessionInfo())
sink()
writeLines(paste("PASS", a[3], "new bounded documentation execution"),
           file.path(a[2], "STATUS.txt"))
