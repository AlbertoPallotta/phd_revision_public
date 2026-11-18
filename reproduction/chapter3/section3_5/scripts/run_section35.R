# Historical Section 3.5 study; preserve its original strategy rules.
# Rscript run_section35.R <new-output-folder> <historical-TT-source> <historical-R-library>
# Requires the 14 October executable identified in manifests/historical-access.csv.
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=3L) stop("Missing configuration: supply output folder, historical TT source and R library.",call.=FALSE)
out <- normalizePath(args[1],winslash="/",mustWork=FALSE)
source_root <- normalizePath(args[2],winslash="/",mustWork=TRUE)
historical_library <- normalizePath(args[3],winslash="/",mustWork=TRUE)
if (file.exists(out)) stop("Output exists; choose a new folder. No evidence will be overwritten.")
if (!file.exists(file.path(source_root,"DESCRIPTION")) || !dir.exists(file.path(historical_library,"TradeTesterR"))) stop("Missing historical TT source/library.")
.libPaths(c(historical_library,.libPaths()))
dir.create(out,recursive=TRUE,showWarnings=FALSE)
Sys.setenv(TZ = "UTC")
suppressPackageStartupMessages({
  library(TradeTesterR)
  library(data.table)
  library(xts)
  library(TTR)
})
expected_library <- normalizePath(file.path(historical_library,"TradeTesterR"),winslash="/")
stopifnot(normalizePath(find.package("TradeTesterR"), winslash = "/") == expected_library)
# Compare the loaded implementation with the source used by the chapter.
for (pair in list(c("backtest", "backtest.R"), c("exit_TP_SL_signal", "exit_strategies.R"),
                  c("mergeAskBid", "data_utils.R"), c("analyze_performance", "performance_analysis.R"))) {
  expressions <- parse(file.path(source_root, "R", pair[2]), keep.source = FALSE)
  found <- Filter(function(e) is.call(e) && as.character(e[[1]]) %in% c("<-", "=") &&
                    is.symbol(e[[2]]) && as.character(e[[2]]) == pair[1], as.list(expressions))
  stopifnot(length(found) == 1L)
  candidate_fun <- eval(found[[1]][[3]], envir = asNamespace("TradeTesterR"))
  loaded_fun <- get(pair[1], envir = asNamespace("TradeTesterR"))
  stopifnot(identical(formals(candidate_fun), formals(loaded_fun)),
            identical(paste(deparse(body(candidate_fun)), collapse = "\n"),
                      paste(deparse(body(loaded_fun)), collapse = "\n")))
}
input <- file.path(source_root, "inst/extdata/crude_wti_futures.csv")
df <- fread(input)
setnames(df, c("date", "Open", "High", "Low", "Close", "Volume"))
stopifnot(inherits(df$date, "POSIXct"), !anyNA(df$date), !anyDuplicated(df$date))
setorder(df, date)
price_small <- df[, .(date, Open, High, Low, Close)]
ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = price_small$date)
bid <- ask - 0.04
execution <- mergeAskBid(ask, bid)
daily_xts <- to.period(ask, period = "days", k = 1, indexAt = "startof",
                       name = NULL, drop.time = FALSE)
daily <- data.table(date = index(daily_xts), coredata(daily_xts))
setkey(daily, date)
stopifnot(all(daily$date %in% execution$date))
fwrite(data.table(input_rows = nrow(df), daily_rows = nrow(daily),
                  start = min(df$date), end = max(df$date),
                  median_minutes = median(as.numeric(diff(df$date), units = "mins")),
                  duplicate_times = anyDuplicated(df$date), timezone = "UTC",
                  spread = 0.04, encoded_base_size = 200), file.path(out, "input-summary.csv"))
gaps <- data.table(seconds = as.numeric(diff(df$date), units = "secs"))
fwrite(gaps[, .N, by = seconds][order(seconds)], file.path(out, "execution-spacing.csv"))
fwrite(daily[, .(date, Open, High, Low, Close)], file.path(out, "daily-ohlc.csv"))
saveRDS(daily, file.path(out, "daily-ohlc.rds"))
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
writeLines(capture.output(.libPaths()), file.path(out, "library-paths.txt"))
writeLines(capture.output(TTR::BBands, TTR::EMA, TTR::ATR, TTR::RSI),
           file.path(out, "indicator-implementations.txt"))

prepare <- function(id) {
  price <- copy(daily)
  price[, `:=`(NextOpenTime = shift(date, type = "lead"), NextOpen = shift(Open, type = "lead"),
               PrevClose = shift(Close), PrevLow = shift(Low), PrevHigh = shift(High))]
  width <- if (id == "X07") 1.5 else 2
  boll <- BBands(price[, .(High, Low, Close)], n = 20, sd = width)
  price[, `:=`(upperBB = boll[, "up"], lowerBB = boll[, "dn"], midBB = boll[, "mavg"])]
  if (id %in% c("X01", "X02", "X08")) {
    price[, BuySignal := Close <= lowerBB & PrevClose > lowerBB]
    price[, SellSignal := if (id == "X01") FALSE else Close >= upperBB & PrevClose < upperBB]
  }
  if (id == "X03") {
    price[, ema20 := EMA(Close, n = 20)]
    price[, `:=`(BuySignal = Close > ema20, SellSignal = Close < ema20)]
  }
  if (id == "X07") {
    price[, ATR := ATR(price[, .(High, Low, Close)], n = 14)[, "atr"]]
    price[, `:=`(BuySignal = TRUE, SellSignal = FALSE)]
  }
  if (id == "X08") {
    price[, RSI := RSI(Close, n = 14, maType = "SMA")]
    price[, ATR := ATR(price[, .(High, Low, Close)], n = 14)[, "atr"]]
    price[, ATR_Weekly := frollmean(ATR, n = 5, align = "right")]
    price[, `:=`(BuySignal = BuySignal & RSI <= 30, SellSignal = SellSignal & RSI >= 70)]
  }
  price[BuySignal %in% TRUE, OrderSize := 200]
  price[SellSignal %in% TRUE, OrderSize := -200]
  if (id %in% c("X01", "X02", "X08")) {
    price[BuySignal %in% TRUE | SellSignal %in% TRUE,
          `:=`(EntryPrice = NextOpen, EntryType = "Market")]
  }
  if (id == "X03") {
    price[BuySignal %in% TRUE, `:=`(EntryPrice = PrevLow, EntryType = "Limit",
                                   StopLoss = PrevLow - 2000/200)]
    price[SellSignal %in% TRUE, `:=`(EntryPrice = PrevHigh, EntryType = "Limit")]
  }
  if (id %in% c("X01", "X02", "X03")) {
    price[Close > upperBB, exit_long := 1]
    if (id != "X01") price[Close < lowerBB, exit_short := 1]
  }
  if (id == "X07") {
    price[, `:=`(EntryPrice = lowerBB, EntryType = "Limit")]
    price[, StopLoss := EntryPrice - 2000/200]
    price[, `:=`(targetorder_long_1 = midBB, targetorder_long_1_amount = 0.5,
                 targetorder_long_2 = upperBB, targetorder_long_2_amount = 1,
                 stoporder_long_1 = PrevLow - 0.4*ATR, stoporder_long_1_amount = 1)]
  }
  if (id == "X08") {
    price[(BuySignal %in% TRUE | SellSignal %in% TRUE) &
            ATR > 1.2*ATR_Weekly, OrderSize := OrderSize*0.5]
    price[BuySignal %in% TRUE | SellSignal %in% TRUE,
          VolAdjusted := fifelse(ATR > 1.2*ATR_Weekly, "Reduced Size", "Full Size")]
  }
  price[, EntryTime := shift(date, type = "lead")]
  price
}
summaries <- list()
for (id in c("X01", "X02", "X03", "X07")) {
  cat("Starting", id, "\n")
  price <- prepare(id)
  dat <- merge(copy(execution), price, by = "date", all.x = TRUE)
  saveRDS(price, file.path(out, paste0(id, "-daily.rds")))
  fwrite(price, file.path(out, paste0(id, "-daily.csv")))
  warnings <- character()
  bt <- withCallingHandlers(
    backtest(dat, exit_fun = exit_TP_SL_signal, AddToPosition = FALSE,
             CloseTradeOnOppositeSignal = FALSE, verbose = FALSE,
             TradeTimeLimit = lubridate::weeks(100000)),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  saveRDS(bt, file.path(out, paste0(id, "-backtest.rds")), compress = "xz")
  fwrite(bt$orders, file.path(out, paste0(id, "-orders.csv")))
  fwrite(bt$results, file.path(out, paste0(id, "-trades.csv")))
  writeLines(warnings, file.path(out, paste0(id, "-warnings.txt")))
  r <- bt$results
  stopifnot(nrow(r) > 0, all(is.finite(r$Returns)), all(r$ExitTime >= r$EntryTime))
  conservation <- r[, .(initial = first(OrderSize), closed = sum(ExitAmount),
                        final = last(CurrentPosSize), legs = .N), by = Order_ID]
  stopifnot(all(abs(conservation$initial + conservation$closed) < 1e-7),
            all(abs(conservation$final) < 1e-7))
  fwrite(conservation, file.path(out, paste0(id, "-quantity-check.csv")))
  orders <- bt$orders
  filled <- orders[Disposition == "filled"]
  firstfills <- r[, .(actual_entry = first(EntryTime)), by = Order_ID]
  checks <- merge(firstfills, orders, by = "Order_ID")
  stopifnot(all(checks$actual_entry >= checks$ActivationTime),
            all(checks$actual_entry < checks$EntryValidUntil))
  summaries[[id]] <- data.table(example = id, trades = uniqueN(r$Order_ID),
    exit_legs = nrow(r), instructions = nrow(orders), pnl_recipe_units = sum(r$Returns),
    first_entry = min(r$EntryTime), last_exit = max(r$ExitTime),
    ambiguity_flags = sum(r$AmbiguousBar %in% TRUE | r$AmbiguousEntry %in% TRUE),
    warnings = length(warnings))
  cat("Completed", id, "trades", uniqueN(r$Order_ID), "P&L", sum(r$Returns), "\n")
}
fwrite(rbindlist(summaries), file.path(out, "new-run-summary.csv"))
# X08 is only a preparation/size audit, not an additional trading-performance run.
scaling <- prepare("X08")
signals <- scaling[BuySignal %in% TRUE | SellSignal %in% TRUE]
stopifnot(all(abs(signals$OrderSize) %in% c(100,200)))
fwrite(signals, file.path(out, "X08-sizing-signals.csv"))
fwrite(signals[, .N, by = .(absolute_size = abs(OrderSize), VolAdjusted)],
       file.path(out, "X08-sizing-summary.csv"))
writeLines("Four contracts completed; see saved ledgers and checks. X08 preparation only.",
           file.path(out, "RUN_COMPLETE.txt"))
