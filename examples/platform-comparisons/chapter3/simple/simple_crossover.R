# Completed illustration of the original Chapter 3 simple comparison.
# One asset unit, unit multiplier, 20/50 SMA crossings and a fixed 2% stop.
# Input: raw, ordered OHLC bars with unique POSIXct opening timestamps.
# NextOpen is an execution reference, not an input to the crossing decision.

prepare_simple_crossover <- function(price, fast_period = 20L,
                                     slow_period = 50L, stop_loss_pct = 2) {
  required <- c("date", "Open", "High", "Low", "Close")
  stopifnot(all(required %in% names(price)),
            fast_period >= 1, slow_period > fast_period,
            nrow(price) >= slow_period, stop_loss_pct > 0, stop_loss_pct < 100)
  price <- data.table::copy(data.table::as.data.table(price)[, required, with = FALSE])
  stopifnot(inherits(price$date, "POSIXct"), !anyNA(price$date),
            !anyDuplicated(price$date), !is.unsorted(price$date),
            all(is.finite(as.matrix(price[, .(Open, High, Low, Close)]))),
            all(price$Low > 0), all(price$Low <= pmin(price$Open, price$Close)),
            all(price$High >= pmax(price$Open, price$Close)))

  price[, `:=`(FastMA = TTR::SMA(Close, n = fast_period),
               SlowMA = TTR::SMA(Close, n = slow_period),
               NextOpenTime = data.table::shift(date, type = "lead"),
               NextOpen = data.table::shift(Open, type = "lead"))]
  price[, `:=`(PrevFastMA = data.table::shift(FastMA),
               PrevSlowMA = data.table::shift(SlowMA))]
  price[, `:=`(BuySignal = (FastMA > SlowMA & PrevFastMA <= PrevSlowMA) %in% TRUE,
               ExitSignal = (FastMA < SlowMA & PrevFastMA >= PrevSlowMA) %in% TRUE)]

  price[, `:=`(OrderSize = 0, EntryType = "Market", EntryTime = NextOpenTime,
               EntryPrice = NA_real_, StopLoss = NA_real_, exit_long = 0)]
  price[BuySignal & is.finite(NextOpen),
        `:=`(OrderSize = 1, EntryPrice = NextOpen,
             StopLoss = NextOpen * (1 - stop_loss_pct / 100))]
  price[ExitSignal & is.finite(NextOpen), exit_long := 1]
  price[]
}

# Small daily-bar harness for this illustration. Bid and ask equal OHLC;
# no additional fees or slippage. This is not the thesis intraday study.
run_simple_crossover <- function(price, fast_period = 20L,
                                 slow_period = 50L, stop_loss_pct = 2) {
  prepared <- prepare_simple_crossover(price, fast_period, slow_period, stop_loss_pct)
  execution <- data.table::copy(prepared)
  for (field in c("Open", "High", "Low", "Close")) {
    execution[, (paste0(field, ".a")) := get(field)]
    execution[, (paste0(field, ".b")) := get(field)]
  }
  # Keep the default four-week limit from adding an unintended strategy exit.
  horizon_seconds <- as.numeric(max(execution$date)) - as.numeric(min(execution$date))
  horizon_seconds <- horizon_seconds + 2 * max(as.numeric(diff(execution$date)))
  TradeTesterR::backtest(execution, CloseTradeOnOppositeSignal = FALSE,
                        AddToPosition = FALSE, verbose = FALSE,
                        TradeTimeLimit = lubridate::dseconds(horizon_seconds))
}
