# Moving-average crossover on the bundled execution data.
# Start in a fresh R session using the library selected in INSTALL.txt.

library(TradeTesterR)
library(data.table)
library(xts)
library(TTR)

# Parameters --------------------------------------------------------------
trade_size_contracts <- 4
futures_multiplier   <- 50
spread               <- 0.04  # Synthetic ask-minus-bid spread, in price units
initial_equity       <- 10000 # Reporting and plotting capital, not a sizing rule

# Data ---------------------------------------------------------------------
df <- fread(system.file("extdata", "crude_wti_futures.csv",
                        package = "TradeTesterR"))
setnames(df, c("date", "Open", "High", "Low", "Close", "Volume"))
price_small <- df[, .(date, Open, High, Low, Close)]
setkey(price_small, date)

price_xts <- to.period(xts(price_small[, .(Open, High, Low, Close)],
                           order.by = price_small$date),
                       period = "days", k = 1, indexAt = "startof",
                       name = NULL, drop.time = FALSE)
price <- data.table(date = index(price_xts), coredata(price_xts))
setkey(price, date)

# Strategy -----------------------------------------------------------------
my_strat <- function(price, fast_period = 20, slow_period = 50, stop_loss_pct = 2) {

  # Identify the next observed daily opening
  price[, NextOpenTime := shift(date, n = 1, type = "lead")]
  price[, NextOpen     := shift(Open, n = 1, type = "lead")]

  # Calculate the fast and slow moving averages
  price[, FastMA := SMA(Close, n = fast_period)]
  price[, SlowMA := SMA(Close, n = slow_period)]

  # Compare current and previous moving averages
  price[, PrevFastMA := shift(FastMA, n = 1, type = "lag")]
  price[, PrevSlowMA := shift(SlowMA, n = 1, type = "lag")]
  price[, BuySignal := FastMA > SlowMA & PrevFastMA <= PrevSlowMA]

  # Enter at the next observed opening; set the stop from that opening
  price[BuySignal == TRUE, EntryPrice := NextOpen]
  price[BuySignal == TRUE, EntryType := "Market"]
  price[BuySignal == TRUE, StopLoss := NextOpen * (1 - stop_loss_pct / 100)]

  # Exit: the opposite cross closes the position
  price[FastMA < SlowMA & PrevFastMA >= PrevSlowMA, exit_long := 1]

  # Position size includes the chosen multiplier
  price[BuySignal == TRUE, OrderSize := futures_multiplier * trade_size_contracts]

  # Entry time: orders placed at a bar's close become active at the next open
  setorder(price, date)
  price[, EntryTime := shift(date, 1, type = "lead")]

  # Execution feed: supplied 15-minute bars with gaps and a synthetic bid
  ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = df$date)
  bid <- ask - spread
  dat <- merge(mergeAskBid(ask, bid), price[], by = "date", all.x = TRUE)

  backtest(dat,
           exit_fun = exit_TP_SL_signal,
           AddToPosition = FALSE,
           CloseTradeOnOppositeSignal = FALSE,
           verbose = FALSE,
           TradeTimeLimit = lubridate::weeks(100000))
}

# Run and inspect ----------------------------------------------------------
bt  <- my_strat(price, fast_period = 20, slow_period = 50, stop_loss_pct = 2)
res <- bt$results

head(bt$orders)
head(res[, .(Order_ID, ExitTime, ExitReason, ExitAmount, CurrentPosSize, Returns)])
res[, .(completed_orders = uniqueN(Order_ID[CurrentPosSize == 0]),
        exit_legs = .N, realised_pnl = sum(Returns))]
table(res$ExitReason)
analyze_performance(bt, initial_equity = initial_equity)
plotEquityCurve(bt, initial_equity = initial_equity)
