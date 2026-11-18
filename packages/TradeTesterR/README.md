# TradeTesterR

<p align="center"><img src="man/figures/logo.png" alt="TradeTesterR logo" width="600"></p>

## Explore Trading Ideas with Readable R Code

TradeTesterR is an open-source R package for building and testing trading
strategies. Write your signals, orders and exits in familiar R tables, run a
backtest, and inspect the orders and trades it produces.

It is designed for students, individual traders and researchers who want to
work through a strategy from price data to results, with the intermediate steps
available to explore.

## Why TradeTesterR?

- 🎓 **See the steps:** work with tables of prices, signals, orders and exits.
- 🧩 **Build up your strategy:** begin with a moving-average crossover, then explore
  partial exits, dynamic stops and parameter-search examples.
- 🔎 **Inspect the results:** follow entry instructions in the order audit and
  examine individual exit legs in the trade ledger.
- 💻 **Free and open source:** use and adapt the R code under the GPL-3 licence.

## What Can You Build?

### 🏗️ Separate Signal and Execution Timeframes

Calculate signals on one timeframe and evaluate orders on finer price bars—for
example, daily rules with intraday execution data. You choose the clocks and
the instruction times; the guide explains their alignment.

### 📈 Market, Limit and Stop Orders

Express entry instructions in a strategy table and inspect filled, unfilled,
cancelled or ignored instructions. Supply ask/bid bars and state the quote
assumptions used by your example.

### 🎯 Flexible Exit Rules

Use fixed stop-loss and take-profit levels, signal exits, time limits and partial
position reductions. Numbered target and stop fields support caller-defined
dynamic levels and multiple exit conditions.

### 📊 Analyse and Compare

Inspect trade statistics, realised-equity curves and drawdowns. Explore parameter
grids, walk-forward evaluation and parameter-stability diagnostics.

The [scope table](#scope) gives the supported methods and their practical limits.

## Installation

Use the [installation instructions](../../INSTALL.txt) to install the version
supplied with this repository and its dependencies. The example below also uses
TTR and runs on the data included with the package. Start in a fresh R session.

## First example: moving-average crossover

This example buys when the 20-period moving average crosses above the 50-period
moving average, and exits on the opposite crossover or a 2% stop. It uses daily
strategy bars constructed from the bundled 15-minute execution data, which contain
gaps. A position still open at the end of the supplied data is closed by the engine.

The complete code below is also in [ma_crossover.R](inst/examples/ma_crossover.R).
Change the periods and stop in the
`my_strat()` call, and the size and spread in the parameter block.

```r
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
```

### Reading the result

The example sets `OrderSize` to 4 × 50 = 200 price-unit exposure. These are
recipe settings, not a verified exchange contract specification. The engine
already includes this quantity in P&L, so do not multiply the result by 50 again.
The 10,000 initial capital is used for reporting and plotting; it does not set
the order size.

Entries are scheduled for the next observed daily opening. The fixed stop is
2% below that scheduled `NextOpen`; it is not reset from a different eventual
fill price. Here `NextOpen` comes from the ask series and is also used for
scheduled signal exits. Consequently, subtracting 0.04 to create a bid series
does not charge a full spread on every transaction. Protective stops follow the
[execution rules](GUIDE.md#entry-exit-and-window-policies). There is no additional
commission, funding or general slippage charge.

Explore `bt$orders` to see what happened to entry instructions and `bt$results`
to inspect exits. A partial exit creates a ledger row without completing the
whole trade; group rows by `Order_ID` when following a position. The summary
above counts completed orders and exit legs separately. Equity and drawdown
reflect realised exit P&L, so they do not show all unrealised gains or losses
while a position is open.

**Plotting note:** `plotTrades()` repeats the last indicator series when several
indicators are requested. Use the equity plot above, or the supplementary
`plotSingleTrade()` band example in the [guide](GUIDE.md#reading-the-plots).

## From prices to a strategy

The preparation above keeps the data and instruction tables visible:

1. `df` contains the bundled price history; `price_small` holds its dated OHLC
   execution observations. For other inputs, choose the source, timezone and
   quantity units explicitly. These are 15-minute bars with gaps, not a tick feed.
2. `price` aggregates those observations to daily strategy bars. `FastMA`,
   `SlowMA`, `PrevFastMA` and `PrevSlowMA` define the crossover conditions.
3. `EntryPrice`, `EntryType`, `EntryTime`, `OrderSize`, `StopLoss` and `exit_long`
   express the orders and exits. The example disables opposite-entry-signal
   closure explicitly (the package default is `TRUE`); the reverse crossover
   closes through `exit_long`.
4. `mergeAskBid()` joins matching ask and bid timestamps. The subsequent merge
   attaches daily instructions to the execution bars; `dat` is passed to
   `backtest()` with explicit engine options. Inspect the returned `bt$data` too.

The strategy clock determines when instructions become available; the execution
clock determines their possible fills. Keep `OrderSize` as a one-shot event and
`NextOpenTime`/`NextOpen` as sparse markers when joining clocks. The supplied
ask/bid construction is a synthetic assumption, and OHLC bars cannot establish
every intrabar price sequence. See the guide for
[required fields, absent-column defaults and conditional exit inputs](GUIDE.md#preparing-inputs-for-a-first-strategy).

## Examples and documentation

The source contains ten strategy/example scripts plus one supplementary setup
file. The [guide](GUIDE.md) explains execution, clocks, sizing, performance and a
[scheduled partial-exit demonstration](GUIDE.md#supplementary-scheduled-partial-exit-demonstration).
The latter uses the first 160 bundled observations and retains three scheduled
entries and partial/final exits; it is separate from the moving-average strategy.

Run examples in fresh R sessions. The other standalone strategy scripts still
begin with `rm(list = ls())` and may need additional packages; read them before
sourcing them into an existing workspace.

| File | Purpose and use |
|---|---|
| [ma_crossover.R](inst/examples/ma_crossover.R) | Complete main example above: 20/50 moving averages, percentage stop and opposite-crossover signal exit; daily strategy and supplied finer execution bars. |
| [csv_demo_setup.R](inst/examples/csv_demo_setup.R) | Supplementary preparation for scheduled entries and partial/final exits; run it through the guide's complete example. |
| [dynamic_exits_trailing.R](inst/examples/dynamic_exits_trailing.R) | Dynamic target and stop levels. The stop can move down as well as up; the script does not enforce a one-way trailing stop. Its header trade count is outdated. |
| [boilerplate.R](inst/examples/boilerplate.R), [boilerplate2.R](inst/examples/boilerplate2.R) | Longer workflow examples including optimisation; inspect separately from the quick start. |
| [mean_reversion_long_short.R](inst/examples/mean_reversion_long_short.R) | Long/short mean-reversion and partial-exit pattern for inspection. |
| [multi_exit_conditions.R](inst/examples/multi_exit_conditions.R) | Multiple exit mechanisms for inspection. |
| [stop_bracket_breakout.R](inst/examples/stop_bracket_breakout.R) | Stop-entry/bracket pattern for inspection. |
| [vix_regime.R](inst/examples/vix_regime.R) | Regime-based sizing; requires an internet connection and a Yahoo Finance download. |
| [volatility_scaling.R](inst/examples/volatility_scaling.R), [volatility_target.R](inst/examples/volatility_target.R) | Volatility-based sizing patterns for inspection. |

Both introductory examples use the bundled `inst/extdata/crude_wti_futures.csv`
without downloading prices. Other scripts may need additional data or packages;
the index identifies these requirements and known issues. They are examples to
inspect and adapt, not a promise that every standalone script runs unchanged.

Installed function help is available through `?backtest`, `?mergeAskBid`,
`?exit_TP_SL_signal`, `?getOrderSize`, `?analyze_performance`, `?plotEquityCurve`,
`?plotTrades` and `?plotSingleTrade`; the source help pages are in [man/](man/).

## Scope

The table distinguishes implemented features, their limits and possible
extensions. “Tested” refers to specific test cases, not every combination of
orders and market data. The [software record](../../reproduction/SOFTWARE_PROVENANCE.md#tradetesterr) gives the test coverage.

| Theoretical method or capability | Package interface | Status | Boundary / evidence |
|---|---|---|---|
| Market orders | `backtest()`, `EntryType="Market"` | **Implemented and tested** | Execution uses supplied or next-bar price; no liquidity model. |
| Limit orders | `backtest()`, `EntryType="Limit"` | **Implemented with stated limitations** | bid/ask bar threshold; unfilled/cancelled instructions retained in `orders`. Default next-strategy-bar expiry. No order-book or fill-quantity model. |
| Stop orders | `backtest()`, `EntryType="Stop"` | **Implemented and tested** | Crossed eligible openings use ask for buys and bid for sells; later intrabar touches use the threshold approximation. No order-book or general slippage model. |
| Stop-limit orders | No dedicated branch | **Not implemented** | No stop-limit branch. Such orders must not be passed to the current engine. |
| Next-bar activation | `EntryTime`; sparse `NextOpenTime`/`NextOpen` clock | **Implemented with stated limitations** | Default uses the next strategy boundary when supplied, otherwise the next execution timestamp. Explicit strategy timestamps remain the caller's responsibility. |
| Dual-timeframe execution | Execution table passed to `backtest()`; `mergeHighLowTimes()` | **Implemented with stated limitations** | Engine consumes a prepared table. The internal `.align_daily_rules()` supplies bounded as-of state and one-shot events for the ES reproduction. Existing exported aggregation interfaces are unchanged. |
| Ask/bid spread treatment | `mergeAskBid()` and ask/bid OHLC fields | **Implemented and tested** | Exact same-frequency intersection; the spread series must be supplied. |
| Commissions | No commission argument in `backtest()` | **Future extension** | `Returns = Side * (ExitPrice - EntryPrice) * abs(ExitAmount)` has no commission deduction. |
| Slippage | No slippage model | **Not implemented** | The observed-opening convention for crossed stops is implemented; additional slippage, liquidity and market impact are not modelled. |
| Market impact | No interface | **Future extension** | No participation, liquidity, or price-impact function in execution. |
| Partial exits | `exit_TP_SL_signal()` | **Implemented and tested** | Positions can close in multiple exit legs; partial exits do not establish partial entry fills. |
| Partial fills | No entry-fill quantity interface | **Not implemented** | Partial position exits do not establish partial entry fills. |
| Order rejection | Instruction audit in `bt$orders` | **Not implemented as a broker model** | Records invalid, ignored, cancelled and unfilled instructions, but does not model exchange/broker rejection. |
| Multiple profit targets | Numbered `targetorder_*_N` and amount fields | **Implemented and tested** | Numbered targets are conditional exit instructions, not guaranteed fills. Ledger rows record executed exit legs rather than declared targets or necessarily whole trades. |
| Trailing stops | Numbered `stoporder_*_N` fields | **Implemented with stated limitations** | `R/exit_strategies.R` consumes caller-created levels and shifts via `NextOpenTime`; causal input preparation is required. |
| Time exits | `TradeTimeLimit`, `CLOSE_CANDLES_SINCE_ENTRY` | **Implemented with stated limitations** | Candle counting uses nonmissing `NextOpen` rows, so this marker must remain sparse. Time, horizon and data-end exits have separate labels; missing future markers no longer discard observed exits. |
| Pyramiding | `AddToPosition=TRUE` | **Implemented with stated limitations** | Allows overlapping entry processing; no consolidated account exposure/margin ledger. |
| Stock sizing | `getOrderSize(type="Stocks")` | **Implemented with stated limitations** | Dollar amount / price; fractional result, without rounding/lot validation or account-aware allocation. |
| Futures contract sizing/multiplier | `getOrderSize(type="Futures", contract_multiplier=...)` | **Implemented with stated limitations** | Sizing arithmetic exists; backtest P&L has no separate contract-multiplier parameter. Scripts must state the units encoded in `OrderSize`; the ES adapter multiplies contract-point P&L by its explicit 50 USD/point assumption once. |
| Percentage-of-equity sizing | No dedicated interface | **Not implemented** | Caller can compute an order size, but the engine does not update it automatically from equity. |
| Volatility-based sizing | `inst/examples/volatility_target.R`, `volatility_scaling.R` | **Implemented with stated limitations** | Script arithmetic using ATR/volatility and fixed budgets; not a general account-level sizing API. |
| Kelly sizing | No exported or internal implementation found | **Not implemented** | Kelly theory is outside the implemented API. |
| Leverage and margin accounting | `getOrderSize(leverage=...)` only | **Not implemented** | Leverage enters sizing arithmetic; no borrow funding, margin requirement, liquidation or margin-call ledger. |
| Short positions | Negative `OrderSize` | **Implemented and tested** | Negative orders represent short positions; no stock borrow or funding costs. |
| Walk-forward optimisation | `optimizeStrategy()` | **Implemented with stated limitations** | Explicit strategy and frozen risk-free inputs; recorded contiguous windows. See the [window rules](GUIDE.md#entry-exit-and-window-policies). |
| Parameter-stability diagnostics | `analyzeParameterStability()`, `compareISOS()` | **Implemented with stated limitations** | `compareISOS()` groups partial-exit legs sharing an `Order_ID` and counts them as one trade within each backtest; IS/OOS use the same metrics and profit per calendar day. Descriptive only, without statistical guarantees. Without an `Order_ID` column, custom-ledger rows are counted separately; distinguish IDs by run/window when pooling ledgers (see [GUIDE.md](GUIDE.md)). |
| Monte Carlo simulation | No interface found | **Not implemented** | Monte Carlo research is outside the implemented API. |
| Live brokerage/exchange connectivity | No interface | **Future extension** | Historical simulation only. |
| Tick-level/queue-priority execution | No queue/order-book model | **Future extension** | Accepting finer timestamps does not implement queue priority or tick-level fill mechanics. |

## Thesis companion materials

The [repository home page](../../README.md) explains where to find the chapter
scripts, figures and tables. To redraw the supplied figures or explore the study
code, follow the [reproduction instructions](../../reproduction/README.txt).
Full historical study runs need additional inputs and software; the
[data-availability page](../../DATA_AVAILABILITY.txt) explains what is included.

Snapshot details and check history: [software provenance](../../reproduction/SOFTWARE_PROVENANCE.md#tradetesterr).
