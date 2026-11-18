# B1 source-derived core: original calculations unchanged.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.10: Stochastic Oversold Strategy (weekly)
library(PortfolioTesteR)
data(sample_prices_weekly)

# Signal → selection → cap → weights
stoch_d  <- calc_stochastic_d(sample_prices_weekly, k = 14, d = 3)
oversold <- filter_below(stoch_d, value = 30)
limited  <- limit_positions(oversold, max_positions = 8)
weights  <- weight_equally(limited)

# Optional: weekly EW benchmark for overlay
ms     <- calc_momentum(sample_prices_weekly, lookback = 1)
selall <- filter_top_n(ms, n = 1e9)
w_ew   <- weight_equally(selall)
bench  <- run_backtest(sample_prices_weekly, w_ew, 100000, "EW Benchmark")

# Backtest & evaluate
result <- run_backtest(sample_prices_weekly, weights, 100000, "Stochastic Oversold")
summary(result)
