# B1 source-derived core: original calculations unchanged.
# Run through run_one.R for explicit setup, evidence capture and plotting.
momentum_signal <- calc_momentum(monthly_prices, lookback = 1)
bench_selected  <- filter_top_n(momentum_signal, n = 1e9)   # select all
bench_weights   <- weight_equally(bench_selected)

bench <- run_backtest(
  prices = monthly_prices,
  weights = bench_weights,
  initial_capital = 100000,
  name = "EW Benchmark"
)

summary(bench)
