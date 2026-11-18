# B1 source-derived core: original calculations unchanged.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.2: RSI Mean Reversion
rsi <- calc_rsi(monthly_prices, period = 14)
oversold <- filter_below(rsi, value = 40)  # Oversold threshold
weights <- weight_equally(oversold)
result_rsi <- run_backtest(
  prices = monthly_prices,
  weights = weights,
  initial_capital = 100000,
  name = "RSI Mean Reversion"
)

summary(result_rsi)
