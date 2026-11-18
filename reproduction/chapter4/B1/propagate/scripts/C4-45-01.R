# B1 amended core: only the explicit momentum NA policy differs from the original.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.1: Classic Momentum (monthly)
# Assumes `monthly_prices` and the EW `bench` from Section 5.0 are already in scope.

momentum <- calc_momentum(monthly_prices, lookback = 12, na_policy = "propagate")  # 12-month signal
selected <- filter_top_n(momentum, n = 5)                  # top 5 each month
weights  <- weight_equally(selected)                       # normalized row-wise

result <- run_backtest(
  prices = monthly_prices,
  weights = weights,
  initial_capital = 100000,
  name = "Simple Momentum"
)

summary(result)
