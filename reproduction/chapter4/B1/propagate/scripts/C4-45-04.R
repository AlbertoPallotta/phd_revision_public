# B1 amended core: only the explicit momentum NA policy differs from the original.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.4: Volatility-Weighted Momentum
momentum <- calc_momentum(monthly_prices, lookback = 12, na_policy = "propagate")
selected <- filter_top_n(momentum, n = 8)  # Top 8 by momentum

# Weight by volatility: pass raw daily prices, let the function handle everything
weights <- weight_by_volatility(
  selected_df            = selected,
  vol_timeframe_data     = sample_prices_daily,      # raw daily prices
  strategy_timeframe_data = monthly_prices,           # alignment target
  lookback_periods       = 60,                        # 60-day vol window
  low_vol_preference     = TRUE,                      # lower vol = higher weight
  vol_method             = "std"
)

result_vol <- run_backtest(
  prices = monthly_prices,
  weights = weights,
  initial_capital = 100000,
  name = "Volatility-Weighted Momentum"
)

summary(result_vol)
