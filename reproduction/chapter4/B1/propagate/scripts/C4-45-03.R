# B1 amended core: only the explicit momentum NA policy differs from the original.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.3: Sector-Relative Momentum
momentum <- calc_momentum(monthly_prices, lookback = 6, na_policy = "propagate")
sector_pct <- rank_within_sector(
  momentum, 
  sector_mapping  = sample_sp500_sectors
)  # Returns 0-100 percentiles
selected <- filter_above(sector_pct, value = 70)  # Top 30% within each sector
weights <- weight_equally(selected)
result_sector <- run_backtest(
  prices = monthly_prices,
  weights = weights,
  initial_capital = 100000,
  name = "Sector-Relative Momentum"
)
summary(result_sector)
