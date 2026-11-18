# B1 amended core: only the explicit momentum NA policy differs from the original.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.5: Regime-Filtered Momentum
# Use equal-weight index as market proxy for regime detection

# Create equal-weight index from our universe (market proxy)
# This represents the "market" condition using our available stocks
ew_prices <- monthly_prices[, -1]  # Remove Date column
ew_returns <- ew_prices / shift(ew_prices, 1) - 1  # Calculate returns
ew_index_returns <- rowMeans(ew_returns, na.rm = TRUE)  # Equal-weight returns
ew_index <- cumprod(1 + c(0, ew_index_returns[-1]))  # Cumulative index

# Calculate MA of the index
ma_periods <- 10
ew_ma <- frollmean(ew_index, n = ma_periods, align = "right", fill = NA)

# Create regime: Index > MA = bullish (trade), Index < MA = defensive (cash)
bullish_regime <- ew_index > ew_ma
bullish_regime[is.na(bullish_regime)] <- FALSE  # Conservative: NA = cash

# Momentum selection
momentum <- calc_momentum(monthly_prices, lookback = 12, na_policy = "propagate")
selected <- filter_top_n(momentum, n = 5)

# Apply regime filter using library function
regime_filtered <- apply_regime(
  selected,
  regime_condition = bullish_regime,
  partial_weight = 0  # 0 = full cash when regime is FALSE
)

weights <- weight_equally(regime_filtered)
result_regime <- run_backtest(
  prices = monthly_prices,
  weights = weights,
  initial_capital = 100000,
  name = "Regime-Filtered Momentum"
)
summary(result_regime)
