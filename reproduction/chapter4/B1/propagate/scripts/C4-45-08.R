# B1 source-derived core: original calculations unchanged.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.8: Stochastic Oversold Bounce with Adaptive Weighting
# Uses: calc_stochastic_d, filter_below, limit_positions, switch_weights, weight_by_rank

# Load data and convert to monthly
data(sample_prices_weekly)
monthly_prices <- convert_to_nweeks(sample_prices_weekly, n = 4)

# Create equal-weight market proxy (consistent with Strategy 5.5)
# Calculate returns first
ew_prices <- monthly_prices[, -1]  # Remove Date column
ew_returns <- ew_prices / shift(ew_prices, 1) - 1  # Calculate returns
ew_index_returns <- rowMeans(ew_returns, na.rm = TRUE)  # Equal-weight returns

# Calculate rolling volatility of the market proxy
market_vol <- frollapply(ew_index_returns, n = 5, FUN = sd, fill = NA)

# Calculate Stochastic %D (smoothed stochastic)
stoch_d <- calc_stochastic_d(monthly_prices, k = 14, d = 3)

# Filter for oversold conditions (Stochastic < 30)
oversold <- filter_below(stoch_d, value = 30)

# Limit to maximum 8 positions
limited <- limit_positions(oversold, max_positions = 8)

# Two weighting schemes:
# A) Rank-weighted: deeper oversold (lower %D) gets MORE weight
weights_signal <- weight_by_rank(limited, stoch_d, ascending = TRUE)
# B) Equal weight for defensive positioning
weights_equal <- weight_equally(limited)

# Regime switching: high volatility = defensive (equal), low vol = aggressive (signal)
vol_threshold <- median(market_vol, na.rm = TRUE)
use_equal <- market_vol > vol_threshold
use_equal[is.na(use_equal)] <- TRUE  # Default to defensive if NA

# Switch between weighting schemes based on volatility regime
final_weights <- switch_weights(
  weights_a = weights_signal,  # Default: signal-based
  weights_b = weights_equal,   # Alternative: equal weight
  use_b_condition = use_equal  # Use equal when volatility is high
)

# Run backtest
result <- run_backtest(
  prices = monthly_prices,
  weights = final_weights,
  initial_capital = 100000,
  name = "Stochastic Oversold with Adaptive Weights"
)

# Display results
print(result)
summary(result)
