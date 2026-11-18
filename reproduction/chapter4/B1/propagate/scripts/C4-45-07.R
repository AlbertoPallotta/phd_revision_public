# B1 source-derived core: original calculations unchanged.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.7: Distance from Moving Average Mean Reversion  
# Uses: calc_moving_average, calc_distance, filter_top_n, weight_by_rank

# Load data and convert to monthly (4-week periods)
data(sample_prices_weekly)
monthly_prices <- convert_to_nweeks(sample_prices_weekly, n = 4)

# Calculate 5-period moving average (5 months)
ma_5 <- calc_moving_average(monthly_prices, window = 5)

# Calculate distance from MA (percentage)
distance_from_ma <- calc_distance(monthly_prices, ma_5)

# Select 10 stocks furthest below their MA (ascending=TRUE for lowest values)
selected <- filter_top_n(distance_from_ma, n = 10, ascending = TRUE)

# Rank-weight: more negative distance gets higher weight
weights <- weight_by_rank(selected, distance_from_ma, ascending = TRUE)

# Run backtest
result <- run_backtest(
  prices = monthly_prices,
  weights = weights,
  initial_capital = 100000,
  name = "MA Distance Mean Reversion"
)

# Display results
print(result)
summary(result)
