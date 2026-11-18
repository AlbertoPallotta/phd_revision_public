# B1 source-derived core: original calculations unchanged.
# Run through run_one.R for explicit setup, evidence capture and plotting.
# Strategy 5.9: CCI Trend Following
# Uses: calc_cci, filter_above, filter_by_percentile, analyze_performance

# Load data and convert to monthly
data(sample_prices_weekly)
data(sample_prices_daily)  
monthly_prices <- convert_to_nweeks(sample_prices_weekly, n = 4)

# Calculate CCI (Commodity Channel Index)
cci_values <- calc_cci(monthly_prices, period = 20)

# Filter for strong CCI readings (> 100 indicates strong trend)
strong_trend <- filter_above(cci_values, value = 100)

# Additionally filter by percentile - only top 30% of CCI values
high_percentile <- filter_by_percentile(cci_values, percentile = 30, type = "top")

# Combine both conditions
selected <- combine_filters(strong_trend, high_percentile, op = "and")

# Limit positions and weight by signal strength
limited <- limit_positions(selected, max_positions = 10)
weights <- weight_by_signal(limited, cci_values)

# Run backtest
result <- run_backtest(
  prices = monthly_prices,
  weights = weights,
  initial_capital = 100000,
  name = "CCI Trend Following"
)

# Display results
print(result)
summary(result)

# Analyze performance with daily data for detailed plots
perf <- analyze_performance(result, sample_prices_daily)

# Plot 4-panel summary
