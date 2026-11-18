# Exact shared data/grid setup from Word p02750-p02759.
# n=4 means anchored28-day bins, not calendar months.
library(PortfolioTesteR)
library(data.table)

# Bundled data
data(sample_prices_weekly)
data(sample_prices_daily)
data(sample_sp500_sectors)

# Monthly grid → monthly rebalancing
monthly_prices <- convert_to_nweeks(sample_prices_weekly, n = 4)
