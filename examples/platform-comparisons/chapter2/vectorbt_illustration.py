import vectorbt as vbt

# Load price data, e.g., for AAPL (vectorbt can fetch from yfinance easily)
price = vbt.YFData.download("AAPL", start="2019-01-01", end="2020-12-31").get("Close")

# Compute moving averages using vectorbt indicators
fast_ma = vbt.MA.run(price, window=50).ma
slow_ma = vbt.MA.run(price, window=200).ma

# Generate entry and exit signals as boolean arrays
entries = (fast_ma > slow_ma)  # True when fast MA is above slow MA
exits   = (fast_ma < slow_ma)  # True when fast MA falls below slow MA

# Run portfolio simulation from signals
portfolio = vbt.Portfolio.from_signals(price, entries, exits, init_cash=100000)
portfolio.stats()
portfolio.plot().show()
