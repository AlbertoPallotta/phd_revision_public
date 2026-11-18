from zipline.api import order_target_percent, record, symbol, set_benchmark
from zipline import run_algorithm
import pandas as pd

def initialize(context):
    context.asset = symbol('AAPL')        # the asset we want to trade
    context.short_window = 50
    context.long_window = 200
    set_benchmark(context.asset)          # track AAPL performance as benchmark

def handle_data(context, data):
    # Compute short and long moving averages of the asset's price
    short_mavg = data.history(context.asset, 'price', bar_count=context.short_window, frequency="1d").mean()
    long_mavg  = data.history(context.asset, 'price', bar_count=context.long_window, frequency="1d").mean()
    # Trading logic: generate orders on crossover
    if short_mavg > long_mavg and context.portfolio.positions[context.asset].amount == 0:
        order_target_percent(context.asset, 1.0)   # fully allocate to asset
    elif short_mavg < long_mavg and context.portfolio.positions[context.asset].amount > 0:
        order_target_percent(context.asset, 0.0)   # exit position (go to 0% allocation)
    # Record values for later analysis
    record(short_mavg=short_mavg, long_mavg=long_mavg, price=data.current(context.asset, 'price'))

# Load historical price data (for example, from a CSV or bundling system) and run.
start = pd.Timestamp("2019-01-01", tz='utc')
end = pd.Timestamp("2020-01-01", tz='utc')
result = run_algorithm(start=start, end=end, initialize=initialize, handle_data=handle_data,
                       capital_base=100000, data_frequency='daily', bundle='quantopian-quandl')
