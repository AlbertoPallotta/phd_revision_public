import bt
import pandas as pd

# Fetch historical data (bt has a convenient data loader for single asset)
data = bt.get('aapl', start='2019-01-01', end='2020-12-31')  # DataFrame of AAPL prices

# Compute 50-day and 200-day moving averages
fast_ma = data.rolling(50).mean()
slow_ma = data.rolling(200).mean()

# Create target weight series: 1.0 (100% long) when fast_ma > slow_ma, else 0.0
signal = pd.DataFrame(index=data.index)
signal['weight'] = 0.0
signal.loc[fast_ma['aapl'] > slow_ma['aapl'], 'weight'] = 1.0

# Define strategy: when rebalancing, use the target weights from signal
strategy = bt.Strategy('SMA_Cross',
                       [bt.algos.WeighTarget(signal[['weight']].rename(columns={'weight': 'aapl'})),   # allocate according to target weight
                        bt.algos.Rebalance()])                    # rebalance portfolio to match target
# Set up backtest with initial capital
bt_test = bt.Backtest(strategy, data, initial_capital=100000)
# Run the backtest
res = bt.run(bt_test)
res.display()        # display performance metrics
res.plot()           # plot equity curve
