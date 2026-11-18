from backtesting import Backtest, Strategy
from backtesting.lib import crossover
from backtesting.test import SMA, GOOG  # GOOG is example dataset, SMA is indicator func

# Define strategy by specifying what to do each bar
class SmaCrossStrategy(Strategy):
    n_fast = 50
    n_slow = 200
    def init(self):
        # Initialize moving averages for closing price
        self.fast_ma = self.I(SMA, self.data.Close, self.n_fast)
        self.slow_ma = self.I(SMA, self.data.Close, self.n_slow)
    def next(self):
        # If fast MA crosses above slow MA, open long position (buy)
        if crossover(self.fast_ma, self.slow_ma):
            self.position.close()        # first, close any existing position
            self.buy()
        # If fast MA crosses below slow MA, close any long position (or short, if supported)
        elif crossover(self.slow_ma, self.fast_ma):
            self.position.close()

# Run backtest on example data (Google stock OHLCV from backtesting.test)
bt = Backtest(GOOG, SmaCrossStrategy, cash=100000, commission=.002)
stats = bt.run()
print(stats)  # print a summary of performance
bt.plot()     # (in Jupyter, this would show an interactive equity curve chart)
