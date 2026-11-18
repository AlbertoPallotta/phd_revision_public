from datetime import datetime
import backtrader as bt

# 1. Define the strategy by subclassing bt.Strategy
class SmaCross(bt.Strategy):
    params = (('fast', 50), ('slow', 200), )  # moving average periods

    def __init__(self):
        # Initialize two Simple Moving Averages on the data's closing prices
        self.fast_ma = bt.indicators.SimpleMovingAverage(self.datas[0].close, 
                                                         period=self.params.fast)
        self.slow_ma = bt.indicators.SimpleMovingAverage(self.datas[0].close, 
                                                         period=self.params.slow)

    def next(self):
        # Check if we are currently in a position
        if not self.position:  
            # If no position and fast MA crosses above slow MA, enter long
            if self.fast_ma[0] > self.slow_ma[0] and self.fast_ma[-1] <= self.slow_ma[-1]:
                self.buy()  # enter long position (buy at next bar open by default)
        else:
            # If we have a position and fast MA crosses back below slow MA, exit
            if self.fast_ma[0] < self.slow_ma[0] and self.fast_ma[-1] >= self.slow_ma[-1]:
                self.close()  # close the position (sell)
                
# 2. Set up backtesting engine (cerebro) and data
cerebro = bt.Cerebro()
cerebro.addstrategy(SmaCross)
# Load daily data for an example asset, e.g., from Yahoo Finance or a CSV
data = bt.feeds.YahooFinanceCSVData(dataname='AAPL.csv', fromdate=datetime(2020,1,1), todate=datetime(2021,1,1))
cerebro.adddata(data)
cerebro.broker.set_cash(100000)

# 3. Run the backtest
result = cerebro.run()
print(f"Final Portfolio Value: ${cerebro.broker.getvalue():.2f}")

