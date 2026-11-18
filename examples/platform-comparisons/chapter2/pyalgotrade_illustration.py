from pyalgotrade import strategy
from pyalgotrade.tools import yahoofinance
from pyalgotrade.technical import ma, cross

class SMACross(strategy.BacktestingStrategy):
    def __init__(self, feed, instrument, fast_period, slow_period):
        super(SMACross, self).__init__(feed)  # initialize base class
        self.__instrument = instrument
        self.__position = None
        self.setUseAdjustedValues(True)  # use adjusted values as specified
        # Get price series and set up indicators
        self.__prices = feed[instrument].getPriceDataSeries()
        self.__fast_sma = ma.SMA(self.__prices, fast_period)
        self.__slow_sma = ma.SMA(self.__prices, slow_period)

    def onBars(self, bars):
        # If no open position, check for a bullish crossover
        if self.__position is None:
            if cross.cross_above(self.__fast_sma, self.__slow_sma) > 0:
                # Calculate number of shares to buy (90% of cash)
                cash = self.getBroker().getCash()
                price = bars[self.__instrument].getPrice()
                shares = int(cash * 0.9 / price)
                if shares > 0:
                    self.__position = self.enterLong(self.__instrument, shares)
        # If a position is open, check for bearish crossover to exit
        elif self.__position.isOpen():
            if cross.cross_below(self.__fast_sma, self.__slow_sma) > 0:
                self.__position.exitMarket()

    def onEnterCanceled(self, position):
        self.__position = None

    def onExitOk(self, position):
        self.__position = None

# Set up feed and run the strategy
feed = yahoofinance.build_feed(["AAPL"], 2019, 2020, ".")

strategy = SMACross(feed, "AAPL", fast_period=50, slow_period=200)
strategy.run()
print(f"Final portfolio value: ${strategy.getBroker().getEquity():.2f}")

