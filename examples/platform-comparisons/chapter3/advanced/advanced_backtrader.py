import backtrader as bt


class BollingerRsiStaged(bt.Strategy):
    params = dict(bb_period=20, rsi_period=14, rsi_oversold=30,
                  contracts=4, multiplier=50, risk_budget=2000, max_bars=10)

    def __init__(self):
        typical = (self.data.high + self.data.low + self.data.close) / 3
        self.bands = bt.ind.BollingerBands(typical, period=self.p.bb_period)
        self.rsi = bt.ind.RSI(self.data.close, period=self.p.rsi_period, safediv=True)
        self.entry = None
        self.exits = []
        self.middle_done = False
        self.entry_bar = None
        self.stop_level = None

    def notify_order(self, order):
        if order.status == order.Completed:
            if order.isbuy():
                self.entry_bar = len(self)
                self.stop_level = order.info.stop_level
                self.middle_done = False
            elif order.info.get("role") == "middle":
                self.middle_done = True
        if self.entry is not None and order.ref == self.entry.ref and not order.alive():
            self.entry = None

    def next(self):
        for order in self.exits:
            if order.alive():
                self.cancel(order)
        self.exits = []
        if self.position:
            if len(self) - self.entry_bar >= self.p.max_bars - 1:
                self.close().addinfo(role="time")
                return
            legs = [(self.position.size, self.bands.top[0], "upper")]
            if not self.middle_done:
                half = self.position.size / 2
                legs = [(half, self.bands.mid[0], "middle"),
                        (half, self.bands.top[0], "upper")]
            for size, target, role in legs:
                stop = self.sell(size=size, exectype=bt.Order.Stop, price=self.stop_level)
                stop.addinfo(role="stop")
                limit = self.sell(size=size, exectype=bt.Order.Limit, price=target, oco=stop)
                limit.addinfo(role=role)
                self.exits.extend([stop, limit])
            return
        if self.entry is not None and self.entry.alive():
            self.cancel(self.entry)
        if self.rsi[0] < self.p.rsi_oversold and self.data.close[0] <= self.bands.bot[0]:
            level = self.bands.bot[0]
            stop = level - self.p.risk_budget / (self.p.contracts * self.p.multiplier)
            self.entry = self.buy(size=self.p.contracts, exectype=bt.Order.Limit, price=level)
            self.entry.addinfo(stop_level=stop)
