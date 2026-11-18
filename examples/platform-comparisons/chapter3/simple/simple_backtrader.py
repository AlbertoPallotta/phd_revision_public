"""Completed simple illustration: one unit, no additions or short entries.
Standard Backtrader bar matching; stop submitted after entry notification becomes
eligible on the following broker cycle, not retroactively on the entry bar.
"""
import backtrader as bt

class SimpleCrossover(bt.Strategy):
    params = dict(fast=20, slow=50, stop_pct=0.02)
    def __init__(self):
        self.fast = bt.ind.SMA(self.data.close, period=self.p.fast)
        self.slow = bt.ind.SMA(self.data.close, period=self.p.slow)
        self.pending = None
        self.protection = None
        self.executions = []

    def notify_order(self, order):
        if order.status == order.Completed:
            self.executions.append(dict(date=str(self.data.datetime.date(0)),
                size=order.executed.size, price=order.executed.price))
            if order.isbuy():
                self.protection = self.sell(size=1, exectype=bt.Order.Stop,
                    price=order.executed.price * (1-self.p.stop_pct))
        if self.pending is not None and order.ref == self.pending.ref and not order.alive():
            self.pending = None
        if self.protection is not None and order.ref == self.protection.ref and not order.alive():
            self.protection = None

    def next(self):
        if self.pending is not None:
            return
        up = self.fast[0] > self.slow[0] and self.fast[-1] <= self.slow[-1]
        down = self.fast[0] < self.slow[0] and self.fast[-1] >= self.slow[-1]
        if not self.position and up:
            self.pending = self.buy(size=1)
        elif self.position and down:
            if self.protection is not None and self.protection.alive():
                self.cancel(self.protection)
            self.pending = self.close()
