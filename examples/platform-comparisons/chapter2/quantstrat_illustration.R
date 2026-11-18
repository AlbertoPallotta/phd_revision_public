library(quantstrat)
library(quantmod)

# Initialize environment and portfolio
currency("USD")
stock("AAPL", currency = "USD", multiplier = 1)
initDate <- "2019-01-01"
startDate <- "2020-01-01"; endDate <- "2020-12-31"
initEq <- 100000
# Get data
getSymbols("AAPL", src="yahoo", from=startDate, to=endDate)
# Set up portfolio, account, orders
strategy.st <- portfolio.st <- account.st <- "SMACross"
initPortf(portfolio.st, symbols="AAPL", initDate=initDate)
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

# Indicators: 50-day and 200-day SMA
add.indicator(strategy.st, name = "SMA", 
              arguments = list(x=quote(Cl(mktdata)), n=50), label="fast")
add.indicator(strategy.st, name = "SMA", 
              arguments = list(x=quote(Cl(mktdata)), n=200), label="slow")

# Signals: crossover signals (fast SMA crossing above/below slow SMA)
add.signal(strategy.st, name="sigCrossover", label="bull", 
           arguments = list(columns=c("fast", "slow"), relationship="gt"))   # fast > slow
add.signal(strategy.st, name="sigCrossover", label="bear", 
           arguments = list(columns=c("fast", "slow"), relationship="lt"))   # fast < slow

# Rules: enter long on "bull" signal, exit on "bear" signal
add.rule(strategy.st, name="ruleSignal", type="enter", label="enterLong",
         arguments=list(sigcol="bull", sigval=TRUE, orderqty=1, ordertype="market", orderside="long"))
add.rule(strategy.st, name="ruleSignal", type="exit", label="exitLong",
         arguments=list(sigcol="bear", sigval=TRUE, orderqty="all", ordertype="market", orderside="long"))

# Apply strategy to generate trades
applyStrategy(strategy.st, portfolios=portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

