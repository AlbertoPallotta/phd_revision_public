library(data.table)

execution_fixture <- function(side=1) {
  t <- as.POSIXct("2025-01-06 10:00:00", tz="UTC") + 0:5*300
  d <- data.table(date=t, Open.a=100, High.a=101, Low.a=99, Close.a=100,
                 Open.b=99.75, High.b=100.75, Low.b=98.75, Close.b=99.75,
                 OrderSize=0, EntryTime=t, EntryPrice=100, EntryType="Market",
                 TakeProfit=110, StopLoss=90)
  d[1, OrderSize:=side*2]
  if (side < 0) d[, `:=`(TakeProfit=90, StopLoss=110)]
  d
}

test_that("missing future candle marker preserves observed exits", {
  d <- execution_fixture()
  d[, `:=`(CLOSE_CANDLES_SINCE_ENTRY=15, NextOpen=NA_real_)]
  d[2, High.b:=111]
  b <- backtest(d, verbose=FALSE)
  expect_equal(b$results$Returns, 20)
  expect_equal(b$results$ExitReason, "TakeProfit")
  expect_equal(b$results$ExitTime, d$date[2])
  expect_equal(d$OrderSize, c(2, rep(0,5)))
})

test_that("explicit horizon prevents a later profit from leaking into a window", {
  d <- execution_fixture()
  d[5, High.b:=111]
  b <- backtest(d, end_time=d$date[4], verbose=FALSE)
  expect_equal(b$results$ExitReason, "Execution horizon")
  expect_equal(b$results$Returns, -0.5)
  expect_equal(b$results$ExitTime, d$date[4])
  expect_true(all(b$data$date < d$date[4]))
})

test_that("same-bar stop-first is explicit and unknowns cannot disappear", {
  for (side in c(1,-1)) {
    d <- execution_fixture(side)
    d[2, `:=`(High.b=112, High.a=112.25, Low.a=89, Low.b=88.75)]
    u <- backtest(d, verbose=FALSE)
    expect_true(is.na(u$results$Returns))
    expect_error(analyze_performance(u), "unknown/nonfinite")
    b <- backtest(d, exit_args=list(ambiguity="stop_first"), verbose=FALSE)
    expect_equal(b$results$Returns, -20)
    expect_true(b$results$AmbiguousBar)
    expect_equal(b$results$ExitReason, "StopLoss (same-bar assumption)")
  }
})

test_that("terminal short closes use the ask and small samples are evaluated", {
  d <- execution_fixture(-1)[1:2]
  d[, Close.a:=102]
  b <- backtest(d, verbose=FALSE)
  expect_equal(b$results$Returns, -4)
  expect_equal(b$results$ExitReason, "End of data")
})

test_that("a pending entry expires without needing a cancellation instruction", {
  d <- execution_fixture()
  d[, EntryType:="Limit"]
  d[, Low.a:=101]
  d[5, Low.a:=99]
  b <- backtest(d, verbose=FALSE)
  expect_equal(nrow(b$results), 0L)
  expect_equal(b$orders$Disposition, "expired_entry_bar")
  expect_equal(b$orders$DispositionTime, d$date[2])
})

test_that("invalid custom exits fail position conservation", {
  d <- execution_fixture()
  bad <- function(x) data.table(ExitTime=x$date[2], ExitAmount=-3,
                                 ExitPrice=101, CurrentPosSize=0)
  expect_error(backtest(d, exit_fun=bad, verbose=FALSE), "conserve")
})

test_that("alignment respects visibility, exclusive expiry and one-shot events", {
  t <- as.POSIXct("2025-03-09 01:30:00", tz="America/New_York")
  x <- data.table(date=t+0:11*300, price=1:12)
  r <- data.table(RuleTime=t-3600+c(0,1800), AvailableAt=t+c(600,2400),
                  ValidUntil=t+c(1500,3300), ATR=c(1,2), OrderSize=c(2,-2),
                  EntryTime=t+c(600,2400))
  before_x <- copy(x); before_r <- copy(r)
  a <- TradeTesterR:::.align_daily_rules(x[12:1], r[2:1], "ATR", c("OrderSize","EntryTime"), x$date[c(1,7)])
  expect_identical(x, before_x); expect_identical(r, before_r)
  expect_equal(a$data$date, x$date)
  expect_equal(a$data$ATR, c(NA,NA,1,1,1,NA,NA,NA,2,2,2,NA))
  expect_equal(which(a$data$OrderSize != 0), c(3L,9L))
  expect_equal(which(!is.na(a$data$NextOpen)), c(1L,7L))
  expect_equal(nrow(a$data), nrow(x))
  expect_true(all(a$data$StateRuleTime <= a$data$date, na.rm=TRUE))
  expect_error(TradeTesterR:::.align_daily_rules(x, r[c(1,1)], "ATR", c("OrderSize","EntryTime")), "overlap")
  r[1, RuleTime:=AvailableAt+1]
  expect_error(TradeTesterR:::.align_daily_rules(x, r, "ATR", c("OrderSize","EntryTime")), "RuleTime")
})

test_that("an absent activation bar is recovered once, an expired interval is not", {
  t <- as.POSIXct("2025-01-01", tz="UTC")
  x <- data.table(date=t+c(0,600,1200))
  r <- data.table(RuleTime=t, AvailableAt=t+300, ValidUntil=t+900,
                  OrderSize=1, EntryTime=t+300, ATR=10)
  a <- TradeTesterR:::.align_daily_rules(x,r,"ATR",c("OrderSize","EntryTime"))
  expect_equal(a$data$OrderSize, c(0,1,0))
  expect_equal(a$data$EntryTime[2],t+600)
  r[, ValidUntil:=t+600]
  a <- TradeTesterR:::.align_daily_rules(x,r,"ATR",c("OrderSize","EntryTime"))
  expect_equal(sum(a$data$OrderSize),0)
  expect_equal(nrow(a$audit$unplaced_events),1)
})

test_that("first and weekend realised profits enter returns and losses enter drawdown", {
  bt <- list(results=data.table(Order_ID=1:2, ExitTime=as.POSIXct(c("2025-01-05","2025-01-06"),tz="UTC"), Returns=c(-10,20)))
  m <- analyze_performance(bt,initial_equity=100)
  expect_equal(m$total_pnl,10)
  expect_equal(m$total_return,0.1)
  expect_equal(m$max_dd,-0.1)
  expect_equal(m$max_trade_dd,-0.1)
  expect_equal(m$period_days,2)
})

test_that("IS/OOS reports trade counts and duration-adjusted profit consistently", {
  b <- list(results=data.table(Order_ID=c(1,1,2),Returns=c(10,-2,12)))
  scores <- data.table(k=1:2, net_profit=c(600,300), profit_factor=c(2,3), sharpe=c(1,2),sortino=c(1,2))
  obj <- list(optimization_history=list("2025-01-01"=scores),test_results=list(b),
    windows=data.table(train_start=as.Date("2024-01-01"),train_end=as.Date("2024-01-31"),
                      test_start=as.Date("2024-01-31"),test_end=as.Date("2024-02-02")),
    test_metrics=list(data.table(net_profit=20,profit_factor=Inf,sharpe=0.5,sortino=NA_real_)))
  d <- compareISOS(obj,"sharpe")$performance_comparison
  expect_equal(d$IS_NetProfit,300)
  expect_equal(d$OOS_NumTrades,2)
  expect_equal(d$OOS_WinRate,1)
  expect_equal(d$NetProfit_Degradation,0)
  expect_equal(d$Raw_NetProfit_Degradation,(300-20)/300)
  expect_equal(d$OOS_Sharpe,0.5)
})

test_that("optimisation accepts an explicit callback and enforces window bounds", {
  t <- as.POSIXct("2020-01-01",tz="UTC")
  p <- data.table(date=t+0:499*86400)
  strategy <- function(price,k) {
    stopifnot(!is.null(attr(price,"execution_end")))
    list(results=data.table(Order_ID=1,EntryTime=price$date[1],ExitTime=price$date[2],Returns=k))
  }
  o <- optimizeStrategy(p,data.frame(k=c(1,2)),train_set_weeks=26,test_set_weeks=26,strategy=strategy,verbose=FALSE)
  expect_equal(nrow(o$windows),2)
  expect_true(all(vapply(o$test_results,function(b)b$results$Returns,numeric(1))==2))
  expect_equal(o$optimization_history[[1]]$net_profit,c(1,2))
  bad <- function(price,k) list(results=data.table(EntryTime=price$date[1],ExitTime=attr(price,"execution_end")+1,Returns=k))
  expect_error(optimizeStrategy(p,data.frame(k=1),train_set_weeks=26,test_set_weeks=26,strategy=bad,verbose=FALSE),"escaped")
})

test_that("plotted trade counts aggregate legs and unresolved prices are rejected", {
  t <- as.POSIXct("2025-01-01",tz="UTC")
  b <- list(results=data.table(Order_ID=c(1,1,2),Returns=c(10,-2,-3),ExitTime=t+1:3*86400))
  r <- TradeTesterR:::.plot_results(b,aggregate=TRUE)
  expect_equal(r$Returns,c(8,-3))
  table <- plotly::plotly_build(plotPerformanceTable(b,100))
  values <- table$x$data[[1]]$cells$values
  expect_equal(as.character(values[[2]][2]),"2")
  expect_equal(as.character(values[[2]][1]),"5.00")
  expect_silent(plotly::plotly_build(plotWinLossPie(b)))
  b$results$Returns[1] <- NA_real_
  expect_error(plotCumReturns(b),"unknown/nonfinite")
})
