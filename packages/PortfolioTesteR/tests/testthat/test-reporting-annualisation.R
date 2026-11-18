# Explicit small ledgers make elapsed-time and warmup expectations independent
# of the reporter's implementation and avoid introducing a strategy fixture.
report_fixture <- function(days=c(0,28,56,84), values=c(100,110,99,120), start=1L) {
  dates <- as.Date('2020-01-01')+days
  n <- length(values)
  structure(list(name='report fixture',dates=dates,portfolio_values=values,
    first_trade_idx=start,first_trade_date=dates[start],initial_capital=100,
    start_date=dates[1],end_date=tail(dates,1),n_periods=n,warmup_periods=start-1L,
    active_periods=n-start+1L,final_value=tail(values,1),
    total_return=tail(values,1)/values[start]-1,total_return_with_warmup=tail(values,1)/100-1,
    returns=c(0,diff(values)/head(values,-1)),active_returns=c(0,if(start<n)diff(values[start:n])/head(values[start:n],-1)),
    transactions=data.table::data.table(Date=dates[c(1,n)],Shares=c(1,-1),Value=c(100,-120)),
    positions=data.table::data.table(Date=dates,A=rep(1,n)),cash=rep(0,n),n_transactions=2L),
    class=c('backtest_result','list'))
}

test_that('elapsed four-week reporting agrees across interfaces without changing its object', {
  x <- report_fixture(); before <- serialize(x,NULL)
  m <- backtest_metrics(x)
  r <- c(.1,-.1,120/99-1)
  expect_equal(m$annualized_return,1.2^(365.25/84)-1)
  expect_equal(m$annual_purchase_turnover,100/107.25/(84/365.25))
  expect_equal(m$volatility,sd(r)*sqrt(365.25/28))
  expect_equal(m$sharpe_ratio,m$annualized_return/m$volatility)
  expect_equal(m$annualisation$return_intervals,3L)
  expect_equal(m$annualisation$frequency_source,'observed intervals / elapsed years')
  p <- capture.output(ret <- print(x)); s <- capture.output(ret2 <- summary(x))
  expect_true(any(grepl(sprintf('Annualized Return: %.2f%%',100*m$annualized_return),p,fixed=TRUE)))
  expect_true(any(grepl(sprintf('Annual purchase turnover (full period): %.1f%%',100*m$annual_purchase_turnover),s,fixed=TRUE)))
  expect_true(any(grepl('Includes initial purchases',s,fixed=TRUE)))
  expect_identical(ret,x);expect_identical(ret2,x)
  expect_identical(serialize(x,NULL),before)
})

test_that('daily weekly and four-week intervals affect reports but not totals', {
  m <- lapply(c(1,7,28),function(d)backtest_metrics(report_fixture(days=(0:3)*d)))
  expect_equal(vapply(m,function(z)z$total_return,0),rep(.2,3))
  expect_equal(vapply(m,function(z)z$annualisation$periods_per_year,0),365.25/c(1,7,28))
  expect_equal(m[[1]]$annual_purchase_turnover/m[[2]]$annual_purchase_turnover,7)
  expect_equal(m[[2]]$annual_purchase_turnover/m[[3]]$annual_purchase_turnover,4)
  expect_equal(m[[2]]$volatility/m[[3]]$volatility,2)
})

test_that('warmup uses active elapsed intervals for risk and full dates for turnover', {
  x <- report_fixture(days=c(0,28,56,84,112),values=c(100,100,110,99,120),start=2L)
  expect_no_error(m <- backtest_metrics(x))
  expect_equal(m$annualized_return,1.2^(365.25/84)-1)
  expect_equal(m$annual_purchase_turnover,100/105.8/(112/365.25))
  expect_equal(m$annualisation$active_start,x$dates[2])
  expect_equal(m$annualisation$full_start,x$dates[1])
  expect_equal(m$volatility,sd(c(.1,-.1,120/99-1))*sqrt(365.25/28))
})

test_that('irregular spacing is exposed and override changes only risk scaling', {
  x <- report_fixture(days=c(0,1,21,84)); auto <- backtest_metrics(x)
  explicit <- backtest_metrics(x,periods_per_year=13)
  expect_true(auto$annualisation$unequal_intervals)
  expect_equal(auto$annualisation$periods_per_year,3/(84/365.25))
  expect_equal(explicit$annualisation$frequency_source,'caller supplied')
  expect_equal(explicit$annualisation$periods_per_year,13)
  expect_equal(explicit$annualized_return,auto$annualized_return)
  expect_equal(explicit$annual_purchase_turnover,auto$annual_purchase_turnover)
  expect_equal(explicit$volatility,sd(c(.1,-.1,120/99-1))*sqrt(13))
  p <- capture.output(print(x,periods_per_year=13))
  expect_true(any(grepl('caller supplied',p,fixed=TRUE)))
  expect_true(any(grepl('unequal intervals: approximation',p,fixed=TRUE)))
})

test_that('single valuation, one interval and zero volatility are explicitly unavailable', {
  one <- backtest_metrics(report_fixture(days=0,values=100))
  expect_true(is.na(one$annualized_return));expect_true(is.na(one$annual_purchase_turnover))
  expect_true(is.na(one$volatility));expect_true(is.na(one$sharpe_ratio))
  two <- backtest_metrics(report_fixture(days=c(0,28),values=c(100,110)))
  expect_equal(two$annualized_return,1.1^(365.25/28)-1)
  expect_true(is.na(two$volatility))
  flat <- report_fixture(values=rep(100,4)); flat$transactions <- flat$transactions[0]
  m <- backtest_metrics(flat)
  expect_equal(m$annualized_return,0);expect_equal(m$volatility,0)
  expect_equal(m$annual_purchase_turnover,0);expect_true(is.na(m$sharpe_ratio))
  expect_true(is.na(m$sortino_ratio))
})

test_that('invalid reporting overrides and dates do not silently invent a scale', {
  x <- report_fixture()
  for(rate in list(0,-1,NA_real_,Inf,c(12,13),'13',TRUE))
    expect_error(backtest_metrics(x,periods_per_year=rate),'finite positive')
  for(ds in list(c(0,0,28,56),c(0,28,21,56),c(0,28,NA,56))) {
    bad <- x;bad$dates <- as.Date('2020-01-01')+ds
    expect_error(backtest_metrics(bad),'strictly increasing')
  }
})
