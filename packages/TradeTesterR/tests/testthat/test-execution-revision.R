library(data.table)

revision_feed <- function(n=12,step=300) {
  t <- as.POSIXct('2025-01-06 10:00:00',tz='UTC')+(0:(n-1))*step
  data.table::data.table(date=t,Open.a=100,High.a=101,Low.a=99,Close.a=100,
    Open.b=100,High.b=101,Low.b=99,Close.b=100,OrderSize=0,
    EntryTime=t,EntryPrice=100,EntryType='Market',TakeProfit=105,StopLoss=70,
    EntryValidUntil=t[1]+n*step)
}
revision_bt <- function(d,...) backtest(d,CloseTradeOnOppositeSignal=FALSE,verbose=FALSE,...)

test_that('future fills cannot remove completed earlier trades',{
  d <- revision_feed()
  d[1,`:=`(OrderSize=1,EntryType='Limit',EntryPrice=90,TakeProfit=95)]
  d[2,OrderSize:=1]
  d[3,`:=`(High.a=106,High.b=106)]
  d[5,`:=`(Low.a=89,Low.b=89)]
  a <- revision_bt(d[1:4]); b <- revision_bt(d)
  expect_equal(a$results$Order_ID,2L); expect_equal(b$results$Order_ID,2L)
  expect_equal(a$results$Returns,5); expect_equal(b$results$Returns,5)
  expect_equal(b$orders$Disposition,c('cancelled_competing_fill','filled'))
  expect_equal(a$orders,b$orders)
})

test_that('opening fills beat later touches and unresolved ties are visible',{
  d <- revision_feed()
  d[1,`:=`(OrderSize=1,EntryType='Limit',EntryPrice=95,TakeProfit=98,EntryTime=d$date[2])]
  d[3,`:=`(OrderSize=1,EntryTime=d$date[4])]
  d[4,`:=`(High.a=106,High.b=106,Low.a=94,Low.b=94)]
  b <- revision_bt(d)
  expect_equal(b$results$Order_ID,2L); expect_false(b$results$AmbiguousEntry)
  d[3,`:=`(EntryType='Stop',EntryPrice=105,TakeProfit=110)]
  b <- revision_bt(d)
  expect_equal(b$results$Order_ID,1L); expect_true(b$results$AmbiguousEntry)
  expect_equal(b$orders[Disposition=='filled',CompetingOrderIDs],'2')
})

test_that('intrabar closure does not admit an instruction at that bar opening',{
  d <- revision_feed(); d[1,OrderSize:=1]; d[3,OrderSize:=1]
  d[3,`:=`(High.a=106,High.b=106)]
  d[4,OrderSize:=1]
  b <- revision_bt(d)
  expect_equal(b$results$Order_ID,c(1L,3L))
  expect_equal(b$orders$Disposition,c('filled','ignored_holding','filled'))
})

test_that('entry expiry follows four-hour and weekly clocks without CancelOrder',{
  for(step in c(4*3600,7*86400)) for(type in c('Limit','Stop')) {
    d <- revision_feed(n=49,step=step/12)
    d[,EntryValidUntil:=NULL]
    d[,NextOpenTime:=as.POSIXct(NA,tz='UTC')]
    marks <- c(1,13,25,37)
    d[marks,NextOpenTime:=d$date[marks+12]]
    d[1,`:=`(OrderSize=1,EntryTime=d$date[13],EntryType=type,
      EntryPrice=if(type=='Limit')90 else 110)]
    hitcols <- if(type=='Limit') c('Low.a','Low.b') else c('High.a','High.b')
    d[30,(hitcols):=if(type=='Limit')89 else 111]
    b <- revision_bt(d)
    expect_equal(nrow(b$results),0L)
    expect_equal(b$orders$Disposition,'expired_entry_bar')
    expect_equal(b$orders$DispositionTime,d$date[25])
    d[18,(hitcols):=if(type=='Limit')89 else 111]
    b <- revision_bt(d)
    expect_equal(b$results$EntryTime,d$date[18])
  }
})

test_that('fixed protection survives expiry and callbacks may omit remaining size',{
  d <- revision_feed()
  d[1,`:=`(OrderSize=1,EntryValidUntil=d$date[2])]
  d[8,`:=`(High.a=106,High.b=106)]
  b <- revision_bt(d)
  expect_equal(b$results$ExitTime,d$date[8]); expect_equal(b$results$Returns,5)
  callback <- function(x) data.table::data.table(ExitTime=x$date[2],ExitAmount=-1,ExitPrice=101)
  b <- revision_bt(d,exit_fun=callback)
  expect_equal(b$results$CurrentPosSize,0)
})

revision_gap <- function() {
  fri <- as.POSIXct('2019-01-04 20:00:00',tz='UTC')+(0:7)*900
  sun <- as.POSIXct('2019-01-06 23:00:00',tz='UTC')+(0:3)*900
  d <- revision_feed(); d[,`:=`(date=c(fri,sun),EntryTime=c(fri,sun),EntryValidUntil=sun[4]+900,
    TakeProfit=NA_real_,StopLoss=NA_real_,NextOpen=NA_real_,
    NextOpenTime=as.POSIXct(NA,tz='UTC'),exit_long=0,CLOSE_CANDLES_SINCE_ENTRY=1L)]
  d[1,`:=`(OrderSize=1,EntryTime=fri[2])]
  d[5,`:=`(NextOpen=110,NextOpenTime=sun[1],exit_long=1)]
  d[9,`:=`(Open.a=110,High.a=111,Low.a=109,Close.a=110,
    Open.b=110,High.b=111,Low.b=109,Close.b=110,NextOpen=100,NextOpenTime=sun[2])]
  d
}
test_that('valid next-session exits survive dynamic merges and boundaries exclude their prices',{
  d <- revision_gap(); b <- revision_bt(d)
  expect_equal(b$results$ExitTime,d$date[9]); expect_equal(b$results$Returns,10)
  d[,`:=`(targetorder_long_1=1000,targetorder_long_1_amount=.5)]
  b <- revision_bt(d)
  expect_equal(b$results$ExitTime,d$date[9]); expect_equal(b$results$Returns,10)
  b <- revision_bt(d,end_time=d$date[9])
  expect_equal(b$results$ExitTime,d$date[8]+900); expect_equal(b$results$Returns,0)
  expect_equal(b$results$ExitReason,'Execution horizon')
  bad <- copy(d); bad[5,NextOpenTime:=as.POSIXct('2019-01-06 12:00:00',tz='UTC')]
  expect_error(revision_bt(bad),'observed opening')
  bad <- copy(d); bad[5,NextOpen:=777]
  expect_error(revision_bt(bad),'observed opening')
})

test_that('earlier protective exits precede later scheduled signal exits',{
  d <- revision_feed(); d[1,OrderSize:=1]
  d[,`:=`(NextOpenTime=as.POSIXct(NA,tz='UTC'),NextOpen=NA_real_,exit_long=0)]
  d[2,`:=`(NextOpenTime=d$date[8],NextOpen=100,exit_long=1)]
  d[4,`:=`(High.a=106,High.b=106)]
  b <- revision_bt(d)
  expect_equal(b$results$ExitTime,d$date[4]); expect_equal(b$results$ExitReason,'TakeProfit')
})

test_that('partial next-session signals cannot close the remainder retrospectively',{
  d <- revision_gap(); d[5,exit_long:=.5]
  b <- revision_bt(d)
  expect_equal(b$results$ExitTime,rep(d$date[9],2))
  expect_equal(b$results$ExitAmount,c(-.5,-.5))
  expect_equal(b$results$CurrentPosSize,c(.5,0))
})

test_that('zero trades retain instructions and zero instructions have a typed ledger',{
  d <- revision_feed(); d[1,`:=`(OrderSize=1,EntryType='Limit',EntryPrice=10)]
  b <- revision_bt(d)
  expect_equal(nrow(b$orders),1L); expect_equal(nrow(b$results),0L)
  d[,OrderSize:=0]; b <- revision_bt(d)
  expect_equal(nrow(b$orders),0L); expect_true(all(c('Returns','ExitTime') %in% names(b$results)))
})

test_that('Sortino measures downside around the target rather than the loss mean',{
  returns <- c(-.01,-.01,.03)
  t <- as.POSIXct('2025-01-01',tz='UTC')+0:2*86400
  equity <- 100*cumprod(1+returns)
  b <- list(results=data.table::data.table(Order_ID=1:3,ExitTime=t,Returns=diff(c(100,equity))))
  m <- analyze_performance(b,initial_equity=100)
  expect_equal(m$sortino,mean(returns)*sqrt(365)/sqrt(mean(pmin(returns,0)^2)))
})

test_that('one-second data and default-entry grammar are supported',{
  d <- revision_feed(step=1); d[,c('EntryTime','EntryPrice','EntryType','EntryValidUntil'):=NULL]
  d[1,OrderSize:=1]; d[5,`:=`(High.a=106,High.b=106)]
  b <- revision_bt(d)
  expect_equal(b$results$EntryTime,d$date[2]); expect_equal(b$results$Returns,5)
})

test_that('partial positions stay open and cancellation never removes protection',{
  d <- revision_feed(); d[c(1,4,9),OrderSize:=1]; d[,CancelOrder:=TRUE]
  callback <- function(x) data.table::data.table(ExitTime=x$date[c(2,6)],
    ExitAmount=c(-.5,-.5),ExitPrice=c(101,102))
  # The last instruction has a shorter callback so the test stays in range.
  callback <- function(x) data.table::data.table(ExitTime=x$date[c(2,min(6,nrow(x)))],
    ExitAmount=c(-.5,-.5),ExitPrice=c(101,102))
  b <- revision_bt(d,exit_fun=callback)
  expect_equal(b$orders$Disposition,c('filled','ignored_holding','filled'))
  expect_equal(b$results$CurrentPosSize,c(.5,0,.5,0))
})

test_that('legacy position options remain separate and preserve quantities',{
  for(add in c(FALSE,TRUE))for(opposite in c(FALSE,TRUE)) {
    d <- revision_feed();d[1,OrderSize:=1];d[3,OrderSize:=-1]
    b <- backtest(d,AddToPosition=add,CloseTradeOnOppositeSignal=opposite,verbose=FALSE)
    expect_equal(nrow(b$results),if(add||opposite)2L else 1L)
    expect_equal(b$results[,sum(ExitAmount)+first(OrderSize),by=Order_ID]$V1,
                 rep(0,nrow(b$results)))
    if(opposite)expect_equal(b$results$ExitTime[1],d$date[3])
  }
})

test_that('scoped optimisation bounds restore after nested calls and errors',{
  run <- TradeTesterR:::.with_execution_end
  d <- revision_feed();d[1,OrderSize:=1]
  b <- run(d$date[4],function()revision_bt(d),list())
  expect_equal(b$results$ExitTime,d$date[4])
  expect_true(all(b$data$date<d$date[4]))
  expect_error(run(d$date[3],function()stop('fixture'),list()),'fixture')
  expect_null(TradeTesterR:::.execution_context$end)
  b <- run(d$date[4],function()run(d$date[8],function()revision_bt(d),list()),list())
  expect_equal(b$results$ExitTime,d$date[4])
  expect_null(TradeTesterR:::.execution_context$end)
})

test_that('future data perturbations cannot change already closed trades',{
  set.seed(490)
  d <- revision_feed(n=40);d[c(1,8,16),OrderSize:=1]
  d[c(4,11,19),`:=`(High.a=106,High.b=106)]
  reference <- revision_bt(d)$results[ExitTime<d$date[20],.(Order_ID,EntryTime,ExitTime,Returns)]
  for(k in 1:10) {
    altered <- copy(d); altered[21:40,`:=`(High.a=runif(20,101,200),High.b=runif(20,101,200),
      Low.a=runif(20,1,99),Low.b=runif(20,1,99),OrderSize=sample(c(-1,0,1),20,TRUE))]
    got <- revision_bt(altered)$results[ExitTime<d$date[20],.(Order_ID,EntryTime,ExitTime,Returns)]
    expect_equal(got,reference)
  }
})

test_that('dynamic stop retains legacy priority over a same-bar dynamic target',{
  d <- revision_feed();d[1,OrderSize:=1];d[,TakeProfit:=200]
  d[,`:=`(NextOpenTime=date+300,NextOpen=100,targetorder_long_1=100.5,
    targetorder_long_1_amount=1,stoporder_long_1=99.5,stoporder_long_1_amount=1)]
  b <- revision_bt(d)
  expect_equal(b$results$ExitPrice,99.5)
  expect_match(b$results$ExitReason,'Trailing stop')
})

test_that('zero or negative equity does not produce misleading percentage risk metrics',{
  for(loss in c(-100,-150)) {
    b <- list(results=data.table::data.table(Order_ID=1:3,
      ExitTime=as.POSIXct('2025-01-01',tz='UTC')+0:2*86400,Returns=c(loss,-10,200)))
    m <- analyze_performance(b,initial_equity=100)
    expect_true(m$nonpositive_equity)
    expect_identical(tail(names(m),1),'nonpositive_equity')
    expect_true(all(is.na(unlist(m[,.(cagr,ann_vol,sharpe,sortino)]))))
    expect_equal(m$total_return,(loss-10+200)/100)
    expect_equal(m$period_days,3)
    expect_equal(m$max_dd,(loss-10)/100)
  }
})
