library(data.table)

opening_feed <- function(step=300) {
  t <- as.POSIXct('2025-01-06 10:00:00',tz='UTC')+0:7*step
  data.table(date=t,Open.a=100,High.a=100.1,Low.a=100,Close.a=100,
    Open.b=99.75,High.b=99.85,Low.b=99.75,Close.b=99.75,
    OrderSize=0,EntryTime=t,EntryPrice=100,EntryType='Market',
    StopLoss=NA_real_,TakeProfit=NA_real_)
}
opening_bt <- function(d,...) backtest(d,CloseTradeOnOppositeSignal=FALSE,verbose=FALSE,...)

test_that('limits and targets improve only at eligible openings on both sides', {
  for(side in c(1,-1)) for(step in c(1,60,900)) {
    d <- opening_feed(step)
    d[1,`:=`(OrderSize=side*2,EntryType='Limit',EntryTime=d$date[2],
      EntryPrice=if(side>0)102 else 98,StopLoss=if(side>0)90 else 110)]
    b <- opening_bt(d)
    expect_equal(b$results$EntryPrice,if(side>0)100 else 99.75)
    expect_equal(b$orders$EntryPrice,if(side>0)102 else 98)
    expect_equal(b$results$StopLoss,if(side>0)90 else 110)
    for(dynamic in c(FALSE,TRUE)) {
      d <- opening_feed(step);d[1,OrderSize:=side*2]
      level <- if(side>0)103 else 97
      if(dynamic) {
        col <- if(side>0)'targetorder_long_1' else 'targetorder_short_1'
        d[,NextOpenTime:=date+step];d[,(col):=level];d[,(paste0(col,'_amount')):=.5]
      } else d[1,TakeProfit:=level]
      op <- if(side>0)105 else 95
      d[3,`:=`(Open.a=op,High.a=op+.1,Low.a=op,Close.a=op,
        Open.b=op-.25,High.b=op-.15,Low.b=op-.25,Close.b=op-.25)]
      b <- opening_bt(d)
      expect_equal(b$results$ExitTime[1],d$date[3])
      expect_equal(b$results$ExitPrice[1],if(side>0)104.75 else 95)
      expect_equal(sum(b$results$ExitAmount),-side*2)
      expect_equal(b$results$ExitAmount[1],-side*if(dynamic)1 else 2)
    }
  }
})

test_that('closing-side stop triggers cannot wait for the opposite quote', {
  for(side in c(1,-1)) for(dynamic in c(FALSE,TRUE)) for(at_open in c(TRUE,FALSE)) {
    d <- opening_feed();d[1,OrderSize:=side]
    level <- 99.9
    # Keep the entry bar away from the stop; expose just the closing-side quote
    # on bar 3 and a much worse gap on bar 4.
    if(side>0) d[1:2,`:=`(Open.a=101,High.a=101.1,Low.a=101,Close.a=101,
      Open.b=100.75,High.b=100.85,Low.b=100.75,Close.b=100.75)] else
      d[1:2,`:=`(Open.a=99,High.a=99.1,Low.a=99,Close.a=99,
        Open.b=98.75,High.b=98.85,Low.b=98.75,Close.b=98.75)]
    if(!at_open) {
      if(side>0) d[3,`:=`(Open.a=100.2,High.a=100.3,Open.b=99.95,High.b=100.05)] else
        d[3,`:=`(Open.a=99.8,Low.a=99.8,Open.b=99.55,Low.b=99.55)]
    }
    if(dynamic) {
      col <- if(side>0)'stoporder_long_1' else 'stoporder_short_1'
      d[,NextOpenTime:=date+300];d[,(col):=level];d[,(paste0(col,'_amount')):=1]
    } else d[1,StopLoss:=level]
    op <- if(side>0)90 else 110
    d[4,`:=`(Open.a=op,High.a=op+.1,Low.a=op,Close.a=op,
      Open.b=op-.25,High.b=op-.15,Low.b=op-.25,Close.b=op-.25)]
    b <- opening_bt(d)
    expect_equal(b$results$ExitTime,d$date[3])
    expect_equal(b$results$ExitPrice,if(at_open){if(side>0)99.75 else 100}else level)
  }
})

test_that('off-grid fallback includes the first bar but never overrides explicit bounds', {
  d <- opening_feed();d[1,`:=`(OrderSize=1,EntryTime=date+420)]
  b <- opening_bt(d)
  expect_equal(b$results$EntryTime,d$date[3])
  expect_equal(b$orders$ActivationTime,d$date[1]+420)
  expect_equal(b$orders$EntryValidUntil,d$date[4])
  for(bound in c('strategy','explicit','ttl','cancel','horizon')) {
    x <- copy(d)
    if(bound=='strategy') {x[,NextOpenTime:=as.POSIXct(NA,tz='UTC')];x[c(1,5),NextOpenTime:=date+600]}
    if(bound=='explicit') x[1,EntryValidUntil:=d$date[3]]
    if(bound=='cancel') x[3,CancelOrder:=TRUE]
    args <- if(bound=='ttl')list(TradeTimeLimit=lubridate::seconds(180)) else
      if(bound=='horizon')list(end_time=d$date[3]) else list()
    b <- do.call(opening_bt,c(list(x),args))
    expect_equal(nrow(b$results),0,info=bound)
  }
  x <- d[-3];x[,NextOpenTime:=as.POSIXct(NA,tz='UTC')]
  x[c(1,4),NextOpenTime:=date+600]
  expect_equal(nrow(opening_bt(x)$results),0)
  x <- copy(d);x[1,EntryTime:=tail(d$date,1)+600]
  expect_equal(nrow(opening_bt(x)$results),0)
})

test_that('opposite signal semantics do not require an executable opposite entry', {
  d <- opening_feed();d[1,OrderSize:=1]
  d[3,`:=`(OrderSize=-1,EntryPrice=NA_real_)]
  b <- backtest(d,verbose=FALSE)
  expect_equal(b$orders$Disposition,c('filled','invalid_instruction'))
  expect_equal(b$results$ExitReason,'Opposite signal')
  expect_equal(b$results$ExitTime,d$date[3])
  expect_equal(opening_bt(d)$results$ExitReason,'End of data')
})
