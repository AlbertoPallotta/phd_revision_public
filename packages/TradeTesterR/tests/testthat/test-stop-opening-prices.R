library(data.table)

stop_open_feed <- function(step=60) {
  t <- as.POSIXct("2025-01-06 10:00:00",tz="UTC")+0:7*step
  data.table(date=t,Open.a=100,High.a=101,Low.a=99,Close.a=100,
    Open.b=99.75,High.b=100.75,Low.b=98.75,Close.b=99.75,
    OrderSize=0,EntryTime=t,EntryPrice=100,EntryType="Market",
    StopLoss=NA_real_,TakeProfit=NA_real_,EntryValidUntil=tail(t,1)+step)
}
stop_open_bar <- function(d,row,open,high=open+1,low=open-1,close=open) {
  d[row,`:=`(Open.a=open,High.a=high,Low.a=low,Close.a=close,
    Open.b=open-.25,High.b=high-.25,Low.b=low-.25,Close.b=close-.25)]
}
stop_open_bt <- function(d,...) backtest(d,CloseTradeOnOppositeSignal=FALSE,
                                       verbose=FALSE,...)

test_that("crossed stop entries use opening quotes and retain instruction levels", {
  for(side in c(1,-1)) for(step in c(1,60,900)) {
    d <- stop_open_feed(step)
    threshold <- if(side>0) 102 else 98
    d[1,`:=`(OrderSize=side*200,EntryType="Stop",EntryPrice=threshold,
             EntryTime=d$date[2],StopLoss=threshold-side*10,
             TakeProfit=threshold+side*10)]
    stop_open_bar(d,2,if(side>0)104 else 96)
    b <- stop_open_bt(d)
    expected <- if(side>0)104 else 95.75
    expect_equal(b$results$EntryPrice,expected)
    expect_equal(b$orders$EntryPrice,threshold)
    expect_equal(b$results$StopLoss,threshold-side*10)
    expect_equal(b$results$TakeProfit,threshold+side*10)
    expect_equal(b$results$EntryTime,d$date[2])
    expect_equal(b$results$Returns,
                 side*(b$results$ExitPrice-expected)*200)
    expect_equal(sum(b$results$ExitAmount),-side*200)
  }
})

test_that("ordinary intrabar stop entries retain threshold prices", {
  for(side in c(1,-1)) {
    d <- stop_open_feed()
    threshold <- if(side>0)102 else 98
    d[1,`:=`(OrderSize=side,EntryType="Stop",EntryPrice=threshold,
             EntryTime=d$date[2])]
    stop_open_bar(d,2,100,high=103,low=97)
    b <- stop_open_bt(d)
    expect_equal(b$results$EntryPrice,threshold)
    expect_equal(b$results$EntryTime,d$date[2])
  }
})

test_that("existing fixed stops use the closing-side quote at a crossed opening", {
  for(side in c(1,-1)) {
    d <- stop_open_feed()
    d[1,`:=`(OrderSize=side*2,EntryPrice=if(side>0)100 else 99.75,
             StopLoss=if(side>0)98 else 102)]
    stop_open_bar(d,3,if(side>0)96 else 104)
    b <- stop_open_bt(d)
    expect_equal(b$results$ExitTime,d$date[3])
    expect_equal(b$results$ExitPrice,if(side>0)95.75 else 104)
    expect_equal(b$results$ExitReason,"StopLoss")
    expect_false(b$results$AmbiguousBar)
  }
})

test_that("an earlier intrabar stop is not postponed to a worse later opening", {
  d <- stop_open_feed(); d[1,`:=`(OrderSize=1,StopLoss=98)]
  stop_open_bar(d,2,100,low=97,close=97.5)
  stop_open_bar(d,3,96)
  b <- stop_open_bt(d)
  expect_equal(b$results$ExitTime,d$date[2])
  expect_equal(b$results$ExitPrice,98)
})

test_that("dynamic replacements activate on schedule and execute crossed openings", {
  for(side in c(1,-1)) {
    d <- stop_open_feed(); d[1,OrderSize:=side*2]
    col <- if(side>0) "stoporder_long_1" else "stoporder_short_1"
    d[,`:=`(NextOpenTime=date+60,NextOpen=100)]
    d[,(col):=if(side>0)90 else 110]
    d[,(paste0(col,"_amount")):=1]
    # The old stop survives the prior bar; only its replacement is crossed.
    stop_open_bar(d,2,if(side>0)98 else 102)
    d[2,(col):=100]
    stop_open_bar(d,3,if(side>0)98 else 102)
    b <- stop_open_bt(d)
    expect_equal(b$results$ExitTime,d$date[3])
    expect_equal(b$results$ExitPrice,if(side>0)97.75 else 102)
    expect_match(b$results$ExitReason,"Trailing stop")
    expect_equal(b$results$CurrentPosSize,0)
  }
})

test_that("fixed protection remains active before a dynamic replacement", {
  d <- stop_open_feed(); d[1,`:=`(OrderSize=1,StopLoss=98)]
  d[,`:=`(NextOpenTime=date+60,NextOpen=100,
          stoporder_long_1=90,stoporder_long_1_amount=1)]
  stop_open_bar(d,2,96)
  d[3,stoporder_long_1:=100]
  b <- stop_open_bt(d)
  expect_equal(b$results$ExitTime,d$date[2])
  expect_equal(b$results$ExitPrice,95.75)
  expect_equal(b$results$ExitReason,"StopLoss")
})

test_that("an opening fixed exit precedes the opposite bracket's later touch", {
  for(side in c(1,-1)) for(open_stop in c(TRUE,FALSE)) {
    d <- stop_open_feed()
    d[1,`:=`(OrderSize=side,StopLoss=100-side*2,TakeProfit=100+side*2)]
    opening <- 100+if(open_stop)-side*4 else side*4
    stop_open_bar(d,3,opening,high=106,low=94)
    b <- stop_open_bt(d) # default unknown applies only if the phase is unresolved
    expect_false(b$results$AmbiguousBar)
    expect_equal(b$results$ExitReason,if(open_stop)"StopLoss" else "TakeProfit")
    expected <- if(side>0) opening-.25 else opening
    expect_equal(b$results$ExitPrice,expected)
  }
})

test_that("an intrabar entry cannot use that bar's earlier opening for protection", {
  for(side in c(1,-1)) for(dynamic in c(FALSE,TRUE)) {
    d <- stop_open_feed(); level <- if(side>0)102 else 98
    d[1,`:=`(OrderSize=side,EntryType="Stop",EntryPrice=100+side*5,
             EntryTime=d$date[2])]
    if(dynamic) {
      col <- if(side>0) "stoporder_long_1" else "stoporder_short_1"
      d[,NextOpenTime:=date]
      d[,(col):=level]; d[,(paste0(col,"_amount")):=1]
    } else d[1,StopLoss:=level]
    stop_open_bar(d,2,100,high=106,low=94)
    b <- stop_open_bt(d)
    expect_equal(b$results$ExitTime,d$date[2])
    expect_equal(b$results$ExitPrice,level)
  }
})

test_that("partial dynamic stops conserve the remaining quantity at opening prices", {
  d <- stop_open_feed(); d[1,OrderSize:=4]
  d[,`:=`(NextOpenTime=date+60,NextOpen=100,
          stoporder_long_1=98,stoporder_long_1_amount=.5)]
  stop_open_bar(d,3,96)
  b <- stop_open_bt(d)
  expect_equal(b$results$ExitPrice[1],95.75)
  expect_equal(b$results$ExitAmount,c(-2,-2))
  expect_equal(b$results$CurrentPosSize,c(2,0))
  expect_equal(sum(b$results$Returns),sum((b$results$ExitPrice-100)*2))
})

test_that("opening dynamic orders precede touches with legacy same-phase ties", {
  for(open_stop in c(TRUE,FALSE)) {
    d <- stop_open_feed(); d[1,OrderSize:=1]
    d[,`:=`(NextOpenTime=date+60,NextOpen=100,
      stoporder_long_1=98,stoporder_long_1_amount=1,
      targetorder_long_1=102,targetorder_long_1_amount=1)]
    stop_open_bar(d,3,if(open_stop)96 else 104,high=106,low=94)
    b <- stop_open_bt(d)
    expect_match(b$results$ExitReason,if(open_stop)"Trailing stop" else "Trailing Target")
    expect_equal(b$results$ExitPrice,if(open_stop)95.75 else 103.75)
  }
})

test_that("crossed limits improve at the opening and stop cancellation is unchanged", {
  d <- stop_open_feed(); d[1,`:=`(OrderSize=1,EntryType="Limit",EntryPrice=98,
                                EntryTime=d$date[2])]
  stop_open_bar(d,2,96)
  expect_equal(stop_open_bt(d)$results$EntryPrice,96)
  d[1,`:=`(EntryType="Stop",EntryPrice=102,EntryValidUntil=d$date[3])]
  stop_open_bar(d,3,104)
  b <- stop_open_bt(d)
  expect_equal(nrow(b$results),0)
  expect_equal(b$orders$Disposition,"expired_entry_bar")
})

test_that("a protective opening exit does not admit entry until the following bar", {
  d <- stop_open_feed(); d[c(1,3,4),OrderSize:=1]
  d[1,StopLoss:=98]
  stop_open_bar(d,3,96)
  b <- stop_open_bt(d)
  expect_equal(b$orders$Disposition,c("filled","ignored_holding","filled"))
  expect_equal(b$results$Order_ID,c(1L,3L))
  expect_equal(b$results$ExitPrice[1],95.75)
  expect_equal(b$results$EntryTime[2],d$date[4])
})
