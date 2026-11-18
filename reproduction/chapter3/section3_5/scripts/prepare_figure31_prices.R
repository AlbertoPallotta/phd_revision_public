# Delivery support. Preparation/indicator calculation, never a backtest.
# source(..., local=env); call prepare_figure31(csv_path) for in-memory plot data.
prepare_figure31 <- function(csv_path) {
  stopifnot(requireNamespace("data.table", quietly=TRUE),
            requireNamespace("xts", quietly=TRUE),requireNamespace("TTR",quietly=TRUE))
  suppressPackageStartupMessages(library(data.table))
  raw <- fread(csv_path,colClasses=c(open_time="character"))
  setnames(raw,c("date","Open","High","Low","Close","Volume"))
  raw[,date:=as.POSIXct(date,format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")]
  stopifnot(!anyNA(raw$date),!anyDuplicated(raw$date));setorder(raw,date)
  # Preserve all observations from the frozen start for the indicator warm-up.
  # Daily aggregation and TTR BBands settings are those in historical F0017.
  ask <- xts::xts(as.matrix(raw[,.(Open,High,Low,Close)]),order.by=raw$date)
  dx <- xts::to.period(ask,period="days",k=1,indexAt="startof",name=NULL,drop.time=FALSE)
  daily <- data.table(date=zoo::index(dx),zoo::coredata(dx));setkey(daily,date)
  daily[,NextOpenTime:=shift(date,type="lead")]
  bb <- TTR::BBands(daily[,.(High,Low,Close)],n=20,sd=2)
  daily[,`:=`(lowerBB=bb[,"dn"],upperBB=bb[,"up"])]
  start <- as.POSIXct("2016-02-06 00:00:00",tz="UTC")
  end <- as.POSIXct("2016-03-07 00:00:00",tz="UTC")
  prices <- raw[date>=start & date<=end,.(date,Close)]
  bands <- daily[!is.na(NextOpenTime) & NextOpenTime>=start-3*86400 & NextOpenTime<=end,
                 .(date,NextOpenTime,lowerBB,upperBB)]
  stopifnot(nrow(prices)==1829L,nrow(bands)==29L)
  list(prices=as.data.frame(prices),bands=as.data.frame(bands))
}
# Future local comparison: every timestamp exactly, every Close within 1e-12,
# lowerBB/upperBB within absolute 1e-9, vs F0063/F0058 respectively.
# F0058 saved bands and F0063 observed prices are LOCAL COMPARISON ONLY.
# Neither is uploaded or read by the delivery plotter; originals stay unchanged.
# No reconstructed market-price CSV is persisted or added to the delivery.
