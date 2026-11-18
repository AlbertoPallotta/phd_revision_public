# Delivery support.
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==5L)
source(args[1]);prepared<-prepare_figure31(args[2])
stamp<-function(x)as.POSIXct(x,format="%Y-%m-%d %H:%M:%S",tz="UTC")
oldp<-read.csv(args[3]);oldp$date<-stamp(oldp$date)
oldb<-read.csv(args[4]);oldb$date<-stamp(oldb$date);oldb$NextOpenTime<-stamp(oldb$NextOpenTime)
stopifnot(identical(as.numeric(prepared$prices$date),as.numeric(oldp$date)),
          identical(as.numeric(prepared$bands$date),as.numeric(oldb$date)),
          identical(as.numeric(prepared$bands$NextOpenTime),as.numeric(oldb$NextOpenTime)),
          max(abs(prepared$prices$Close-oldp$Close))<1e-12,
          max(abs(prepared$bands$lowerBB-oldb$lowerBB))<1e-9,
          max(abs(prepared$bands$upperBB-oldb$upperBB))<1e-9)
stopifnot(!file.exists(args[5]));writeLines(c("PASS: new reconstruction versus frozen plotted values",capture.output(sessionInfo())),args[5])
