# Presentation only: reads derived CSVs. No package loading or strategy calls.
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
fig <- file.path(root, "figures")
blue <- "#245777"; rust <- "#964C32"; grey <- "#666666"
read <- function(name) read.csv(file.path(fig, name), check.names = FALSE)
draw <- function(stem, height, fun) {
  for (fmt in c("png", "pdf")) {
    file <- file.path(fig, paste0(stem, ".", fmt))
    if (fmt == "png") png(file, width=7.1, height=height, units="in", res=300,
                           type="cairo", bg="white")
    else cairo_pdf(file, width=7.1, height=height, family="serif")
    par(family="serif", mar=c(4.1,4.8,1.1,0.8), mgp=c(2.8,0.8,0),
        las=1, cex=1, col.axis="#222222", col.lab="#111111", bty="l")
    fun(); dev.off()
  }
}
moneyaxis <- function(at, side=2) axis(side, at=at, labels=format(at, big.mark=",", scientific=FALSE, trim=TRUE))
dates <- function(x) as.Date(x)
surface <- read("first-window-surface.csv")
draw("Figure_5_4_first_window_surface", 3.3, function() {
  par(mar=c(4,4.8,0.8,0.8))
  ns <- sort(unique(surface$atr_n)); ks <- sort(unique(surface$kTP))
  plot(NA, xlim=c(0.5,2.5), ylim=c(0.5,3.5), xaxs="i", yaxs="i", axes=FALSE,
       xlab="ATR lookback (daily observations)", ylab="Target multiplier", bty="n")
  pal <- colorRampPalette(c("#E3ECEF", "#EDC8BA", "#C77F62"))(101)
  severity <- (max(surface$net_profit)-surface$net_profit)/diff(range(surface$net_profit))
  for (i in seq_len(nrow(surface))) {
    x <- match(surface$atr_n[i],ns); y <- match(surface$kTP[i],ks)
    rect(x-.5,y-.5,x+.5,y+.5,col=pal[1+round(100*severity[i])],border="white",lwd=2)
    text(x,y,format(round(surface$net_profit[i]),big.mark=",",scientific=FALSE,trim=TRUE),cex=1.15)
  }
  i <- which.max(surface$net_profit);x <- match(surface$atr_n[i],ns);y <- match(surface$kTP[i],ks)
  rect(x-.48,y-.48,x+.48,y+.48,border=blue,lwd=2.5)
  axis(1,at=1:2,labels=ns,tick=FALSE);axis(2,at=1:3,labels=ks,tick=FALSE)
})
path <- read("atr-parameter-path.csv")
draw("Figure_5_5_parameter_path",3.2,function() {
  x <- dates(path$test_start)
  plot(x,path$atr_n,type="s",lwd=1.8,col=blue,ylim=c(9,21),yaxt="n",
       xlab="Start of OOS window (UTC date)",ylab="Selected ATR lookback")
  axis(2,at=c(10,14,20));points(x,path$atr_n,pch=19,cex=.65,col=blue)
})
cmp <- read("is-oos-comparison.csv")
draw("Figure_5_6_is_oos_comparison",5.8,function() {
  layout(matrix(c(1,2,3,3),2,2,byrow=TRUE),heights=c(1,1))
  par(mar=c(4.2,4.9,1.9,1),cex=.88,mgp=c(3,.8,0))
  xr <- range(cmp$IS_USD_per_day);yr <- range(cmp$OOS_USD_per_day)
  plot(cmp$IS_USD_per_day,cmp$OOS_USD_per_day,pch=19,col=blue,
       xlim=xr,ylim=yr,xlab="IS P&L (USD per day)",ylab="OOS P&L (USD per day)")
  abline(h=0,v=0,col="#C5C5C5",lty=3);abline(a=0,b=1,col=grey,lty=2)
  points(cmp$IS_USD_per_day,cmp$OOS_USD_per_day,pch=19,col=blue)
  title("(a) Paired rates",adj=0,line=.5,cex.main=1)
  x <- dates(cmp$test_start)
  plot(x,cmp$OOS_USD_per_day,type="b",pch=19,lwd=1.2,cex=.5,col=rust,
       ylim=range(c(cmp$IS_USD_per_day,cmp$OOS_USD_per_day)),
       xlab="Start of OOS window (UTC)",ylab="P&L (USD per day)")
  abline(h=0,col="#999999",lty=3)
  lines(x,cmp$IS_USD_per_day,type="b",pch=1,lwd=1.2,cex=.5,col=blue)
  title("(b) Rates across windows",adj=0,line=.5,cex.main=1)
  legend("bottomleft",legend=c("IS","OOS"),col=c(blue,rust),lty=1,pch=c(1,19),bty="n",horiz=TRUE,cex=.9)
  par(mar=c(4,4.9,1.9,1))
  hist(cmp$degradation_percent,breaks=seq(-1000,6000,by=500),col="#CBD9E2",border="white",
       main="",xlab="Relative degradation (%)",ylab="Number of windows",xaxt="n")
  moneyaxis(seq(-1000,6000,1000),1);abline(v=0,lty=2,col=grey)
  title("(c) Relative degradation across all 24 windows",adj=0,line=.5,cex.main=1)
})
oos <- read("oos-cumulative-pnl.csv")
draw("Figure_5_7_oos_cumulative_pnl",3.5,function() {
  par(mar=c(4.1,5.8,1,0.8),mgp=c(3.8,.8,0))
  plot(dates(oos$date),oos$pnl_usd,type="s",col=blue,lwd=1.5,yaxt="n",
       xlab="",ylab="Cumulative realised P&L (USD)")
  mtext("UTC exit date",side=1,line=2.6)
  moneyaxis(pretty(range(oos$pnl_usd)));abline(h=0,col="#999999",lty=3)
})
full <- read("full-history-pnl.csv")
draw("Figure_5_8_full_history_comparison",3.8,function() {
  par(mar=c(4.1,5.8,1,0.8),mgp=c(3.8,.8,0))
  plot(dates(full$date),full$base_usd,type="s",col=blue,lwd=1.4,yaxt="n",
       ylim=range(c(full$base_usd,full$sma200_usd)),
       xlab="",ylab="Cumulative realised P&L (USD)")
  mtext("UTC exit date",side=1,line=2.6)
  lines(dates(full$date),full$sma200_usd,type="s",col=rust,lwd=1.4,lty=2)
  moneyaxis(pretty(range(c(full$base_usd,full$sma200_usd))));abline(h=0,col="#BBBBBB",lty=3)
  abline(h=-100000,col="#666666",lty=3,lwd=1)
  legend("bottomleft",legend=c("Base","SMA200 filter","Zero equity from USD 100,000"),
         col=c(blue,rust,grey),lty=c(1,2,3),lwd=c(1.4,1.4,1),bty="n",cex=.92)
})
writeLines(capture.output(sessionInfo()),file.path(root,"evidence/plot-session.txt"))
cat("Exported five figures as PNG and PDF from saved-result CSVs only.\n")
