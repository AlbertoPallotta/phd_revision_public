# Scientific figures from saved results only. No strategy execution.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L)
task <- normalizePath(args[1],winslash="/",mustWork=TRUE)
g <- file.path(task,"figures")
Sys.setenv(TZ="UTC")
read <- function(name) read.csv(file.path(g,name),stringsAsFactors=FALSE)
heat <- read("figure3_2_first_window.csv")
selected <- read("figure3_3_selected_parameters.csv")
selected$date <- as.Date(selected$train_start)
comp <- read("figure3_4_IS_OOS.csv")
comp$date <- as.Date(comp$Window)
degradation <- read("figure3_5_degradation.csv")
export <- function(stem,height,fun) {
  png(file.path(g,paste0(stem,".png")),width=7.1,height=height,units="in",res=300)
  fun(); dev.off()
  pdf(file.path(g,paste0(stem,".pdf")),width=7.1,height=height,family="Times")
  fun(); dev.off()
}
base_par <- function(...,cex=.94) par(family="serif",cex=cex,tcl=-.25,mgp=c(2.4,.7,0),bty="l",las=1,...)
export("Figure3_2_first_window_heatmap",3.0,function() {
  base_par(mar=c(3.7,4.8,1,1))
  x <- sort(unique(heat$boll_n)); y <- sort(unique(heat$stop_loss_amount))
  z <- matrix(NA_real_,nrow=length(x),ncol=length(y))
  for (i in seq_len(nrow(heat))) z[match(heat$boll_n[i],x),match(heat$stop_loss_amount[i],y)] <- heat$net_profit[i]
  limit <- max(abs(z))
  image(x,y,z,col=colorRampPalette(c("#d5886f","#fffdf7","#80b5d0"))(60),
        zlim=c(-limit,limit),xlab="Bollinger lookback (daily observations)",
        ylab="",xaxt="n",yaxt="n",useRaster=FALSE)
  axis(1,at=x);axis(2,at=y,las=1)
  mtext("Nominal stop-loss amount",side=2,line=3.6,las=0)
  for (i in seq_len(nrow(heat))) text(heat$boll_n[i],heat$stop_loss_amount[i],
                                     sprintf("%.0f",heat$net_profit[i]),cex=.85)
  top <- heat[which.max(heat$net_profit),]
  rect(top$boll_n-4.8,top$stop_loss_amount-480,top$boll_n+4.8,
       top$stop_loss_amount+480,border="black",lwd=1.8)
})
export("Figure3_3_parameter_paths",4.4,function() {
  base_par(mfrow=c(2,1),mar=c(3.9,4.8,1.6,1))
  ticks <- as.Date(paste0(seq(2016,2024,2),"-01-01"))
  plot(selected$date,selected$boll_n,type="s",col="#21618c",lwd=1.2,
       xlab="",ylab="Lookback",yaxt="n",xaxt="n",ylim=c(15,105))
  axis(2,at=c(20,40,60,80,100),las=1);axis.Date(1,at=ticks,format="%Y")
  points(selected$date,selected$boll_n,pch=16,col="#21618c",cex=.7)
  title("(a) Bollinger lookback",adj=0,font.main=1,cex.main=1)
  plot(selected$date,selected$stop_loss_amount,type="s",col="#7d4931",lwd=1.2,
       xlab="Training-window start",ylab="",xaxt="n",yaxt="n",ylim=c(500,4500))
  axis.Date(1,at=ticks,format="%Y");axis(2,at=c(1000,2000,3000,4000),las=1)
  mtext("Stop-loss amount",side=2,line=3.6,las=0)
  points(selected$date,selected$stop_loss_amount,pch=16,col="#7d4931",cex=.7)
  title("(b) Nominal stop-loss amount",adj=0,font.main=1,cex.main=1)
})
export("Figure3_4_IS_OOS_comparison",3.35,function() {
  base_par(mfrow=c(1,2),mar=c(4.3,4.6,2.1,.7),cex=.83)
  limits <- range(c(comp$IS_ProfitPerDay,comp$OOS_ProfitPerDay))
  plot(comp$IS_ProfitPerDay,comp$OOS_ProfitPerDay,xlim=limits,ylim=limits,
       pch=21,bg="#21618c",col="white",cex=1.15,xlab="IS P&L / scheduled day",
       ylab="",asp=1)
  mtext("OOS P&L / scheduled day",side=2,line=3.2,las=0)
  abline(a=0,b=1,lty=2,col="#777777");abline(h=0,v=0,col="#dddddd",lwd=.6)
  points(comp$IS_ProfitPerDay,comp$OOS_ProfitPerDay,pch=21,bg="#21618c",col="white",cex=1.15)
  title("(a) Paired outcomes",adj=0,font.main=1,cex.main=1.1)
  plot(comp$date,comp$IS_ProfitPerDay,type="l",col="#21618c",lwd=1.2,
       ylim=limits,xlab="Training-window start",ylab="",xaxt="n")
  mtext("P&L / scheduled day",side=2,line=3.2,las=0)
  ticks <- as.Date(paste0(seq(2016,2024,2),"-01-01"))
  axis.Date(1,at=ticks,format="%Y")
  abline(h=0,col="#aaaaaa",lwd=.5)
  points(comp$date,comp$IS_ProfitPerDay,pch=16,col="#21618c",cex=.65)
  lines(comp$date,comp$OOS_ProfitPerDay,col="#7d4931",lty=2,lwd=1.2)
  points(comp$date,comp$OOS_ProfitPerDay,col="#7d4931",pch=17,cex=.7)
  title("(b) Outcomes by window",adj=0,font.main=1,cex.main=1.1)
  legend("topleft",legend=c("IS","OOS"),col=c("#21618c","#7d4931"),
         lty=c(1,2),pch=c(16,17),bty="n",horiz=TRUE,cex=.95)
})
export("Figure3_5_degradation",2.8,function() {
  base_par(mar=c(4,4.4,1,1))
  d <- degradation$degradation_percent
  d <- d[is.finite(d)]
  hist(d,breaks="Sturges",col="#9ab9cb",border="white",main="",
       xlab="Degradation in P&L per scheduled day (%)",ylab="Window count")
  abline(v=0,lty=2,col="#555555")
})
cat("Four figures exported from saved results; no backtests.\n")
