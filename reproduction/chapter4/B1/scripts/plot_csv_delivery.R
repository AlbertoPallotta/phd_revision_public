# Delivery support.
args <- commandArgs(trailingOnly=TRUE);stopifnot(length(args)==2L)
input <- normalizePath(args[1],winslash="/",mustWork=TRUE)
out <- args[2];stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
for(rid in c("C4-45-01","C4-45-04")) {
 d <- read.csv(file.path(input,paste0(rid,"-figure-data.csv")));d$Date<-as.Date(d$Date)
 stopifnot(nrow(d)==39L,!anyNA(d$Date),!anyDuplicated(d$Date),
           all(is.finite(d$portfolio_value)),all(is.finite(d$drawdown_pct)),
           max(abs(d$drawdown_pct-100*(d$portfolio_value/cummax(d$portfolio_value)-1)))<1e-8)
 title <- if(rid=="C4-45-01")"4.5.1  Simple momentum" else "4.5.4  Momentum with low-volatility rank weights"
 cadence <- "anchored 28-day grouped observations"
figfun<-function() {
       par(mfrow=c(2,1),mar=c(4.1,6.7,2.4,1.2),oma=c(2.5,0,3.2,0),las=1,cex.axis=.9)
       plot(d$Date,d$portfolio_value,type="l",col="#194C72",lwd=2,main="Portfolio value",xlab="Date",ylab="");mtext("Portfolio value ($)",side=2,line=4.8,las=0);grid()
       plot(d$Date,d$drawdown_pct,type="l",col="#A6423A",lwd=2,main="Drawdown",xlab="Date",ylab="");mtext("Drawdown (%)",side=2,line=4.8,las=0);grid();abline(h=0,lty=3)
       mtext(title,outer=TRUE,side=3,line=1.7,font=2,cex=1.25)
       mtext(paste(nrow(d),cadence,"|",min(d$Date),"to",max(d$Date)),outer=TRUE,side=3,line=.35,cex=.85)
       mtext("Gross returns | Initial capital $100,000 | Momentum availability: propagate (0.1.4.9006)",outer=TRUE,side=1,line=.4,cex=.8)
     }
 for(ext in c("png","pdf")) {
  path<-file.path(out,paste0(rid,".",ext))
  if(ext=="png")png(path,width=2000,height=1400,res=200,type="cairo") else pdf(path,width=10,height=7,family="Helvetica",useDingbats=FALSE)
  figfun();dev.off()
 }
}
writeLines(capture.output(sessionInfo()),file.path(out,"session-info.txt"))
writeLines("New CSV display checks only; historical saved-object checks were not rerun.",file.path(out,"scope.txt"))
