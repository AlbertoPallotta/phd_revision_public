# Presentation only: derived CSV input, no library or strategy execution.
args<-commandArgs(trailingOnly=TRUE);root<-normalizePath(args[[1]],winslash='/',mustWork=TRUE)
read<-function(n)read.csv(file.path(root,'evidence',n),check.names=FALSE)
blue<-'#245777';rust<-'#964C32';grey<-'#666666'
draw<-function(stem,height,fun) {
 for(fmt in c('png','pdf')) {
  file<-file.path(root,'figures',paste0(stem,'.',fmt))
  if(fmt=='png')png(file,width=7.1,height=height,units='in',res=300,type='cairo',bg='white')
  else cairo_pdf(file,width=7.1,height=height,family='serif')
  par(family='serif',mar=c(3.3,4.5,1.8,.8),mgp=c(2.8,.7,0),las=1,cex=1,
      col.axis='#222222',col.lab='#111111',bty='l')
  fun();dev.off()
 }
}
paths<-read('h13-equity-drawdown.csv');paths$Date<-as.Date(paths$Date)
pool<-paths[paths$regime=='pooled',];group<-paths[paths$regime=='per_group',]
draw('Figure_5_1_h13_equity_drawdown',4.8,function() {
 par(mfrow=c(2,1),oma=c(1.8,0,0,0),mar=c(2.7,4.6,1.8,.7),cex=.95)
 plot(pool$Date,pool$equity/1000,type='l',lwd=1.6,col=blue,ylim=range(paths$equity/1000),xlab='',ylab='Value (USD thousands)')
 lines(group$Date,group$equity/1000,lwd=1.6,col=rust,lty=2)
 title('(a) Gross portfolio value',adj=0,line=.45,cex.main=1)
 legend('topleft',legend=c('Pooled','Sector-based'),col=c(blue,rust),lty=c(1,2),lwd=1.6,bty='n',cex=.88,inset=.01)
 plot(pool$Date,100*pool$drawdown,type='l',lwd=1.6,col=blue,ylim=range(100*paths$drawdown),xlab='',ylab='Drawdown (%)')
 abline(h=0,col='#AAAAAA',lty=3);lines(group$Date,100*group$drawdown,lwd=1.6,col=rust,lty=2)
 title('(b) Drawdown from running peak',adj=0,line=.45,cex.main=1)
 mtext('Weekly date',side=1,outer=TRUE,line=.5,cex=.85)
})
roll<-read('ic-rolling.csv');roll$Date<-as.Date(roll$Date)
summ<-read('ic-summary.csv')
panel<-function(id,titletext,col,ylim,xlim) {
 x<-roll[roll$series==id,];s<-summ[summ$series==id,];end<-as.Date(s$last_finite_IC)
 plot(x$Date,x$roll_mean,type='n',ylim=ylim,xlim=xlim,xlab='',ylab='Mean rank IC')
 usr<-par('usr');rect(end,usr[3],xlim[2],usr[4],col='#F0F0F0',border=NA)
 abline(h=0,col='#888888',lty=3)
 lines(x$Date,x$roll_mean,col=col,lwd=1.6)
 title(titletext,adj=0,line=.45,cex.main=1)
}
draw('Figure_5_2_h13_model_ic',4.9,function() {
 par(mfrow=c(2,1),oma=c(1.8,0,0,0),mar=c(2.7,4.6,1.8,.7),cex=.95)
 lim<-range(roll$roll_mean[roll$series%in%c('model_pooled','model_per_group')],na.rm=TRUE)
 dates<-as.Date(c('2016-12-30','2023-12-15'))
 panel('model_pooled','(a) Pooled model',blue,lim,dates)
 panel('model_per_group','(b) Sector-based models',rust,lim,dates)
 mtext('Weekly date',side=1,outer=TRUE,line=.5,cex=.85)
})
draw('Figure_5_3_feature_ic',4.9,function() {
 par(mfrow=c(2,1),oma=c(1.8,0,0,0),mar=c(2.7,4.6,1.8,.7),cex=.95)
 lim<-range(roll$roll_mean[roll$series%in%c('feature_mom52','feature_rsi14')],na.rm=TRUE)
 dates<-as.Date(c('2014-01-03','2023-12-29'))
 panel('feature_mom52','(a) Lagged 52-week momentum',blue,lim,dates)
 panel('feature_rsi14','(b) Lagged RSI14',rust,lim,dates)
 mtext('Weekly date',side=1,outer=TRUE,line=.5,cex=.85)
})
cat('Three consolidated figures exported as PNG and vector PDF.\n')
