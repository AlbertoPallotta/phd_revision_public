source(file.path(Sys.getenv("PT_BUILDER_ROOT"),"thesis-reproduction/scripts/common.R"))
stopifnot(all(fread(file.path(repdir,"evidence/result-validation-checks.csv"))$status=="PASSED"))
out<-file.path(repdir,"results/study");fig<-file.path(repdir,"figures")
a1<-readRDS(file.path(out,"A1.rds"));gr<-readRDS(file.path(out,"B3-grid.rds"));wf<-readRDS(file.path(out,"B3-WF.rds"))
eq<-copy(wf$oos_stitched);eq$Date[1]<-as.Date(wf$oos_results[[1]]$dates[1])
stopifnot(identical(eq$Value,wf$oos_stitched$Value))
scores<-data.frame(lookback=vapply(gr$all_results,function(z)z$params$lookback,numeric(1)),n_top=vapply(gr$all_results,function(z)z$params$n_top,numeric(1)),score=gr$all_scores)
paired<-as.data.frame(wf$optimization_summary)
foot<-function(s)mtext(s,side=1,outer=TRUE,line=1,cex=.78)
plotters<-list(
 B3_full_sample_score_heatmap=function() {
   # Explicit row-to-coordinate mapping avoids the inherited helper label-order bug.
   par(mar=c(5,5,3,7));xs<-sort(unique(scores$lookback));ys<-sort(unique(scores$n_top));zl<-range(scores$score)
   pal<-hcl.colors(100,"YlGnBu",rev=TRUE)
   plot(NA,xlim=c(.5,4.5),ylim=c(.5,3.5),xaxs="i",yaxs="i",axes=FALSE,xlab="Momentum lookback (weekly observations)",ylab="Top N assets",main="Full-sample momentum grid")
   for(i in seq_len(nrow(scores))) {
     x<-match(scores$lookback[i],xs);y<-match(scores$n_top[i],ys);ci<-1+floor(99*(scores$score[i]-zl[1])/diff(zl))
     rect(x-.5,y-.5,x+.5,y+.5,col=pal[ci],border="white")
     luminance<-sum(col2rgb(pal[ci])[,1]/255*c(.2126,.7152,.0722))
     text(x,y,sprintf("%.3f",scores$score[i]),col=if(luminance<.5)"white" else "#182A3A",cex=1.1)
   }
   b<-scores[which.max(scores$score),];bx<-match(b$lookback,xs);by<-match(b$n_top,ys)
   rect(bx-.46,by-.46,bx+.46,by+.46,border="#D45F00",lwd=3)
   axis(1,at=seq_along(xs),labels=xs);axis(2,at=seq_along(ys),labels=ys,las=1);box();par(xpd=NA)
   for(i in 1:100)rect(4.7,.5+(i-1)*.03,4.85,.5+i*.03,col=pal[i],border=NA)
   text(4.95,c(.5,2,3.5),sprintf("%.3f",c(zl[1],mean(zl),zl[2])),adj=0,cex=.85);text(4.7,3.8,"Score",adj=0,cex=.9)
   foot("Full score range; 12 evaluations; orange border = maximum score")
   list(kind="direct_score_heatmap",cells=nrow(scores),range=zl,best=b)
 },
 B3_WF_selected_parameters=function() {
   par(mfrow=c(2,1),mar=c(4,5,2.8,1))
   for(nm in c("lookback","n_top")) {
     yy<-paired[[nm]];ticks<-if(nm=="lookback")c(8,12,26) else c(5,10)
     plot(1:4,yy,type="b",pch=19,lwd=1.8,col="#2C5D86",xaxt="n",yaxt="n",xlim=c(.9,4.1),ylim=range(ticks)+c(-1,1),xlab=if(nm=="n_top")"Walk-forward window" else "",ylab=if(nm=="lookback")"Weekly observations" else "Assets",main=if(nm=="lookback")"Selected momentum lookback" else "Selected Top N")
     axis(1,at=1:4,labels=paste0("W",1:4));axis(2,at=ticks,las=1);abline(h=ticks,col="grey88",lty=3)
   }
   foot("Six IS candidates per window; IS 52 / OOS 26 / step 26 rows")
   list(kind="parameter_path",values=paired[,c("Window","lookback","n_top")])
 },
 B3_WF_paired_scores=function() {
   x<-paired$IS_Score;y<-paired$OOS_Score;r<-cor(x,y)
   plot(x,y,pch=19,cex=1.3,col="#2C5D86",xlim=c(-.25,4.6),ylim=c(-.6,4.6),xlab="Selected in-sample score",ylab="Subsequent out-of-sample score",main="Selected IS and subsequent OOS scores")
   grid(col="grey90");abline(a=0,b=1,lty=2,col="grey55");text(x,y,labels=paste0("W",paired$Window),pos=4,offset=.55,cex=.9)
   legend("topleft",legend=sprintf("Pearson r = %.3f; four finite pairs",r),bty="n")
   foot("Scores: arithmetic mean / sample SD x sqrt(52); dashed diagonal = equal scores")
   list(kind="genuine_paired_scatter",columns=c("IS_Score","OOS_Score"),finite_pairs=4L,correlation=r)
 },
 B3_WF_within_leg_equity=function() {
   plot(as.Date(eq$Date),eq$Value,type="l",lwd=1.8,col="#2C5D86",xlab="Date",ylab="Gross portfolio value ($)",main="Compounded within-leg OOS returns")
   grid(col="grey88");abline(h=100000,lty=3,col="grey55");foot("Each OOS account resets to $100,000; boundary holding intervals excluded")
   list(kind="within_leg_equity",rows=nrow(eq),first=eq$Date[1],last=tail(eq$Value,1))
 },
 A1_multifactor_equity_drawdown=function() {
   par(mfrow=c(2,1),mar=c(4.5,5.2,2.8,1));v<-a1$backtest$portfolio_values;d<-as.Date(a1$backtest$dates)
   plot(d,v,type="l",lwd=1.8,col="#2C5D86",xlab="",ylab="Gross value ($)",main="A1 momentum and reciprocal-volatility blend");grid(col="grey87");abline(h=100000,lty=3,col="grey55")
   plot(d,100*(v/cummax(v)-1),type="l",lwd=1.6,col="#A54739",xlab="Date",ylab="Drawdown (%)",main="Drawdown on grouped decision dates");grid(col="grey87")
   foot("39 grouped four-week dates; same-row gross execution; initial capital $100,000")
   list(kind="A1_equity_drawdown",rows=length(v),final_value=tail(v,1))
 })
meta<-list()
for(n in names(plotters)) {
 png(file.path(fig,paste0(n,".png")),width=1800,height=1300,res=180);par(oma=c(3,0,1,0),mar=c(4.5,4.8,3,1),family="sans",cex=.95)
 meta[[n]]<-plotters[[n]]();dev.off()
 pdf(file.path(fig,paste0(n,".pdf")),width=10,height=7.22,onefile=FALSE);par(oma=c(3,0,1,0),mar=c(4.5,4.8,3,1),family="sans",cex=.95)
 pdf_meta<-plotters[[n]]();dev.off();stopifnot(identical(meta[[n]],pdf_meta))
}
saveRDS(meta,file.path(fig,"plot-metadata.rds"));capture.output(str(meta),file=file.path(fig,"plot-metadata.txt"))
fwrite(scores,file.path(fig,"B3_heatmap_raw_scores.csv"));fwrite(paired,file.path(fig,"B3_WF_plot_scores_and_parameters.csv"));fwrite(eq,file.path(fig,"B3_WF_display_equity.csv"))
fwrite(data.frame(Date=as.Date(a1$backtest$dates),Value=a1$backtest$portfolio_values,Drawdown=100*(a1$backtest$portfolio_values/cummax(a1$backtest$portfolio_values)-1)),file.path(fig,"A1_plot_data.csv"))
cat("Five PNG/PDF figure pairs produced from saved objects only.\n")
