# Validate saved selections and render figures from saved outputs only.
# This script does not run a strategy or change a portfolio result.
root <- normalizePath(commandArgs(trailingOnly=TRUE)[1],winslash="/",mustWork=TRUE)
library(PortfolioTesteR)
library(data.table)
stopifnot(normalizePath(find.package("PortfolioTesteR"),winslash="/")==normalizePath(Sys.getenv("PT_EXPECT_PATH"),winslash="/"))
options(warn=1)
out <- file.path(root,"local-outputs/momentum-figures-r1")
if(dir.exists(out))stop("Use a new output directory; existing figures are preserved.")
dir.create(out)
dir.create(file.path(out,"original"))
dir.create(file.path(out,"candidates"))
capture.output(sessionInfo(),file=file.path(out,"session-info.txt"))
ids <- c("C4-45-01", "C4-45-03", "C4-45-04", "C4-45-05")
audit <- list(); indexes <- list(); plot_checks <- list()
check <- function(id,name,ok,detail) {
 audit[[length(audit)+1L]] <<- data.table(example_id=id,check=name,passed=isTRUE(ok),detail=paste(detail,collapse="; "))
}
for(id in ids) {
 dir <- file.path(root,"local-outputs",id)
 z <- readRDS(file.path(dir,"internal-source-objects.rds"))
 nm <- switch(id,"C4-45-BM4"="bench","C4-45-02"="result_rsi","C4-45-03"="result_sector","C4-45-04"="result_vol","C4-45-05"="result_regime","result")
 bt <- z[[nm]]
 syms <- bt$symbols
 mask_name <- switch(id,"C4-45-BM4"="bench_selected","C4-45-02"="oversold","C4-45-05"="regime_filtered",
   "C4-45-08"="limited","C4-45-09"="limited","C4-45-10"="limited","selected")
 signal_name <- switch(id,"C4-45-BM4"="momentum_signal","C4-45-02"="rsi","C4-45-03"="sector_pct",
   "C4-45-07"="distance_from_ma","C4-45-08"="stoch_d","C4-45-09"="cci_values","C4-45-10"="stoch_d","momentum")
 mask <- as.matrix(z[[mask_name]][, ..syms])>0
 sig <- as.matrix(z[[signal_name]][, ..syms])
 w <- as.matrix(bt$weights[, ..syms])
 check(id,"no positive weights outside source selection",all(w[!mask]==0),sum(w[!mask]>0))
 check(id,"selected signal values finite",all(is.finite(sig[mask])),sum(!is.finite(sig[mask])))
 if(id=="C4-45-02")check(id,"RSI selection satisfies package below40 boundary",all(sig[mask]<=40),range(sig[mask]))
 if(id=="C4-45-03")check(id,"sector selection satisfies package above70 boundary",all(sig[mask]>=70),range(sig[mask]))
 if(id %in% c("C4-45-08","C4-45-10"))check(id,"stochastic selection satisfies package below30 boundary",all(sig[mask]<=30),range(sig[mask]))
 if(id=="C4-45-09")check(id,"CCI selection satisfies package above100 boundary",all(sig[mask]>=100),range(sig[mask]))
 if(id %in% c("C4-45-01","C4-45-04","C4-45-07")) {
   ascending <- id=="C4-45-07"
   dominance <- vapply(seq_len(nrow(sig)),function(i) {
     a<-sig[i,mask[i,]]; b<-sig[i,!mask[i,] & is.finite(sig[i,])]
     if(!length(a)||!length(b))return(TRUE)
     if(ascending)max(a)<=min(b)+1e-12 else min(a)>=max(b)-1e-12
   },logical(1))
   check(id,"selected scores dominate unselected scores in intended direction",all(dominance),sum(!dominance))
 }
 # Plot data must agree with the saved result, including every date.
 figures <- list(list(result=bt,result_id=id,datafile=file.path(dir,paste0(id,"-figure-data.csv"))))
 if(id=="C4-45-10")figures[[2]]<-list(result=z$bench,result_id="C4-45-BMW",datafile=file.path(dir,"C4-45-BMW-figure-data.csv"))
 for(rec in figures) {
   rid<-rec$result_id; x<-rec$result; d<-fread(rec$datafile); d$Date<-as.Date(d$Date)
   check(rid,"figure data matches saved equity and drawdown",identical(as.character(d$Date),as.character(x$dates)) &&
     max(abs(d$portfolio_value-x$portfolio_values))<1e-7 &&
     max(abs(d$drawdown_pct-(x$portfolio_values/cummax(x$portfolio_values)-1)*100))<1e-8,"CSV round-trip and full date alignment checked.")
   # Preserve an unmodified package plot for comparison, including the CCI overlay.
   original <- file.path(out,"original",paste0(rid,".png"))
   png(original,width=1800,height=1250,res=180,type="cairo")
   old_result <- tryCatch({if(rid=="C4-45-09")plot(z$perf,type="summary") else plot(x,type="performance");"DRAWN"},error=function(e)paste("ERROR",conditionMessage(e)))
   dev.off()
   plot_checks[[length(plot_checks)+1L]]<-data.table(example_id=rid,artifact="original package plot",status=old_result,note="Original package display retained for comparison; source plot preserved.")
   cadence <- if(rid %in% c("C4-45-10","C4-45-BMW")) "weekly observations" else "anchored 28-day grouped observations"
   title <- switch(rid,"C4-45-BM4"="Equal-weight benchmark: grouped grid","C4-45-BMW"="Equal-weight benchmark: weekly grid",
     "C4-45-01"="4.5.1  Simple momentum","C4-45-02"="4.5.2  RSI mean reversion",
     "C4-45-03"="4.5.3  Sector-relative momentum","C4-45-04"="4.5.4  Momentum with low-volatility rank weights",
     "C4-45-05"="4.5.5  Regime-filtered momentum","C4-45-07"="4.5.7  Moving-average distance",
     "C4-45-08"="4.5.8  Stochastic adaptive weights","C4-45-09"="4.5.9  CCI trend following",
     "C4-45-10"="4.5.10  Weekly stochastic oversold")
   if(rid=="C4-45-09") {
     df<-fread(file.path(dir,"C4-45-09-daily-figure-data.csv"));df$Date<-as.Date(df$Date)
     check(rid,"daily figure data matches saved analysis",identical(as.character(df$Date),as.character(z$perf$daily$dates)) &&
       max(abs(df$portfolio_value-z$perf$daily$values))<1e-7 &&
       max(abs(df$drawdown_pct-(z$perf$daily$values/cummax(z$perf$daily$values)-1)*100))<1e-8 &&
       max(abs(df$cumulative_return_pct-(cumprod(1+z$perf$daily$returns)-1)*100))<1e-8,
       "Full daily series checked; rolling volatility uses the original30-day/252 convention.")
     figfun<-function() {
       par(mfrow=c(2,2),mar=c(4.4,6.7,2.4,1.2),oma=c(2.5,0,3.2,0),las=1,cex.axis=.85)
       plot(df$Date,df$portfolio_value,type="l",col="#194C72",lwd=2,main="Daily portfolio value",xlab="Date",ylab="");mtext("Portfolio value ($)",side=2,line=4.8,las=0);grid()
       plot(df$Date,df$drawdown_pct,type="l",col="#A6423A",lwd=2,main="Daily drawdown",xlab="Date",ylab="");mtext("Drawdown (%)",side=2,line=4.8,las=0);grid();abline(h=0,lty=3)
       plot(df$Date,df$rolling_vol_30d_pct,type="l",col="#2F6D55",lwd=2,main="30-day rolling volatility",xlab="Date",ylab="");mtext("Annualised volatility (%)",side=2,line=4.8,las=0);grid()
       plot(df$Date,df$cumulative_return_pct,type="l",col="#194C72",lwd=2,main="Portfolio cumulative return",xlab="Date",ylab="");mtext("Return (%)",side=2,line=4.8,las=0);grid()
       mtext(title,outer=TRUE,side=3,line=1.7,font=2,cex=1.25)
       mtext("Daily monitoring; decisions on the unchanged grouped grid",outer=TRUE,side=3,line=.35,cex=.85)
       mtext("Gross returns. Portfolio-only display; original benchmark overlay withheld.",outer=TRUE,side=1,line=.6,cex=.8)
     }
     data_path<-file.path(dir,"C4-45-09-daily-figure-data.csv")
     limit<-"Original package benchmark overlay is inconsistent with excess-return definition; omitted from this display only. Daily analytics/results unchanged."
   } else {
     figfun<-function() {
       par(mfrow=c(2,1),mar=c(4.1,6.7,2.4,1.2),oma=c(2.5,0,3.2,0),las=1,cex.axis=.9)
       plot(d$Date,d$portfolio_value,type="l",col="#194C72",lwd=2,main="Portfolio value",xlab="Date",ylab="");mtext("Portfolio value ($)",side=2,line=4.8,las=0);grid()
       plot(d$Date,d$drawdown_pct,type="l",col="#A6423A",lwd=2,main="Drawdown",xlab="Date",ylab="");mtext("Drawdown (%)",side=2,line=4.8,las=0);grid();abline(h=0,lty=3)
       mtext(title,outer=TRUE,side=3,line=1.7,font=2,cex=1.25)
       mtext(paste(nrow(d),cadence,"|",min(d$Date),"to",max(d$Date)),outer=TRUE,side=3,line=.35,cex=.85)
       mtext("Gross returns | Initial capital $100,000 | Momentum availability: propagate (0.1.4.9006)",outer=TRUE,side=1,line=.4,cex=.8)
     }
     data_path<-rec$datafile
     limit<-"New candidate run: original recipe explicitly opts into momentum availability. No cost deduction; 9005 reporting unchanged."
   }
   for(ext in c("png","pdf")) {
     path<-file.path(out,"candidates",paste0(rid,".",ext))
     if(ext=="png")png(path,width=2000,height=1400,res=200,type="cairo") else pdf(path,width=10,height=7,family="Helvetica",useDingbats=FALSE)
     figfun();dev.off()
     indexes[[length(indexes)+1L]]<-data.table(example_id=rid,artifact_type=paste("figure",ext),path=path,generating_script=file.path(dirname(root),"scripts/render_candidates.R"),
       source_result=if(rid=="C4-45-09")file.path(dir,"C4-45-09-daily-analysis.rds") else file.path(dir,paste0(rid,"-backtest.rds")),
       numerical_data=data_path,sample=if(rid=="C4-45-09")"737 daily monitoring dates,2017-01-27 to2019-12-31" else paste(nrow(d),cadence),
       units="US dollars; percentages",cost_treatment="gross",backend="none: rule-based",interpretation_limit=limit,visual_review="PENDING")
   }
 }
}
fwrite(rbindlist(audit),file.path(out,"saved-result-checks.csv"))
fwrite(rbindlist(plot_checks),file.path(out,"original-plot-status.csv"))
fwrite(rbindlist(indexes),file.path(out,"figure-index.csv"))
stopifnot(all(rbindlist(audit)$passed))
print(rbindlist(audit)[,.(checks=.N,passed=sum(passed)),by=example_id])
cat("Figures generated from saved results. Visual inspection remains required.\n")
