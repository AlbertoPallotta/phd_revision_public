# Descriptive exports only: no fitting, allocation, execution or optimisation.
args<-commandArgs(trailingOnly=TRUE)
if (length(args)!=1L) stop("Missing configuration: supply output folder and set PT_C5_ROOT to the historical workspace.",call.=FALSE)
out<-normalizePath(args[[1]],winslash='/',mustWork=TRUE)
required_path <- function(name) {
  value <- Sys.getenv(name, unset="")
  if (!nzchar(value)) stop("Missing configuration: set ", name, " to the required historical path.", call.=FALSE)
  normalizePath(value, winslash="/", mustWork=TRUE)
}
src <- required_path("PT_C5_ROOT")
if (dir.exists(file.path(out,"evidence")) && length(list.files(file.path(out,"evidence"),all.files=TRUE,no..=TRUE))) stop("Output evidence exists; choose a fresh output folder.")
dir.create(file.path(out,"evidence"),showWarnings=FALSE)
library(data.table);library(PortfolioTesteR);setDTthreads(1)
stopifnot(as.character(packageVersion('PortfolioTesteR'))=='0.1.4.9007')
px<-readRDS(file.path(src,'internal/weekly-prices.rds'))
features<-readRDS(file.path(src,'internal/features-propagate.rds'))
gross_ref<-fread(file.path(src,'results/comparison-v1/gross-metrics.csv'))[policy=='propagate']
cost_ref<-fread(file.path(src,'results/comparison-v1/cost-metrics.csv'))[policy=='propagate' & Net==TRUE]
checks<-list();gross<-list();costs<-list();paths<-list();turns<-list();rolls<-list();raws<-list();summaries<-list()
test<-function(name,ok,error=NA_real_) {
 checks[[length(checks)+1L]]<<-data.table(check=name,passed=isTRUE(ok),max_error=error)
 if(!isTRUE(ok))stop(name)
}
metrics<-function(r) {
 r<-r[is.finite(r)];v<-cumprod(1+r);total<-tail(v,1)-1
 ann<-(1+total)^(52/length(r))-1;vol<-sd(r)*sqrt(52)
 data.table(total_return=total,ann_return=ann,ann_vol=vol,sharpe=ann/vol,max_drawdown=min(v/cummax(v)-1))
}
compare<-function(a,b,label) {
 cols<-c('total_return','ann_return','ann_vol','sharpe','max_drawdown')
 err<-max(abs(as.numeric(a[,..cols])-as.numeric(b[,..cols])))
 test(label,err<1e-10,err)
}
ic_export<-function(scores,labels,id) {
 # Match ml_ic_series_on_scores(): drop rows with no finite scores first.
 ss<-copy(scores);sy<-setdiff(names(ss),'Date')
 ss<-ss[rowSums(is.finite(as.matrix(ss[,..sy])))>0]
 ics<-ml_ic_series_on_scores(ss,labels)
 lab<-labels[match(ss$Date,labels$Date)]
 independent<-vapply(seq_len(nrow(ss)),function(i) {
  x<-as.numeric(ss[i,..sy]);y<-as.numeric(lab[i,..sy]);ok<-is.finite(x)&is.finite(y)
  if(sum(ok)<3)return(NA_real_)
  suppressWarnings(cor(rank(x[ok]),rank(y[ok])))
 },numeric(1))
 test(paste(id,'IC availability'),identical(is.finite(ics$IC),is.finite(independent)))
 err<-max(abs(ics$IC-independent),na.rm=TRUE);test(paste(id,'Spearman reconstruction'),err<1e-12,err)
 rolling<-roll_ic_stats(ics,window=26L)
 ownmean<-vapply(seq_len(nrow(ics)),function(i)if(i<26)NA_real_ else mean(ics$IC[(i-25):i],na.rm=TRUE),numeric(1))
 test(paste(id,'rolling mean'),isTRUE(all.equal(rolling$roll_mean,ownmean,tolerance=1e-12)))
 # Compare with the plotting helper's returned data, while discarding its plot.
 pdf(NULL);returned<-ml_plot_ic_roll(copy(scores),labels,window=26L);dev.off()
 test(paste(id,'original helper output'),isTRUE(all.equal(rolling,returned,tolerance=1e-12)))
 rolling[,series:=id];ics[,series:=id]
 counts<-vapply(seq_len(nrow(ss)),function(i)sum(is.finite(as.numeric(ss[i,..sy]))&is.finite(as.numeric(lab[i,..sy]))),integer(1))
 ics[,paired_names:=counts]
 rolls[[id]]<<-rolling;raws[[id]]<<-ics
 fin<-which(is.finite(rolling$roll_mean));last<-tail(fin,1)
 summaries[[id]]<<-data.table(series=id,raw_rows=nrow(ics),finite_IC=sum(is.finite(ics$IC)),
   first_finite_IC=as.character(ics$Date[which(is.finite(ics$IC))[1]]),last_finite_IC=as.character(tail(ics$Date[is.finite(ics$IC)],1)),
   first_roll=as.character(rolling$Date[fin[1]]),last_roll=as.character(rolling$Date[last]),
   mean_raw_IC=mean(ics$IC,na.rm=TRUE),min_roll=min(rolling$roll_mean,na.rm=TRUE),max_roll=max(rolling$roll_mean,na.rm=TRUE),
   last_roll_mean=rolling$roll_mean[last],last_roll_sd=rolling$roll_sd[last],last_roll_ICIR=rolling$roll_ICIR[last],
   last_window_finite=sum(is.finite(tail(ics$IC,26))),positive_roll_fraction=mean(rolling$roll_mean[fin]>0))
}
for(reg in c('pooled','per_group'))for(h in c(1L,4L,13L)) {
 cell<-paste0('propagate-',reg,'-H',h)
 obj<-readRDS(file.path(src,'internal',cell,'result.rds'))[[1]];b<-obj$backtest
 dates<-as.Date(b$dates);r<-as.numeric(b$returns);v<-as.numeric(b$portfolio_values)
 test(paste(cell,'full calendar'),length(r)==522 && all(is.finite(r)))
 test(paste(cell,'equity compound'),max(abs(1e5*cumprod(1+r)-v))<1e-7)
 sy<-setdiff(names(obj$weights),'Date');w<-as.matrix(obj$weights[,..sy]);active<-which(rowSums(w!=0)>0)
 wn<-w[active,,drop=FALSE];previous<-rbind(rep(0,ncol(w)),head(wn,-1))
 tau<-rep(0,length(r));tau[active]<-.5*rowSums(abs(wn-previous))
 turns[[cell]]<-data.table(regime=reg,horizon=h,Date=dates,turnover=tau,active=seq_along(r)%in%active)
 for(slice in c('Full','Pre','Post')) {
  keep<-switch(slice,Full=rep(TRUE,length(r)),Pre=dates<as.Date('2020-01-01'),Post=dates>=as.Date('2020-01-01'))
  m<-metrics(r[keep]);ref<-gross_ref[regime==reg & horizon==h & Slice==slice];stopifnot(nrow(ref)==1)
  compare(m,ref,paste(cell,slice,'saved metrics'))
  compare(m,as.data.table(perf_metrics(data.frame(Date=dates[keep],ret=r[keep]),freq=52)),paste(cell,slice,'package metrics'))
  av<-mean(tau[keep & seq_along(r)%in%active]);test(paste(cell,slice,'turnover mean'),abs(av-ref$avg_turnover)<1e-12)
  m[,`:=`(regime=reg,horizon=h,Slice=slice,n_weeks=sum(keep),avg_turnover=av)];gross[[paste(cell,slice)]]<-m
 }
 for(bps in c(5,10,15,20,25,50)) {
  m<-metrics(r-bps/10000*tau);ref<-cost_ref[regime==reg & horizon==h & Cost_bps==bps];stopifnot(nrow(ref)==1)
  compare(m,ref,paste(cell,bps,'saved costs'))
  m[,`:=`(regime=reg,horizon=h,Cost_bps=bps,Net=TRUE)];costs[[paste(cell,bps)]]<-m
 }
 if(h==13L) {
  paths[[reg]]<-data.table(regime=reg,Date=dates,equity=v,drawdown=v/cummax(v)-1,returns=r)
  masked<-scores_oos_only(copy(obj$scores),copy(obj$weights))
  ic_export(masked,make_labels(px,horizon=13L,type='log'),paste0('model_',reg))
 }
}
Y4<-make_labels(px,horizon=4L,type='log')
ic_export(features$mom52,Y4,'feature_mom52');ic_export(features$rsi14,Y4,'feature_rsi14')
write<-function(name,rows)fwrite(rbindlist(rows,use.names=TRUE,fill=TRUE),file.path(out,'evidence',name),na='NA')
write('gross-metrics-verified.csv',gross);write('cost-metrics-verified.csv',costs)
write('target-turnover.csv',turns);write('h13-equity-drawdown.csv',paths)
write('ic-raw.csv',raws);write('ic-rolling.csv',rolls);write('ic-summary.csv',summaries)
write('export-checks.csv',checks)
capture.output(sessionInfo(),file=file.path(out,'evidence/export-session.txt'))
print(rbindlist(summaries));cat('Checks passed:',length(checks),'No model or backtest executed.\n')
