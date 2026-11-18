args<-commandArgs(trailingOnly=TRUE)
if (length(args)!=3L) stop("Missing configuration: supply policy, regime and horizon; set PT_C5_ROOT and PT_C5_LIBRARY.",call.=FALSE)
policy<-args[1];regime<-args[2];horizon<-as.integer(args[3])
stopifnot(policy%in%c('legacy','propagate'),regime%in%c('pooled','per_group'),horizon%in%c(1L,4L,13L))
# PT_C5_ROOT is the original prepared historical workspace; PT_C5_LIBRARY holds its PT9007 installation.
required_path <- function(name) {
  value <- Sys.getenv(name, unset="")
  if (!nzchar(value)) stop("Missing configuration: set ", name, " to the required historical path.", call.=FALSE)
  normalizePath(value, winslash="/", mustWork=TRUE)
}
root <- required_path("PT_C5_ROOT")
historical_library <- required_path("PT_C5_LIBRARY")
.libPaths(c(historical_library,.libPaths()))
cell<-paste(policy,regime,paste0('H',horizon),sep='-')
out<-file.path(root,'internal',cell);stopifnot(!dir.exists(out));dir.create(out)
library(PortfolioTesteR);library(data.table);setDTthreads(1L)
stopifnot(as.character(packageVersion('PortfolioTesteR'))=='0.1.4.9007',
 normalizePath(find.package('PortfolioTesteR'),winslash='/')==normalizePath(file.path(historical_library,'PortfolioTesteR'),winslash='/',mustWork=TRUE),
 as.character(packageVersion('xgboost'))=='3.1.3.1')
source(file.path(root,'inputs/ml_helpers.R'))
prices<-readRDS(file.path(root,'internal/weekly-prices.rds'))
features<-readRDS(file.path(root,'internal',paste0('features-',policy,'.rds')))
groups<-readRDS(file.path(root,'internal/group-map.rds'))
model<-ml_make_model('xgboost',params=list(objective='reg:squarederror',eta=.05,max_depth=4,
 subsample=1,colsample_bytree=1,nthread=1),nrounds=200L)
fits<-list();warnings_seen<-list();started<-Sys.time()
log_every<-if(regime=='pooled')5L else 55L
fit_logged<-function(X,y) {
 idx<-length(fits)+1L;t<-proc.time()[['elapsed']]
 input_hash<-digest::digest(list(X=X,y=y),algo='sha256')
 rng_hash<-digest::digest(.Random.seed,algo='sha256')
 m<-model$fit(X,y)
 fits[[idx]]<<-data.table(fit_index=idx,n_rows=nrow(X),n_features=ncol(X),input_sha256=input_hash,
   rng_before_sha256=rng_hash,fit_seconds=proc.time()[['elapsed']]-t)
 if(idx==1L || idx%%log_every==0L) {
   fwrite(rbindlist(fits),file.path(out,'fit-audit.csv'))
   cat(format(Sys.time(),tz='UTC',usetz=TRUE),cell,'fit',idx,'rows',nrow(X),'elapsed',round(as.numeric(difftime(Sys.time(),started,units='secs')),1),'seconds\n');flush.console()
 }
 m
}
set.seed(42)
parameters<-list(features_list=features,prices_weekly=prices,horizons=horizon,
 fit_fn=fit_logged,predict_fn=model$predict,schedule=list(is=156L,oos=4L,step=4L),
 transform='zscore',selection=if(regime=='pooled')list(top_k=20L) else list(top_k=20L,max_per_group=3L),
 weighting=list(method='softmax',temperature=12),
 caps=if(regime=='pooled')list(max_per_symbol=.10) else list(max_per_symbol=.10,max_per_group=NULL),
 group_mode=regime,initial_capital=1e5,name_prefix='Paper-lite ',seed=42)
if(regime=='per_group')parameters$group_map<-groups
result<-withCallingHandlers(do.call(ml_backtest_multi,parameters),warning=function(w){
 warnings_seen[[length(warnings_seen)+1L]]<<-data.table(message=conditionMessage(w),call=paste(deparse(conditionCall(w)),collapse=' '))
})
bt<-result[[1]]$backtest
saveRDS(result,file.path(out,'result.rds'))
fwrite(rbindlist(fits),file.path(out,'fit-audit.csv'))
if(length(warnings_seen))fwrite(rbindlist(warnings_seen),file.path(out,'warnings.csv')) else fwrite(data.table(message=character(),call=character()),file.path(out,'warnings.csv'))
sy<-bt$symbols;p<-as.matrix(bt$positions[,..sy]);px<-as.matrix(bt$prices[,..sy]);v<-as.numeric(bt$portfolio_values)
checks<-data.table(check=c('positions_cash_reconcile','return_series_reconcile','positive_finite_equity','final_total_reconcile'),
 passed=c(max(abs(rowSums(p*px,na.rm=TRUE)+bt$cash-v))<1e-6,
 max(abs(bt$returns-c(0,diff(v)/head(v,-1))))<1e-12,all(is.finite(v)&v>0),
 abs(tail(v,1)/bt$initial_capital-1-bt$total_return_with_warmup)<1e-12))
fwrite(checks,file.path(out,'checks.csv'));stopifnot(all(checks$passed))
perf<-as.data.table(perf_metrics(data.frame(Date=as.Date(bt$dates),ret=as.numeric(bt$returns)),freq=52))
perf[,`:=`(policy=policy,regime=regime,horizon=horizon,transactions=bt$n_transactions,first_trade_date=as.character(bt$first_trade_date))]
fwrite(perf,file.path(out,'metrics.csv'))
fwrite(data.table(cell=cell,policy=policy,regime=regime,horizon=horizon,backend='xgboost',backend_version=as.character(packageVersion('xgboost')),
 package_version=as.character(packageVersion('PortfolioTesteR')),seed=42L,
 started_utc=format(started,tz='UTC',usetz=TRUE),finished_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
 elapsed_seconds=as.numeric(difftime(Sys.time(),started,units='secs')),fits=length(fits),status='COMPLETE'),file.path(out,'run-record.csv'))
capture.output(sessionInfo(),file=file.path(out,'session-info.txt'))
print(perf);cat('CELL COMPLETE:',cell,'\n')
