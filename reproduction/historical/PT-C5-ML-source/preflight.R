# PT_C5_ROOT is the original prepared historical workspace; PT_C5_LIBRARY holds its PT9007 installation.
required_path <- function(name) {
  value <- Sys.getenv(name, unset="")
  if (!nzchar(value)) stop("Missing configuration: set ", name, " to the required historical path.", call.=FALSE)
  normalizePath(value, winslash="/", mustWork=TRUE)
}
root <- required_path("PT_C5_ROOT")
historical_library <- required_path("PT_C5_LIBRARY")
.libPaths(c(historical_library,.libPaths()))

# Keep every preflight output separate from existing historical evidence.
outputs <- c("evidence/actual-dependencies.csv","evidence/backend-smoke.csv",
 "internal/weekly-prices.rds","evidence/weekly-panel-summary.csv","internal/group-map.rds",
 "internal/features-legacy.rds","internal/features-propagate.rds","evidence/feature-differences.csv",
 "evidence/feature-differences-by-date.csv","evidence/historical-rdata-inventory.csv",
 "evidence/historical-panel-comparison.csv","evidence/preflight-session.txt")
if (any(file.exists(file.path(root,outputs)))) stop("Preflight output exists; use a fresh prepared historical workspace.")
library(PortfolioTesteR);library(data.table);setDTthreads(1L)
stopifnot(as.character(packageVersion('PortfolioTesteR'))=='0.1.4.9007',
 normalizePath(find.package('PortfolioTesteR'),winslash='/')==normalizePath(file.path(historical_library,'PortfolioTesteR'),winslash='/',mustWork=TRUE))
deps<-c('PortfolioTesteR','data.table','TTR','zoo','xgboost','Matrix','jsonlite','glmnet','RSQLite')
dep<-rbindlist(lapply(deps,function(p){ok<-requireNamespace(p,quietly=TRUE);data.table(package=p,available=ok,
 version=if(ok)as.character(packageVersion(p)) else NA_character_,path=if(ok)find.package(p) else NA_character_)}))
fwrite(dep,file.path(root,'evidence/actual-dependencies.csv'));print(dep)
stopifnot(requireNamespace('xgboost',quietly=TRUE))
model<-ml_make_model('xgboost',params=list(objective='reg:squarederror',eta=.05,max_depth=4,
 subsample=1,colsample_bytree=1,nthread=1),nrounds=200L)
set.seed(42);sx<-matrix(seq_len(48)/10,ncol=3);sy<-seq_len(16)/100
m<-model$fit(sx,sy);pred<-model$predict(m,sx)
stopifnot(length(pred)==16,all(is.finite(pred)))
fwrite(data.table(actual_backend='xgboost',version=as.character(packageVersion('xgboost')),
 wrapper_fit_predict_pass=TRUE,nrounds=200L),file.path(root,'evidence/backend-smoke.csv'))

symbols<-readLines(file.path(root,'inputs/recommended_symbols_2014_2024.txt'),warn=FALSE)
univ<-symbols[!symbols%in%c('SPY','^VIX')]
raw<-fread(file.path(root,'internal/sql-query-data.csv'))
standardized<-getFromNamespace('standardize_data_format','PortfolioTesteR')(raw,symbol_order=univ)
weekly_prices<-convert_to_nweeks(standardized,n=1)
stopifnot(nrow(weekly_prices)>=160L,ncol(weekly_prices)==99L,!anyDuplicated(weekly_prices$Date))
saveRDS(weekly_prices,file.path(root,'internal/weekly-prices.rds'))
fwrite(data.table(rows=nrow(weekly_prices),symbols=ncol(weekly_prices)-1L,
 first_date=as.character(min(weekly_prices$Date)),last_date=as.character(max(weekly_prices$Date))),
 file.path(root,'evidence/weekly-panel-summary.csv'))
syms<-setdiff(names(weekly_prices),'Date')
sectors<-fread(file.path(root,'inputs/sp500_sectors.csv'))[Symbol%in%syms]
gmap<-unique(sectors[,.(Symbol,Group=Sector)])
stopifnot(!anyDuplicated(gmap$Symbol),setequal(gmap$Symbol,syms),!anyNA(gmap$Group))
saveRDS(gmap,file.path(root,'internal/group-map.rds'))

X<-ml_prepare_features(weekly_prices,include=c('mom12','mom26','mom52','rsi14','dist20'))
Xp<-lapply(X,copy)
for(lb in c(12L,26L,52L))Xp[[paste0('mom',lb)]]<-panel_lag(calc_momentum(weekly_prices,lb,na_policy='propagate'),1L)
saveRDS(X,file.path(root,'internal/features-legacy.rds'))
saveRDS(Xp,file.path(root,'internal/features-propagate.rds'))
audits<-list();rowchanges<-list()
for(n in names(X)) {
 a<-as.matrix(X[[n]][,..syms]);b<-as.matrix(Xp[[n]][,..syms])
 equal<-(is.na(a)&is.na(b))|(!is.na(a)&!is.na(b)&a==b)
 bad<-which(!equal,arr.ind=TRUE)
 stopifnot(all(a[!equal]==0),all(is.na(b[!equal])))
 if(!startsWith(n,'mom'))stopifnot(identical(X[[n]],Xp[[n]]))
 audits[[n]]<-data.table(feature=n,changed_cells=nrow(bad),
  first_changed_date=if(nrow(bad))as.character(weekly_prices$Date[min(bad[,1])]) else NA_character_,
  last_changed_date=if(nrow(bad))as.character(weekly_prices$Date[max(bad[,1])]) else NA_character_)
 rowchanges[[n]]<-data.table(Date=weekly_prices$Date,feature=n,changed_assets=rowSums(!equal))
}
fwrite(rbindlist(audits),file.path(root,'evidence/feature-differences.csv'))
fwrite(rbindlist(rowchanges),file.path(root,'evidence/feature-differences-by-date.csv'))
oldenv<-new.env(parent=emptyenv());loaded<-load(file.path(root,'inputs/.RData'),envir=oldenv)
inv<-rbindlist(lapply(loaded,function(n){x<-get(n,oldenv);data.table(name=n,class=paste(class(x),collapse=';'),
 length=length(x),bytes=as.numeric(object.size(x)))}))
fwrite(inv,file.path(root,'evidence/historical-rdata-inventory.csv'))
if(exists('weekly_prices',oldenv,inherits=FALSE)){
 oldpx<-get('weekly_prices',oldenv);fwrite(data.table(object='weekly_prices',
  all_equal=isTRUE(all.equal(oldpx,weekly_prices)),old_rows=nrow(oldpx),new_rows=nrow(weekly_prices)),
  file.path(root,'evidence/historical-panel-comparison.csv'))
}
capture.output(sessionInfo(),file=file.path(root,'evidence/preflight-session.txt'))
print(rbindlist(audits));cat('PREFLIGHT PASS\n')
