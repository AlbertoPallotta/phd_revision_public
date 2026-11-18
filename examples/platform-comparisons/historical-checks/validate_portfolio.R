# Rscript validate_portfolio.R <historical-comparison-source.R> <new-output-folder> <historical-R-library>
# Use the recorded PT9008/platform environment, not an unexamined current package.
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=3L) stop("Missing configuration: supply comparison source, new output folder and historical R library.",call.=FALSE)
comparison_source <- normalizePath(args[1],winslash="/",mustWork=TRUE)
out <- normalizePath(args[2],winslash="/",mustWork=FALSE)
historical_library <- normalizePath(args[3],winslash="/",mustWork=TRUE)
if (file.exists(out)) stop("Output exists; choose a new folder.")
.libPaths(c(historical_library,.libPaths()))
Sys.setenv(TZ="UTC"); options(warn=1)
suppressPackageStartupMessages({library(PortfolioTesteR);library(data.table);library(quantstrat)})
source(comparison_source)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
checks <- list()
check <- function(id,yes) {checks[[length(checks)+1L]]<<-data.frame(check=id,passed=isTRUE(yes));if(!isTRUE(yes))stop(id)}
near <- function(a,b,tol=1e-9)isTRUE(all.equal(as.numeric(a),as.numeric(b),tolerance=tol))
x <- comparison_targets();saveRDS(x,file.path(out,"target-panels.rds"))
syms <- setdiff(names(x$prices),"Date"); mat <- function(d)as.matrix(d[,syms,with=FALSE])
check("twenty bundled symbols",length(syms)==20)
check("lookback rows remain cash A",all(rowSums(mat(x$weights_a))[1:12]==0))
check("lookback rows remain cash B",all(rowSums(mat(x$weights_b))[1:12]==0))
check("Top10 cap",all(rowSums(mat(x$selected_a))<=10))
check("intersection respected",all(mat(x$selected_b)<=mat(x$selected_m)&mat(x$selected_b)<=mat(x$selected_v)))
check("regime off cash",all(mat(x$weights_b)[!x$regime,,drop=FALSE]==0))
for(n in c("weights_a","weights_b"))check(paste(n,"finite long only normalised"),all(is.finite(mat(x[[n]])))&&all(mat(x[[n]])>=0)&&all(abs(rowSums(mat(x[[n]])))<1e-10|abs(rowSums(mat(x[[n]]))-1)<1e-10))
sigma <- mat(x$sigma); mask <- mat(x$selected_b)>0
q <- ifelse(mask & is.finite(sigma) & sigma>0,1/sigma,0);q[!is.finite(q)]<-0
q <- q/ifelse(rowSums(q)>0,rowSums(q),1)
check("inverse volatility independently normalised",near(q,mat(x$w_ivol)))
sig <- mat(x$momentum);sig[!mask|!is.finite(sig)|sig<=0]<-0
sig <- sig/ifelse(rowSums(sig)>0,rowSums(sig),1)
expected <- .6*sig+.4*q;expected<-expected/ifelse(rowSums(expected)>0,rowSums(expected),1)
check("blend including empty-leg fallback",near(expected,mat(x$weights_b)))
ix <- findInterval(as.numeric(x$prices$Date),as.numeric(x$sigma_daily$Date))
check("alignment never uses future date",all(ix>0)&&all(x$sigma_daily$Date[ix]<=x$prices$Date))
check("aligned volatility equals available daily row",near(mat(x$sigma),mat(x$sigma_daily)[ix,]))
for(letter in c("a","b")) {
  w <- x[[paste0("weights_",letter)]]
  res <- run_backtest(prices=copy(x$prices),weights=copy(w),initial_capital=100000,name=paste("Comparison",letter),verbose=FALSE)
  saveRDS(res,file.path(out,paste0(letter,"-PortfolioTesteR.rds")))
  acct <- account_targets(copy(x$prices),copy(w),paste0("comparison_",letter,"_20261022"))
  saveRDS(acct,file.path(out,paste0(letter,"-blotter.rds")))
  fwrite(acct$ledger,file.path(out,paste0(letter,"-account.csv")))
  fwrite(acct$transactions,file.path(out,paste0(letter,"-transactions.csv")))
  # Independent buy-and-hold-between-targets recurrence, no engine accounting.
  px<-mat(x$prices);ww<-mat(w); eq<-numeric(nrow(px));eq[1]<-100000
  for(k in 2:nrow(px))eq[k]<-eq[k-1]*(1-sum(ww[k-1,])+sum(ww[k-1,]*px[k,]/px[k-1,]))
  check(paste(letter,"manual cash ledger vs independent recurrence"),near(eq,acct$ledger$Equity))
  check(paste(letter,"blotter recorded equity vs recurrence"),near(eq,acct$ledger$RecordedEquity))
  check(paste(letter,"no borrowing"),all(acct$ledger$Cash>= -1e-7))
}
fwrite(rbindlist(checks),file.path(out,"CHECKS.csv"))
capture.output(sessionInfo(),file=file.path(out,"SESSION_INFO.txt"))
jsonlite::write_json(list(status="PASS",checks=length(checks),prices=nrow(x$prices),symbols=length(syms),
                       package=as.character(packageVersion("PortfolioTesteR")),quantstrat=as.character(packageVersion("quantstrat")),
                       scope="Two bundled-data illustration checks, each with two accounting representations. No historical research-study rerun or matched-output claim."),
                    file.path(out,"VALIDATION.json"),auto_unbox=TRUE,pretty=TRUE)
print(rbindlist(checks))
