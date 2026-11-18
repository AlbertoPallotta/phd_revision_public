source(file.path(Sys.getenv("PT_BUILDER_ROOT"),"thesis-reproduction/scripts/common.R"))
out <- file.path(repdir,"results/study")
stopifnot(file.exists(file.path(out,"RUN_COMPLETE.txt")))
auditdir <- file.path(repdir,"results/audit");dir.create(auditdir,showWarnings=FALSE)
data_in <- readRDS(file.path(out,"bundled-input-panels.rds"));weekly<-data_in$weekly;daily<-data_in$daily
a1 <- readRDS(file.path(out,"A1.rds"));wf <- readRDS(file.path(out,"B3-WF.rds"));gr <- readRDS(file.path(out,"B3-grid.rds"))
scores <- fread(file.path(out,"candidate-scores.csv"));attempts<-fread(file.path(out,"evaluation-attempts.csv"))
check("study_count",nrow(scores)==41 && nrow(attempts)==41 && all(is.na(scores$error)|scores$error==""))
check("study_phase_counts",identical(as.integer(table(factor(scores$phase,levels=c("A1","grid","IS","OOS")))),c(1L,12L,24L,4L)))
check("all_20_symbols_order",identical(names(a1$prices),names(weekly)) && ncol(weekly)==21)
# Independent endpoint/ranking oracle: stable column index resolves equal scores.
momentum_oracle <- function(p,L) {
  px<-mat(p);m<-matrix(NA_real_,nrow(px),ncol(px))
  if(nrow(px)>L) for(i in seq.int(L+1L,nrow(px))) for(j in seq_len(ncol(px)))
    if(is.finite(px[i,j])&&is.finite(px[i-L,j])&&px[i-L,j]!=0) {
      z<-px[i,j]/px[i-L,j]-1
      if(is.finite(z)) m[i,j]<-z
    }
  m
}
top_oracle <- function(m,N,ascending=FALSE) {
  s<-matrix(0,nrow(m),ncol(m))
  for(i in seq_len(nrow(m))) {
    ids<-which(is.finite(m[i,]));o<-order(if(ascending)m[i,ids]else -m[i,ids],ids)
    keep<-head(ids[o],N);s[i,keep]<-1
  };s
}
normalise <- function(m) {
  w<-m
  for(i in seq_len(nrow(m))) w[i,]<-if(sum(m[i,])>0)m[i,]/sum(m[i,])else 0
  w
}
mm <- momentum_oracle(a1$prices,12)
check("A1_endpoint_momentum",near(mat(a1$momentum),mm))
dr <- apply(mat(daily),2,function(x)c(NA_real_,diff(x)/head(x,-1)))
sdoracle<-matrix(NA_real_,nrow(daily),ncol(dr))
for(i in 61:nrow(daily)) for(j in seq_len(ncol(dr))) {
  x<-dr[(i-59):i,j]
  if(all(is.finite(x))) sdoracle[i,j]<-sqrt(mean((x-mean(x))^2))
}
check("A1_daily_population_SD",near(mat(a1$daily_sigma),sdoracle))
source_idx<-vapply(a1$prices$Date,function(d)max(which(as.Date(daily$Date)<=as.Date(d,origin="1970-01-01"))),integer(1))
aligned<-sdoracle[source_idx,,drop=FALSE]
check("A1_alignment",near(mat(a1$sigma_aligned),aligned) && identical(source_idx,as.integer(a1$alignment$source_row)))
check("A1_alignment_cutoff",all(as.Date(a1$alignment$source_date)<=as.Date(a1$prices$Date)))
clean<-aligned;clean[!is.finite(clean)|clean<=0]<-NA_real_
sm<-top_oracle(mm,5);sv<-top_oracle(clean,5,TRUE);su<-1*((sm+sv)>0)
check("A1_momentum_selection",all(mat(a1$selected_momentum)==sm))
check("A1_volatility_selection",all(mat(a1$selected_volatility)==sv))
check("A1_union",near(mat(a1$selected_union),su))
mp<-mm;mp[!is.finite(mp)|mp<=0|su==0]<-0
q<-matrix(0,nrow(clean),ncol(clean));ok<-is.finite(clean)&clean>0&su>0;q[ok]<-1/clean[ok]
wm<-normalise(mp);wv<-normalise(q);combined<-normalise(.6*wm+.4*wv)
check("A1_momentum_weights",near(mat(a1$weights_momentum),wm))
check("A1_reciprocal_weights",near(mat(a1$weights_volatility),wv))
check("A1_combined_weights",near(mat(a1$weights),combined))
for(n in c("momentum","daily_sigma","sigma_aligned","sigma_eligible","selected_momentum","selected_volatility","selected_union","reciprocal","weights_momentum","weights_volatility","weights","alignment","prices"))
  fwrite(a1[[n]],file.path(auditdir,paste0("A1-",n,".csv")),na="NA")
row_state <- data.frame(Date=as.Date(a1$prices$Date),momentum_active=rowSums(wm)>0,volatility_active=rowSums(wv)>0,
  selected_momentum=rowSums(sm),selected_volatility=rowSums(sv),union_count=rowSums(su),target_names=rowSums(combined>0),target_budget=rowSums(combined))
fwrite(row_state,file.path(auditdir,"A1-leg-states.csv"))
ledger_rows<-list();summary_rows<-list()
audit_bt <- function(id,bt,p,w) {
  px<-mat(p);ww<-mat(w);shares<-mat(bt$positions);n<-nrow(px);syms<-names(p)[-1]
  check(paste0(id,"_structure"),n==length(bt$dates)&&identical(as.character(p$Date),as.character(bt$dates))&&identical(names(p),names(w))&&identical(names(p),names(bt$positions)))
  check(paste0(id,"_target_budget"),all(is.finite(ww))&&all(ww>=0)&&all(abs(rowSums(ww)-1)<1e-12|rowSums(ww)==0))
  tx<-as.data.frame(bt$transactions);q0<-rep(0,ncol(px));cash0<-100000;max_share_gap<-0;max_cash_gap<-0
  for(i in seq_len(n)) {
    t<-if(nrow(tx))tx[as.Date(tx$Date)==as.Date(p$Date[i]),,drop=FALSE]else tx
    if(nrow(t)) {
      check(paste0(id,"_tx_prices_row",i),near(t$Price,px[i,match(t$Symbol,syms)]))
      check(paste0(id,"_tx_values_row",i),near(t$Value,t$Shares*t$Price))
      for(j in seq_len(nrow(t))) q0[match(t$Symbol[j],syms)]<-q0[match(t$Symbol[j],syms)]+t$Shares[j]
      cash0<-cash0-sum(t$Value)
    }
    max_share_gap<-max(max_share_gap,max(abs(q0-shares[i,])))
    max_cash_gap<-max(max_cash_gap,abs(cash0-bt$cash[i]))
  }
  marked<-rowSums(shares*px)+bt$cash
  check(paste0(id,"_ledger_shares"),max_share_gap==0)
  check(paste0(id,"_ledger_cash"),max_cash_gap<1e-7)
  check(paste0(id,"_cash_plus_holdings"),max(abs(marked-bt$portfolio_values))<1e-7)
  check(paste0(id,"_whole_share_targets"),identical(unname(shares),unname(floor(ww*bt$portfolio_values/px))))
  check(paste0(id,"_long_cash"),all(shares>=0)&all(bt$cash>=-1e-8)&all(shares==floor(shares)))
  ret<-c(0,diff(marked)/head(marked,-1))
  check(paste0(id,"_returns"),near(bt$returns,ret))
  check(paste0(id,"_self_financing"),max(abs(diff(marked)-rowSums(head(shares,-1)*(px[-1,,drop=FALSE]-head(px,-1)))))<1e-7)
  check(paste0(id,"_total_return"),near(bt$total_return,tail(marked,1)/100000-1))
  ledger_rows[[length(ledger_rows)+1L]] <<- data.frame(id=id,rows=n,transactions=nrow(tx),share_gap=max_share_gap,
    cash_gap=max_cash_gap,value_gap=max(abs(marked-bt$portfolio_values)),min_cash=min(bt$cash))
  active<-which(rowSums(shares)>0)
  summary_rows[[length(summary_rows)+1L]] <<- data.frame(id=id,first_date=as.character(p$Date[1]),last_date=as.character(tail(p$Date,1)),
    first_trade=if(length(active))as.character(p$Date[active[1]])else NA_character_,final_value=tail(marked,1),
    total_return=tail(marked,1)/100000-1,max_drawdown=min(marked/cummax(marked)-1),transactions=nrow(tx))
  dest<-file.path(auditdir,id);dir.create(dest,showWarnings=FALSE)
  fwrite(p,file.path(dest,"prices.csv"));fwrite(w,file.path(dest,"targets.csv"));fwrite(bt$positions,file.path(dest,"holdings.csv"))
  fwrite(tx,file.path(dest,"transactions.csv"))
  realised<-as.data.table(shares*px/marked);setnames(realised,syms)
  fwrite(cbind(data.table(Date=as.Date(p$Date),CashWeight=bt$cash/marked),realised),file.path(dest,"realised-weights.csv"))
  fwrite(data.frame(Date=as.Date(p$Date),cash=bt$cash,value=marked,return=ret,drawdown=marked/cummax(marked)-1),file.path(dest,"equity.csv"))
  invisible(ret)
}
audit_bt("A1",a1$backtest,a1$prices,a1$weights)
for(id in scores$id[scores$phase!="A1"]) {
  z<-readRDS(file.path(out,paste0(id,".rds")));p<-z$supplied_prices
  oracle<-momentum_oracle(p,z$params$lookback);selected<-top_oracle(oracle,z$params$n_top);weights<-normalise(selected)
  check(paste0(id,"_momentum"),near(mat(z$panels$momentum),oracle))
  check(paste0(id,"_selection"),near(mat(z$panels$selected),selected))
  check(paste0(id,"_weights"),near(mat(z$panels$weights),weights))
  if(z$phase=="OOS") {
    # Match actual supplied history to the preceding IS rows of the fixed weekly input.
    k<-z$window;is0<-1+26*(k-1);ise<-is0+51;oosidx<-(ise+1):(ise+26);extidx<-(ise-25):(ise+26)
    check(paste0(id,"_history_rows"),identical(as.character(p$Date),as.character(weekly$Date[extidx])) && near(mat(p),mat(weekly[extidx,])))
    check(paste0(id,"_history_endpoint_available"),all(is.finite(oracle[27:52,,drop=FALSE])) && all(rowSums(weights[27:52,,drop=FALSE])==1))
    p<-p[27:52,,drop=FALSE];w<-z$panels$weights[27:52,]
  } else {
    w<-z$panels$weights
    check(paste0(id,"_startup_cash_retained"),all(weights[1:z$params$lookback,,drop=FALSE]==0) && all(z$backtest$returns[1:z$params$lookback]==0))
  }
  ret<-audit_bt(id,z$backtest,p,w)
  # All B3 median date gaps are weekly; score includes the first stored zero.
  check(paste0(id,"_frequency52"),median(as.numeric(diff(as.Date(p$Date))))>=6 && median(as.numeric(diff(as.Date(p$Date))))<=8)
  oracle_score<-mean(ret)/sqrt(sum((ret-mean(ret))^2)/(length(ret)-1))*sqrt(52)
  check(paste0(id,"_score"),abs(oracle_score-z$score)<1e-12)
}
check("grid_order",near(as.matrix(gr$grid),as.matrix(expand.grid(lookback=c(8,12,16,20),n_top=c(5,10,15)))))
check("grid_selected_first_max",gr$best_idx==which.max(scores$score[scores$phase=="grid"]))
schedule<-list();included<-list();excluded<-list();ties<-list()
for(k in 1:4) {
  is0<-1+26*(k-1);ise<-is0+51;oos0<-ise+1;oose<-ise+26
  cand<-scores[phase=="IS" & window==k];best<-which.max(cand$score)
  check(paste0("WF",k,"_max_and_tie"),wf$chosen_params[[k]]$lookback==cand$lookback[best] && wf$chosen_params[[k]]$n_top==cand$n_top[best])
  check(paste0("WF",k,"_candidate_order"),near(as.matrix(cand[,.(lookback,n_top)]),as.matrix(expand.grid(lookback=c(8,12,26),n_top=c(5,10)))))
  check(paste0("WF",k,"_saved_pair"),near(wf$optimization_summary$IS_Score[k],cand$score[best]) && near(wf$optimization_summary$OOS_Score[k],scores[phase=="OOS" & window==k]$score))
  ties[[k]]<-data.frame(window=k,first_max_index=best,exact_max_ties=sum(cand$score==max(cand$score)))
  schedule[[k]]<-data.frame(window=k,is_start_row=is0,is_end_row=ise,history_start_row=ise-25,history_end_row=ise,
    oos_start_row=oos0,oos_end_row=oose,is_start=as.character(weekly$Date[is0]),is_end=as.character(weekly$Date[ise]),
    history_start=as.character(weekly$Date[ise-25]),history_end=as.character(weekly$Date[ise]),oos_start=as.character(weekly$Date[oos0]),oos_end=as.character(weekly$Date[oose]))
  bt<-wf$oos_results[[k]];v<-bt$portfolio_values
  included[[k]]<-data.frame(window=k,from=as.Date(head(bt$dates,-1)),to=as.Date(bt$dates[-1]),return=diff(v)/head(v,-1))
  excluded[[k]]<-data.frame(window=k,from=as.Date(weekly$Date[oos0-1]),to=as.Date(weekly$Date[oos0]),reason=if(k==1)"Before first OOS valuation; starts with cash" else "Cross-window holding interval omitted; account reset")
}
ints<-rbindlist(included);st<-wf$oos_stitched;values<-100000*c(1,cumprod(1+ints$return))
check("WF_100_intervals",nrow(ints)==100 && !anyDuplicated(ints$to))
check("WF_independent_compounding",near(st$Value,values) && identical(as.Date(st$Date[-1]),as.Date(ints$to)))
display<-fread(file.path(out,"WF-display-equity.csv"))
check("WF_display_anchor_only",near(display$Value,st$Value) && as.Date(display$Date[1])==as.Date(wf$oos_results[[1]]$dates[1]) && identical(as.character(display$Date[-1]),as.character(st$Date[-1])))
check("WF_four_distinct_finite_pairs",all(is.finite(wf$optimization_summary$IS_Score))&&all(is.finite(wf$optimization_summary$OOS_Score))&&nrow(unique(wf$optimization_summary[,c("IS_Score","OOS_Score")]))==4 && any(wf$optimization_summary$IS_Score!=wf$optimization_summary$OOS_Score))
fwrite(rbindlist(schedule),file.path(auditdir,"WF-splits-and-history.csv"))
fwrite(ints,file.path(auditdir,"WF-included-intervals.csv"))
fwrite(rbindlist(excluded),file.path(auditdir,"WF-excluded-boundary-intervals.csv"))
fwrite(data.frame(row=157:158,Date=weekly$Date[157:158],reason="No complete additional scheduled OOS block"),file.path(auditdir,"WF-unused-final-dates.csv"))
fwrite(rbindlist(ties),file.path(auditdir,"WF-selection-ties.csv"))
fwrite(rbindlist(ledger_rows),file.path(auditdir,"ledger-reconciliation.csv"))
fwrite(rbindlist(summary_rows),file.path(auditdir,"summary-results.csv"))
fwrite(data.frame(finite_pairs=4,pearson_correlation=cor(wf$optimization_summary$IS_Score,wf$optimization_summary$OOS_Score),
  stitched_final_value=tail(values,1),stitched_total_return=tail(values,1)/100000-1,stitched_max_drawdown=min(values/cummax(values)-1)),file.path(auditdir,"WF-summary.csv"))
save_checks("result-validation-checks")
cat(length(checks),"saved-result reconciliation checks passed; no backtests executed.\n")
