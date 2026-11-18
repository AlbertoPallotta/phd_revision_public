# Completed illustrations of the two original Chapter 4 comparisons.
# Requires PortfolioTesteR 0.1.4.9008. These are NOT the Top-5 union A1 study.
comparison_targets <- function() {
  stopifnot(as.character(utils::packageVersion("PortfolioTesteR")) == "0.1.4.9008")
  e <- new.env(); utils::data(list=c("sample_prices_weekly","sample_prices_daily"),package="PortfolioTesteR",envir=e)
  p <- PortfolioTesteR::convert_to_nweeks(data.table::copy(e$sample_prices_weekly),n=4,method="last")
  symbols <- setdiff(names(p),"Date")
  mom <- PortfolioTesteR::calc_momentum(data.table::copy(p),lookback=12,na_policy="propagate")
  selected_a <- PortfolioTesteR::filter_top_n(mom,n=10)
  weights_a <- PortfolioTesteR::weight_equally(selected_a)
  sigma_daily <- PortfolioTesteR::calc_rolling_volatility(data.table::copy(e$sample_prices_daily),lookback=60,method="std")
  sigma <- PortfolioTesteR::align_to_timeframe(sigma_daily,p$Date,method="forward_fill")
  for(s in symbols) sigma[!is.finite(get(s)) | get(s)<=0,(s):=NA_real_]
  selected_m <- PortfolioTesteR::filter_top_n(mom,n=20)
  selected_v <- PortfolioTesteR::filter_top_n(sigma,n=20,ascending=TRUE)
  selected_b <- PortfolioTesteR::combine_filters(selected_m,selected_v,op="and")
  ma <- PortfolioTesteR::calc_moving_average(data.table::copy(p),window=10)
  distance <- PortfolioTesteR::calc_distance(p,ma)
  above <- as.matrix(distance[,symbols,with=FALSE])>0
  breadth <- rowMeans(above,na.rm=TRUE)
  regime <- is.finite(breadth) & breadth>0.5
  for(s in symbols) data.table::set(selected_b,which(!regime),s,FALSE)
  reciprocal <- data.table::copy(sigma)
  for(s in symbols) reciprocal[,(s):=ifelse(is.finite(get(s)) & get(s)>0,1/get(s),0)]
  w_sig <- PortfolioTesteR::weight_by_signal(selected_b,mom)
  w_ivol <- PortfolioTesteR::weight_by_signal(selected_b,reciprocal)
  weights_b <- PortfolioTesteR::combine_weights(list(w_sig,w_ivol),weights=c(0.6,0.4))
  list(prices=p,momentum=mom,sigma=sigma,sigma_daily=sigma_daily,
       selected_a=selected_a,weights_a=weights_a,selected_m=selected_m,
       selected_v=selected_v,selected_b=selected_b,breadth=breadth,regime=regime,
       reciprocal=reciprocal,w_sig=w_sig,w_ivol=w_ivol,weights_b=weights_b)
}

# Transaction-accounting comparator: targets are supplied by the same recipe.
# Fractional stock quantities, zero fees, synchronous observed prices; no order
# matching simulation. Calls blotter directly, within the quantstrat ecosystem.
account_targets <- function(prices,weights,id,initial_capital=100000) {
  symbols <- setdiff(names(prices),"Date")
  stopifnot(identical(names(prices),names(weights)),identical(prices$Date,weights$Date))
  px <- as.matrix(prices[,symbols,with=FALSE]); w <- as.matrix(weights[,symbols,with=FALSE])
  stopifnot(all(is.finite(px)),all(px>0),all(is.finite(w)),all(w>=0),all(rowSums(w)<=1+1e-12))
  FinancialInstrument::currency("USD")
  for(s in symbols) {
    FinancialInstrument::stock(s,currency="USD",multiplier=1)
    series <- xts::xts(px[,s],order.by=prices$Date)
    colnames(series) <- paste0(s,".Close")
    assign(s,series,envir=.GlobalEnv)
  }
  # Unique caller IDs; do not remove or overwrite any existing portfolios.
  initial <- as.character(min(prices$Date)-1)
  blotter::initPortf(id,symbols=symbols,initDate=initial)
  blotter::initAcct(id,portfolios=id,initDate=initial,initEq=initial_capital)
  cash <- initial_capital; quantities <- setNames(rep(0,length(symbols)),symbols)
  rows <- vector("list",nrow(prices)); trades <- list()
  for(k in seq_len(nrow(prices))) {
    date <- as.character(prices$Date[k])
    # Revalue all current positions BEFORE calculating this date's orders.
    equity <- cash+sum(quantities*px[k,])
    desired <- equity*w[k,]/px[k,]
    delta <- desired-quantities
    for(j in seq_along(symbols)) if(abs(delta[j])>1e-10) {
      blotter::addTxn(id,symbols[j],TxnDate=date,TxnPrice=px[k,j],TxnQty=delta[j],TxnFees=0,verbose=FALSE)
      trades[[length(trades)+1L]] <- data.frame(Date=date,Symbol=symbols[j],Quantity=delta[j],Price=px[k,j])
    }
    cash <- cash-sum(delta*px[k,]); quantities <- desired
    blotter::updatePortf(id,Dates=date); blotter::updateAcct(id,Dates=date); blotter::updateEndEq(id,Dates=date)
    account <- blotter::getAccount(id)$summary
    rows[[k]] <- data.frame(Date=date,Equity=equity,Cash=cash,
                            RecordedEquity=as.numeric(tail(account$End.Eq,1)))
  }
  list(ledger=data.table::rbindlist(rows),transactions=data.table::rbindlist(trades),
       final_quantity=quantities,portfolio=blotter::getPortfolio(id),account=blotter::getAccount(id))
}
