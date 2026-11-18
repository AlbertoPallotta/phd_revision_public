# Appendix B strategy with explicit information timing and a bounded execution
# feed. Daily aggregation follows the supplied file's calendar dates in New York;
# it does not claim to reconstruct an exchange-session calendar.
make_es_strategy <- function(bar5, spread=0.25, contracts=4, contract_multiplier=50) {
  stopifnot(xts::is.xts(bar5), is.finite(contract_multiplier), contract_multiplier > 0)
  force(bar5); force(spread); force(contracts); force(contract_multiplier)
  function(price, atr_n=14L, N_bars=15L, kTP=1.5, kSL=1, use_sma200=FALSE) {
    horizon <- attr(price, "execution_end")
    price <- data.table::copy(price)
    if (is.null(horizon)) horizon <- tail(zoo::index(bar5), 1) + 300
    price[, NextOpenTime := data.table::shift(date, type="lead")]
    price[, `:=`(PDH=data.table::shift(High), PDL=data.table::shift(Low))]
    hlc <- xts::xts(price[, .(High, Low, Close)], order.by=price$date)
    price[, ATR := as.numeric(TTR::ATR(hlc, n=atr_n)[, "atr"])]
    price[, `:=`(BuySignal=(Low <= PDH) & (Close > PDH),
                 ShortSignal=(High >= PDL) & (Close < PDL))]
    if (use_sma200) {
      trend <- TTR::SMA(price$Close, 200)
      price[, `:=`(BuySignal=BuySignal & Close > trend, ShortSignal=ShortSignal & Close < trend)]
    }
    price[, `:=`(OrderSize=0, EntryPrice=NA_real_, EntryType="Limit", EntryTime=NextOpenTime,
                 TakeProfit=NA_real_, StopLoss=NA_real_, CLOSE_CANDLES_SINCE_ENTRY=N_bars)]
    price[BuySignal == TRUE, `:=`(EntryPrice=PDH, OrderSize=contracts)]
    price[ShortSignal == TRUE, `:=`(EntryPrice=PDL, OrderSize=-contracts)]
    price[OrderSize != 0, `:=`(TakeProfit=EntryPrice + sign(OrderSize)*kTP*ATR,
                              StopLoss=EntryPrice - sign(OrderSize)*kSL*ATR)]
    invalid <- price[OrderSize != 0 & (!is.finite(ATR) | !is.finite(TakeProfit) |
                                     !is.finite(StopLoss) | is.na(EntryTime))]
    price[!is.finite(ATR) | !is.finite(TakeProfit) | !is.finite(StopLoss) | is.na(EntryTime), OrderSize:=0]
    markers <- price$date[!is.na(price$NextOpenTime)]
    rules <- price[!is.na(NextOpenTime)]
    rules[, `:=`(RuleTime=date, AvailableAt=NextOpenTime,
      ValidUntil=as.POSIXct(as.character(as.Date(NextOpenTime, tz="America/New_York")+1L), tz="America/New_York"))]
    # Unfilled entries expire at the end of their eligible strategy interval.
    # Protective exits belong to the filled trade and survive that entry expiry.
    times <- zoo::index(bar5)
    bars <- bar5[times >= min(price$date) & times < horizon]
    execution <- TradeTesterR::mergeAskBid(bars, bars-spread)
    aligned <- getFromNamespace(".align_daily_rules", "TradeTesterR")(
      execution, rules, c("ATR", "PDH", "PDL"),
      c("OrderSize", "EntryTime", "EntryType", "EntryPrice", "TakeProfit", "StopLoss", "CLOSE_CANDLES_SINCE_ENTRY"),
      marker_times=markers)
    bt <- TradeTesterR::backtest(aligned$data,
      exit_args=list(ambiguity="stop_first"), CloseTradeOnOppositeSignal=FALSE,
      AddToPosition=FALSE, TradeTimeLimit=lubridate::years(200), end_time=horizon, verbose=FALSE)
    if (nrow(bt$results)) {
      bt$results[, `:=`(Returns_points=Returns, ContractMultiplier=contract_multiplier)]
      bt$results[, Returns := Returns_points * contract_multiplier]
    }
    bt$audit <- c(aligned$audit, list(invalid_orders=invalid, execution_end=horizon,
                                    currency="USD", contract_multiplier=contract_multiplier))
    bt
  }
}
