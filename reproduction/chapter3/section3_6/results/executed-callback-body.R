function (price, stop_loss_amount = 2000, boll_n = 20) 
{
    price[, `:=`(NextOpenTime, shift(date, n = 1, type = "lead"))]
    price[, `:=`(NextOpen, shift(Open, n = 1, type = "lead"))]
    boll <- BBands(price[, .(High, Low, Close)], n = boll_n, 
        sd = 2)
    price[, `:=`(upperBB, boll[, "up"])]
    price[, `:=`(lowerBB, boll[, "dn"])]
    price[, `:=`(midBB, boll[, "mavg"])]
    price[, `:=`(PrevClose, shift(Close, n = 1, type = "lag"))]
    price[, `:=`(BuySignal, Close <= lowerBB & PrevClose > lowerBB)]
    price[BuySignal == TRUE, `:=`(EntryPrice, NextOpen)]
    price[BuySignal == TRUE, `:=`(EntryType, "Market")]
    price[BuySignal == TRUE, `:=`(StopLoss, EntryPrice - (stop_loss_amount/(trade_size_contracts * 
        futures_multiplier)))]
    price[, `:=`(targetorder_long_1, midBB)]
    price[, `:=`(targetorder_long_1_amount, 0.5)]
    price[, `:=`(targetorder_long_2, upperBB)]
    price[, `:=`(targetorder_long_2_amount, 1)]
    price[, `:=`(CLOSE_CANDLES_SINCE_ENTRY, 10)]
    if (tolower(instrument_type) == "futures") {
        if ("BuySignal" %in% names(price)) {
            price[BuySignal == TRUE, `:=`(OrderSize, futures_multiplier * 
                trade_size_contracts)]
        }
        if ("SellSignal" %in% names(price)) {
            price[SellSignal == TRUE, `:=`(OrderSize, -1 * futures_multiplier * 
                trade_size_contracts)]
        }
    }
    else {
        if (isTRUE(is_trade_in_shares) & tolower(instrument_type) == 
            "stocks") {
            if ("BuySignal" %in% names(price)) {
                price[BuySignal == TRUE, `:=`(OrderSize, trade_size_shares)]
            }
            if ("SellSignal" %in% names(price)) {
                price[SellSignal == TRUE, `:=`(OrderSize, -trade_size_shares)]
            }
        }
        else {
            if ("BuySignal" %in% names(price)) {
                price[BuySignal == TRUE, `:=`(OrderSize, getOrderSize(EntryPrice, 
                  trade_size_dollars, instrument_type))]
            }
            if ("SellSignal" %in% names(price)) {
                price[SellSignal == TRUE, `:=`(OrderSize, -getOrderSize(EntryPrice, 
                  trade_size_dollars, instrument_type))]
            }
        }
    }
    setorder(price, date)
    price[, `:=`(EntryTime, shift(date, 1, type = "lead"))]
    ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = df$date)
    bid <- ask - spread
    lower_timeframe <- mergeAskBid(ask, bid)
    dat <- merge(lower_timeframe, price[], by = "date", all.x = TRUE)
    bt <- backtest(dat, exit_fun = exit_TP_SL_signal, AddToPosition = FALSE, 
        CloseTradeOnOppositeSignal = FALSE, verbose = FALSE, 
        TradeTimeLimit = lubridate::weeks(1e+05))
    return(bt)
}
