my_strat <- function(price, bb_period = 20, rsi_period = 14, contracts = 4) {
    # Indicators with full visibility  
    price[, RSI := RSI(Close, n = rsi_period)]
    boll <- BBands(price[, .(High, Low, Close)], n = bb_period, sd = 2)
    price[, upperBB := boll[, "up"]]
    price[, lowerBB := boll[, "dn"]]
    price[, midBB := boll[, "mavg"]]
    
    # Entry signal with RSI confirmation
    price[, BuySignal := RSI < 30 & Close <= lowerBB]
    price[BuySignal == TRUE, EntryPrice := lowerBB]
    price[BuySignal == TRUE, EntryType := "Limit"]
    price[BuySignal == TRUE, StopLoss := EntryPrice - (2000/(contracts*50))]
    
    # Partial exit definitions - framework handles complexity
    price[, targetorder_long_1 := midBB]
    price[, targetorder_long_1_amount := 0.5]  # Exit 50%
    price[, targetorder_long_2 := upperBB]  
    price[, targetorder_long_2_amount := 1.0]  # Exit remaining
    
    # Time-based exit
    price[BuySignal == TRUE, CLOSE_CANDLES_SINCE_ENTRY := 10]
    
    # Position sizing
    price[BuySignal == TRUE, OrderSize := contracts * 50]
}
