# data.table NSE and a few helper symbols used in non-standard eval
utils::globalVariables(c("phase", "time", 
  ".", "CLOSE_CANDLES_SINCE_ENTRY", "CV", "CancelOrder", "Close.b", "CumReturns",
  "CurrentPosSize", "CurrentSize", "DD", "DD_tc", "DailyPnL", "Day", "Drawdown",
  "EntryPrice", "EntryTime", "EntryType", "Equity", "Equity_tc", "Excess",
  "ExitAmount", "ExitPerc", "ExitPrice", "ExitReason", "ExitTime", "High.a", "High.b",
  "InSampleMetric", "Lag", "Low.a", "Low.b", "NextOpen", "NextOpenTime", "Open.a",
  "Open.b", "OrderSize", "Order_ID", "OrigOrder", "Peak", "Peak_tc", "Ret", "Returns",
  "Rf_d", "Side", "StopLoss", "TakeProfit", "Window", "WindowIndex", "ask", "br",
  "browsable", "current.a", "current.b", "h1", "index",  "next_order",
  "open_time", "prev_order", "tagList", "tail"
))

utils::globalVariables(c("phase", "time", "..keys", "..param_cols", "AmbiguousBar", "AvailableAt", "OrderRuleTime", "RuleTime", "StateRuleTime", "ValidUntil", "IS_ProfitPerDay", "OOS_ProfitPerDay", "pnl"))
utils::globalVariables(c("phase", "time", "AmbiguousEntry", "EntryValidUntil", "Disposition", "DispositionTime",
  "SourceTime", "WinnerOrder_ID", "CompetingOrderIDs", "priority", "phase_order",
  "level", "amount", "kind", "id", "Close.a"))
