


#' Optimize strategy parameters using walk-forward analysis
#'
#' @description
#' Performs walk-forward optimization by training on historical windows
#' and testing on subsequent out-of-sample periods.
#'
#' @param price A data.table with columns: date, Open, High, Low, Close, Volume
#' @param opt_params A data.frame where each row is a parameter combination to test
#' @param initial_equity Starting capital for performance calculations (default: 1000)
#' @param optimize_on Metric to optimize: "net_profit", "profit_factor", "sharpe", "sortino" (default: "net_profit")
#' @param train_set_weeks Number of weeks for training window (default: 52)
#' @param test_set_weeks Number of weeks for testing window (default: 26)
#' @param strategy Function receiving a copied price window and grid parameters.
#'   The execution_end attribute gives the exclusive window boundary.
#'   If NULL, resolves the legacy \code{my_strat} in the calling environment.
#' @param risk_free Optional frozen risk-free input for analyze_performance; NULL
#'   means zero. No network request is made.
#' @param verbose Print optimisation progress.
#' @param return_optimization_details Logical. Return detailed optimization history? (default: TRUE)
#'
#' @return If return_optimization_details = TRUE, returns a list with:
#'   - test_results: List of backtest results for each test window
#'   - optimization_history: List of all parameter performances in each training window
#'   - windows: Explicit training/test starts and ends
#'   - test_metrics: OOS metrics under the same estimator as training
#'   - metric_settings: Initial capital, frozen risk-free input and calendar scale
#'
#' If FALSE, returns only test_results (backward compatibility)
#'
#' @examples
#' price <- data.table::data.table(date=as.POSIXct("2020-01-01", tz="UTC") + 0:499*86400)
#' strategy <- function(price, gain) list(results=data.table::data.table(
#'   Order_ID=1, EntryTime=price$date[1], ExitTime=price$date[2], Returns=gain))
#' out <- optimizeStrategy(price, data.frame(gain=c(1,2)), strategy=strategy,
#'                         train_set_weeks=26, test_set_weeks=26, verbose=FALSE)
#' out$windows
#' @import data.table
#' @importFrom lubridate weeks
#' @export
optimizeStrategy <- function(price, opt_params, initial_equity = 1000,
                             optimize_on = "net_profit",
                             train_set_weeks = 52, test_set_weeks = 26,
                             return_optimization_details = TRUE, strategy = NULL,
                             risk_free = NULL, verbose = TRUE) {

  if (is.null(strategy)) strategy <- get0("my_strat", envir = parent.frame(), inherits = TRUE)
  if (!is.function(strategy)) stop("Supply strategy= as a function (or define legacy my_strat).")
  if (!optimize_on %in% c("net_profit", "profit_factor", "sharpe", "sortino"))
    stop("Unsupported optimize_on metric.")
  if (!nrow(opt_params) || train_set_weeks <= 0 || test_set_weeks <= 0)
    stop("A nonempty grid and positive window lengths are required.")
  price <- data.table::copy(data.table::as.data.table(price))
  data.table::setorder(price, date)
  dates <- price$date
  train_start <- min(dates)
  risk_free_rate <- risk_free
  windows <- list()
  test_metrics <- list()
  metrics <- function(bt, start, end) {
    if (is.null(bt$results) || !nrow(bt$results))
      return(data.table(profit_factor=NA_real_, net_profit=0, sharpe=NA_real_, sortino=NA_real_))
    if (any(bt$results$EntryTime < start | bt$results$ExitTime > end))
      stop("Strategy execution escaped its requested window; bound the execution feed.")
    m <- analyze_performance(bt, initial_equity, risk_free_rate, start=start, end=end)
    data.table(profit_factor=m$profit_factor, net_profit=m$total_pnl, sharpe=m$sharpe, sortino=m$sortino)
  }

  bt_test <- list()
  optimization_history <- list()

  while (train_start < max(dates)) {
    train_end <- train_start + lubridate::weeks(train_set_weeks)
    test_start <- train_end
    test_end <- test_start + lubridate::weeks(test_set_weeks)

    train_set <- price[date >= train_start & date < train_end]
    test_set <- price[date >= test_start & date < test_end]

    if (NROW(test_set) < 100) break

    if (verbose) print(paste0("Starting optimization for ", train_start, " to ", train_end))

    results <- data.table()
    for (iter in 1:NROW(opt_params)) {
      if (verbose) print(paste("Parameter set ", iter, "of", nrow(opt_params)))

      args <- list()
      args[[1]] <- data.table::copy(train_set)
      attr(args[[1]], "execution_end") <- train_end
      args <- c(args, lapply(opt_params, function(xx) xx[iter]))
      bt <- .with_execution_end(train_end, strategy, args)

      out <- metrics(bt, train_start, train_end)
      results <- rbind(results, cbind(data.table(opt_params)[iter, ], out))
    }

    # Retain scores in grid order; ties select the first maximum.
    optimization_history[[as.character(train_start)]] <- results

    if (verbose) print("Results on training set:")
    if (verbose) print(results)

    eligible <- which(!is.na(results[[optimize_on]]))
    if (!length(eligible)) stop("No defined optimisation score in this training window.")
    best <- results[eligible[which.max(results[[optimize_on]][eligible])], ]

    if (verbose) cat(paste("Test set with params:\n",
                paste0(best[1, 1:ncol(opt_params)], collapse = ", "), "\n"))

    args2 <- list()
    args2[[1]] <- data.table::copy(test_set)
    attr(args2[[1]], "execution_end") <- test_end
    args2 <- c(args2, lapply(best[, 1:ncol(opt_params)], function(xx) xx))

    bt <- .with_execution_end(test_end, strategy, args2)
    bt_test[[as.character(test_start)]] <- bt
    test_metrics[[as.character(test_start)]] <- metrics(bt, test_start, test_end)
    windows[[length(windows) + 1L]] <- data.table(
      train_start=train_start, train_end=train_end, test_start=test_start, test_end=test_end)
    if (verbose) print(test_metrics[[as.character(test_start)]])

    train_start <- train_start + lubridate::weeks(test_set_weeks)
  }

  # At the end of the function:
  if(return_optimization_details) {
    # Return recorded windows and consistent metrics with the selected backtests.
    return(list(
      test_results = bt_test,
      optimization_history = optimization_history,
      windows = data.table::rbindlist(windows), test_metrics = test_metrics,
      metric_settings = list(initial_equity=initial_equity, risk_free=risk_free_rate, scale=365L)
    ))
  } else {
    # Legacy return shape requested explicitly.
    return(bt_test)
  }
}

#' Analyze parameter stability across walk-forward windows
#'
#' @description
#' Examines which parameters were selected in each optimization window
#' to assess consistency and robustness of parameter selection.
#'
#' @param optimization_output Output from optimizeStrategy() with return_optimization_details = TRUE
#' @param optimize_on The metric that was used for optimization (default: "net_profit")
#'
#' @return A list containing:
#'   - selected_parameters: data.table showing selected parameters for each window
#'   - stability_metrics: data.table with stability statistics for each parameter
#'   - plots: List of plotly visualizations showing parameter evolution
#'   - summary: Summary statistics including most/least stable parameters
#'
#' @examples
#' price <- data.table::data.table(date=as.POSIXct("2020-01-01", tz="UTC") + 0:499*86400)
#' strategy <- function(price, gain) list(results=data.table::data.table(
#'   Order_ID=1, EntryTime=price$date[1], ExitTime=price$date[2], Returns=gain))
#' out <- optimizeStrategy(price, data.frame(gain=c(1,2)), strategy=strategy,
#'                         train_set_weeks=26, test_set_weeks=26, verbose=FALSE)
#' analyzeParameterStability(out)
#' @import data.table
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats sd median
#' @export
analyzeParameterStability <- function(optimization_output, optimize_on = "net_profit") {

  # Check if we have the required structure
  if (!is.list(optimization_output) ||
      !"test_results" %in% names(optimization_output) ||
      !"optimization_history" %in% names(optimization_output)) {
    stop("ERROR: optimization_output must contain both 'test_results' and 'optimization_history'.\n",
         "Please re-run optimizeStrategy with return_optimization_details = TRUE")
  }

  # Extract optimization history and test results
  opt_history <- optimization_output$optimization_history
  test_results <- optimization_output$test_results

  # Verify optimization history is not NULL or empty
  if (is.null(opt_history) || length(opt_history) == 0) {
    stop("ERROR: optimization_history is empty.\n",
         "Please re-run optimizeStrategy with return_optimization_details = TRUE")
  }

  # Identify parameter columns and performance metrics
  first_window <- opt_history[[1]]
  metric_cols <- c("profit_factor", "net_profit", "sharpe", "sortino")
  param_cols <- names(first_window)[!names(first_window) %in% metric_cols]

  # Extract selected parameters for each window
  selected_params <- data.table()

  for(i in seq_along(opt_history)) {
    window_name <- names(opt_history)[i]
    window_data <- opt_history[[i]]

    # Find the best parameter set based on the optimization metric
    if (optimize_on %in% names(window_data)) {
      best_idx <- which.max(window_data[[optimize_on]])
    } else {
      warning(paste("Optimization metric", optimize_on, "not found. Using net_profit."))
      best_idx <- which.max(window_data$net_profit)
    }

    if(length(best_idx) > 0) {
      best_params <- window_data[best_idx, param_cols, with = FALSE]
      best_params[, Window := as.Date(window_name)]
      best_params[, InSampleMetric := window_data[best_idx][[optimize_on]]]
      best_params[, WindowIndex := i]

      selected_params <- rbind(selected_params, best_params, fill = TRUE)
    }
  }

  # Calculate stability metrics for each parameter
  stability_metrics <- data.table()

  for(param in param_cols) {
    if(param %in% names(selected_params) && is.numeric(selected_params[[param]])) {
      values <- selected_params[[param]]

      # Calculate coefficient of variation only if mean is not zero
      mean_val <- mean(values, na.rm = TRUE)
      cv_val <- if(abs(mean_val) > 0.001) {
        sd(values, na.rm = TRUE) / abs(mean_val)
      } else {
        NA
      }

      param_stats <- data.table(
        Parameter = param,
        Mean = mean_val,
        Median = median(values, na.rm = TRUE),
        SD = sd(values, na.rm = TRUE),
        CV = cv_val,
        Min = min(values, na.rm = TRUE),
        Max = max(values, na.rm = TRUE),
        Range = max(values, na.rm = TRUE) - min(values, na.rm = TRUE),
        UniqueValues = length(unique(values)),
        Changes = sum(diff(values) != 0, na.rm = TRUE),
        ConsistencyScore = 1 - (length(unique(values)) / length(values))
      )

      stability_metrics <- rbind(stability_metrics, param_stats)
    }
  }

  # Create visualization of parameter evolution
  setorder(selected_params, Window)

  plots <- list()
  for(param in param_cols) {
    if(param %in% names(selected_params) && is.numeric(selected_params[[param]])) {
      p <- plot_ly(selected_params, x = ~Window, y = ~get(param),
                   type = 'scatter', mode = 'lines+markers',
                   marker = list(size = 10),
                   line = list(width = 2),
                   name = param) %>%
        layout(title = paste("Evolution of", param, "Across Walk-Forward Windows"),
               xaxis = list(title = "Optimization Window Start Date"),
               yaxis = list(title = param),
               hovermode = 'x unified')

      plots[[param]] <- p
    }
  }

  # Create a combined plot if multiple parameters
  if(length(param_cols) > 1) {
    # Normalize parameters to 0-1 scale for comparison
    selected_params_normalized <- copy(selected_params)
    for(param in param_cols) {
      if(param %in% names(selected_params_normalized) && is.numeric(selected_params_normalized[[param]])) {
        vals <- selected_params_normalized[[param]]
        min_val <- min(vals, na.rm = TRUE)
        max_val <- max(vals, na.rm = TRUE)
        if(max_val > min_val) {
          selected_params_normalized[, (param) := (vals - min_val) / (max_val - min_val)]
        }
      }
    }

    # Create combined plot
    p_combined <- plot_ly()
    for(param in param_cols) {
      if(param %in% names(selected_params_normalized) && is.numeric(selected_params_normalized[[param]])) {
        p_combined <- p_combined %>%
          add_trace(data = selected_params_normalized,
                    x = ~Window,
                    y = ~get(param),
                    type = 'scatter',
                    mode = 'lines+markers',
                    name = param)
      }
    }
    p_combined <- p_combined %>%
      layout(title = "Normalized Parameter Evolution (All Parameters)",
             xaxis = list(title = "Optimization Window Start Date"),
             yaxis = list(title = "Normalized Value (0-1)"),
             hovermode = 'x unified')

    plots[["combined"]] <- p_combined
  }

  return(list(
    selected_parameters = selected_params,
    stability_metrics = stability_metrics,
    plots = plots,
    summary = list(
      n_windows = nrow(selected_params),
      most_stable_param = stability_metrics[which.min(CV)]$Parameter,
      least_stable_param = stability_metrics[which.max(CV)]$Parameter
    )
  ))
}





#' Compare in-sample vs out-of-sample performance
#'
#' @description
#' Analyzes the degradation between optimization (in-sample) performance
#' and actual (out-of-sample) results to assess overfitting.
#'
#' @param optimization_output Output from optimizeStrategy() with return_optimization_details = TRUE
#' @param optimize_on The metric that was used for optimization (default: "net_profit")
#'
#' @details NetProfit_Degradation and Performance_Ratio compare profit per
#' calendar day, using recorded window durations. Raw totals remain in
#' Raw_NetProfit_Degradation and Raw_Performance_Ratio. Missing durations
#' produce NA normalised fields. Partial exits count once per Order_ID.
#' @return A list containing:
#'   - performance_comparison: data.table comparing IS and OOS metrics for each window
#'   - summary_stats: Summary statistics including average degradation and correlation
#'   - plots: List of visualizations (scatter plot, time series, degradation histogram)
#'   - recommendation: Reminder of the limits of descriptive comparisons
#'
#' @examples
#' price <- data.table::data.table(date=as.POSIXct("2020-01-01", tz="UTC") + 0:499*86400)
#' strategy <- function(price, gain) list(results=data.table::data.table(
#'   Order_ID=1, EntryTime=price$date[1], ExitTime=price$date[2], Returns=gain))
#' out <- optimizeStrategy(price, data.frame(gain=c(1,2)), strategy=strategy,
#'                         train_set_weeks=26, test_set_weeks=26, verbose=FALSE)
#' compareISOS(out)
#' @import data.table
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats cor median
#' @export
compareISOS <- function(optimization_output, optimize_on = "net_profit") {
  h <- optimization_output$optimization_history
  tests <- optimization_output$test_results
  windows <- optimization_output$windows
  if (!length(h) || length(h) != length(tests))
    stop("Matching optimization_history and test_results are required.")
  settings <- optimization_output$metric_settings
  if (is.null(settings)) settings <- list(initial_equity=1000, risk_free=NULL)
  rows <- lapply(seq_along(h), function(i) {
    scores <- data.table::as.data.table(h[[i]])
    if (!optimize_on %in% names(scores)) stop("The selected optimisation metric is absent.")
    best <- which.max(scores[[optimize_on]])
    if (!length(best)) stop("No defined optimisation score in a stored window.")
    is <- scores[best]
    bt <- tests[[i]]
    has_windows <- !is.null(windows) && nrow(windows) == length(h)
    oos <- optimization_output$test_metrics[[i]]
    if (is.null(oos)) {
      if (is.null(bt$results) || !nrow(bt$results)) {
        oos <- data.table(net_profit=0, profit_factor=NA_real_, sharpe=NA_real_, sortino=NA_real_)
      } else {
        m <- analyze_performance(bt, initial_equity=settings$initial_equity, risk_free=settings$risk_free)
        oos <- data.table(net_profit=m$total_pnl, profit_factor=m$profit_factor, sharpe=m$sharpe, sortino=m$sortino)
      }
    }
    trades <- if (is.null(bt$results)) data.table() else data.table::as.data.table(bt$results)
    if (nrow(trades)) {
      if (any(!is.finite(trades$Returns))) stop("Unknown OOS returns cannot be discarded.")
      grouped <- if ("Order_ID" %in% names(trades)) trades[, .(pnl=sum(Returns)), by=Order_ID] else data.table(pnl=trades$Returns)
      count <- nrow(grouped); win <- mean(grouped$pnl > 0)
    } else { count <- 0L; win <- NA_real_ }
    train_days <- if (has_windows) as.numeric(as.Date(windows$train_end[i])-as.Date(windows$train_start[i])) else NA_real_
    test_days <- if (has_windows) as.numeric(as.Date(windows$test_end[i])-as.Date(windows$test_start[i])) else NA_real_
    is_rate <- is$net_profit/train_days
    os_rate <- oos$net_profit/test_days
    row <- data.table(Window=as.Date(names(h)[i]), WindowIndex=i,
      IS_NetProfit=is$net_profit, IS_ProfitFactor=is$profit_factor,
      IS_Sharpe=is$sharpe, IS_Sortino=is$sortino,
      OOS_NetProfit=oos$net_profit, OOS_ProfitFactor=oos$profit_factor,
      OOS_Sharpe=oos$sharpe, OOS_WinRate=win, OOS_NumTrades=count,
      IS_Days=train_days, OOS_Days=test_days, IS_ProfitPerDay=is_rate, OOS_ProfitPerDay=os_rate,
      Raw_NetProfit_Degradation=if (is$net_profit != 0) (is$net_profit-oos$net_profit)/abs(is$net_profit) else NA_real_,
      Raw_Performance_Ratio=if (is$net_profit != 0) oos$net_profit/is$net_profit else NA_real_,
      NetProfit_Degradation=if (is.finite(is_rate) && is_rate != 0) (is_rate-os_rate)/abs(is_rate) else NA_real_,
      Performance_Ratio=if (is.finite(is_rate) && is_rate != 0) os_rate/is_rate else NA_real_)
    param_cols <- setdiff(names(scores), c("profit_factor", "net_profit", "sharpe", "sortino"))
    cbind(row, is[, ..param_cols])
  })
  d <- data.table::rbindlist(rows, fill=TRUE)
  if (all(is.na(d$IS_Days))) warning("Window durations unavailable: normalised comparisons are NA; raw totals are not like-for-like.")
  finite_mean <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
  finite_median <- function(x) if (any(is.finite(x))) stats::median(x[is.finite(x)]) else NA_real_
  ok <- is.finite(d$IS_ProfitPerDay) & is.finite(d$OOS_ProfitPerDay)
  corr <- if (sum(ok) > 1 && stats::sd(d$IS_ProfitPerDay[ok]) > 0 && stats::sd(d$OOS_ProfitPerDay[ok]) > 0)
    stats::cor(d$IS_ProfitPerDay[ok], d$OOS_ProfitPerDay[ok]) else NA_real_
  summary <- list(avg_is_performance=finite_mean(d$IS_NetProfit),
    avg_oos_performance=finite_mean(d$OOS_NetProfit),
    avg_degradation=finite_mean(d$NetProfit_Degradation), median_degradation=finite_median(d$NetProfit_Degradation),
    pct_profitable_oos=mean(d$OOS_NetProfit > 0),
    pct_worse_oos=finite_mean(as.numeric(d$OOS_ProfitPerDay < d$IS_ProfitPerDay)), correlation=corr)
  plots <- list(
    scatter=plotly::layout(plotly::plot_ly(d, x=~IS_ProfitPerDay, y=~OOS_ProfitPerDay,
      type="scatter", mode="markers"), xaxis=list(title="IS profit / calendar day"), yaxis=list(title="OOS profit / calendar day")),
    time_series=plotly::layout(plotly::add_trace(plotly::plot_ly(d, x=~Window, y=~IS_ProfitPerDay,
      type="scatter", mode="lines+markers", name="IS / day"), y=~OOS_ProfitPerDay, name="OOS / day"),
      yaxis=list(title="Profit / calendar day")),
    degradation=plotly::layout(plotly::plot_ly(x=d$NetProfit_Degradation*100, type="histogram"),
      xaxis=list(title="Change in profit per day (%)")))
  list(performance_comparison=d, summary_stats=summary, plots=plots,
    recommendation="Descriptive comparison only. Profit per calendar day adjusts unequal window lengths; it is not a test of robustness or evidence against overfitting.")
}
