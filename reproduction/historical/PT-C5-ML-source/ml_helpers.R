## ============================
## Helper 1: multi-horizon runner
## ============================
ml_backtest_multi <- function(features_list, prices_weekly, horizons,
                              fit_fn, predict_fn, schedule,
                              transform = "zscore",
                              selection = list(top_k = 20L),
                              weighting = list(method = "softmax", temperature = 12),
                              caps = list(max_per_symbol = 0.10),
                              group_mode = c("pooled","per_group"),
                              group_map = NULL,
                              initial_capital = 1e5,
                              name_prefix = "",
                              seed = NULL,
                              ...) {

  group_mode <- match.arg(group_mode)
  keys <- paste0("H", horizons, "w")
  res  <- stats::setNames(vector("list", length(horizons)), keys)

  for (i in seq_along(horizons)) {
    h <- horizons[i]
    if (!is.null(seed)) set.seed(seed)

    Yh <- PortfolioTesteR::make_labels(
      prices  = prices_weekly,
      horizon = h,
      type    = "log"
    )

    args_common <- list(
      features_list   = features_list,
      labels          = Yh,
      fit_fn          = fit_fn,
      predict_fn      = predict_fn,
      schedule        = schedule,
      transform       = transform,
      selection       = selection,
      weighting       = weighting,
      caps            = caps,
      prices          = prices_weekly,
      initial_capital = initial_capital
    )

    res[[i]] <-
      if (group_mode == "pooled") {
        do.call(PortfolioTesteR::ml_backtest,
                c(args_common,
                  list(group = "pooled",
                       name  = sprintf("%sPooled (%dw)", name_prefix, h)),
                  list(...)))
      } else {
        stopifnot(!is.null(group_map),
                  all(c("Symbol","Group") %in% names(group_map)))
        do.call(PortfolioTesteR::ml_backtest,
                c(args_common,
                  list(group = "per_group", group_map = group_map,
                       name  = sprintf("%sSector-neutral (%dw)", name_prefix, h)),
                  list(...)))
      }
  }

  attr(res, "config") <- list(
    horizons   = horizons,
    schedule   = schedule,
    selection  = selection,
    weighting  = weighting,
    caps       = caps,
    transform  = transform,
    group_mode = group_mode,
    seed       = seed,
    name_prefix = name_prefix
  )
  res
}


## ============================
## Helper 2: results collector (robust)
## ============================
pt_collect_results <- function(bt_pooled_list,
                               bt_neutral_list,
                               weekly_prices,              # kept for backwards compat
                               horizons,
                               split_date,
                               cost_bps = c(5,10,15,20,25,50),
                               freq = 52,
                               # --- new, all default to current behaviour ---
                               prices = NULL,              # alias for weekly_prices
                               ic_roll_window = 26L,
                               mask_scores_to_decision_dates = TRUE,
                               cost_model = function(turnover, bps) (bps/10000) * turnover) {

  prices_in <- if (!is.null(prices)) prices else weekly_prices

  # ---- internal helpers ----
  scores_oos_only_ <- function(scores_dt, weights_wide) {
    if (!mask_scores_to_decision_dates) return(data.table::as.data.table(scores_dt))
    scores_oos_only(scores_dt, weights_wide)  # uses the exported helper above
  }

  turnover_series_ <- function(weights_wide) {
    W <- data.table::as.data.table(weights_wide)
    if (!nrow(W)) return(data.table::data.table(Date = as.Date(character()), turnover = numeric()))
    if (is.numeric(W$Date)) W[, Date := as.Date(Date, origin = "1970-01-01")]
    sy <- setdiff(names(W), "Date")
    if (!length(sy)) return(data.table::data.table(Date = as.Date(character()), turnover = numeric()))
    Wn <- W[rowSums(as.matrix(W[, ..sy]) != 0) > 0]
    if (!nrow(Wn)) return(data.table::data.table(Date = as.Date(character()), turnover = numeric()))
    L  <- data.table::melt(Wn, id.vars = "Date", variable.name = "Symbol", value.name = "w")
    data.table::setorder(L, Date, Symbol)
    L[, w_lag := data.table::shift(w, 1L), by = Symbol]
    L[, .(turnover = 0.5 * sum(abs(w - data.table::fifelse(is.na(w_lag), 0, w_lag)))), by = Date][!is.na(turnover)]
  }

  as_dt_metrics_ <- function(M) {
    if (data.table::is.data.table(M)) return(M)
    if (is.data.frame(M))            return(data.table::as.data.table(M))
    if (is.list(M))                  return(data.table::as.data.table(M))
    data.table::as.data.table(as.list(M))
  }

  perf_splits_ <- function(bt, split_date, freq) {
    b <- bt$backtest
    dates <- as.Date(b$dates, origin = "1970-01-01")
    rvec  <- if (!is.null(b$returns)) as.numeric(b$returns) else as.numeric(b$active_returns)
    if (!length(rvec)) return(data.table::data.table(Slice = character()))
    R <- data.table::data.table(Date = dates[seq_along(rvec)], Ret = rvec)

    summarize <- function(D) {
      M <- PortfolioTesteR::perf_metrics(data.frame(Date = D$Date, ret = D$Ret), freq = freq)
      as_dt_metrics_(M)
    }
    full <- summarize(R);                      full[,  Slice := "Full"]
    pre  <- summarize(R[Date <  split_date]);  pre[,   Slice := "Pre"]
    post <- summarize(R[Date >= split_date]);  post[,  Slice := "Post"]
    data.table::rbindlist(list(full, pre, post), use.names = TRUE, fill = TRUE)
  }

  cost_sweep_ <- function(returns_dt, turnover_dt, cost_bps, freq) {
    if (!nrow(returns_dt)) return(data.table::data.table())
    D <- base::merge(data.table::copy(returns_dt),
                     data.table::copy(turnover_dt),
                     by = "Date", all.x = TRUE)
    D[is.na(turnover), turnover := 0]

    out <- lapply(cost_bps, function(bps) {
      D[, Ret_net := Ret - cost_model(turnover, bps)]
      g <- PortfolioTesteR::perf_metrics(data.frame(Date = D$Date, ret = D$Ret),      freq = freq)
      n <- PortfolioTesteR::perf_metrics(data.frame(Date = D$Date, ret = D$Ret_net),  freq = freq)
      gdt <- as_dt_metrics_(g)[, `:=`(Cost_bps = bps, Net = FALSE)]
      ndt <- as_dt_metrics_(n)[, `:=`(Cost_bps = bps, Net = TRUE)]
      list(gdt, ndt)
    })
    data.table::rbindlist(unlist(out, recursive = FALSE), use.names = TRUE, fill = TRUE)
  }

  # ---- sanity ----
  need_names <- paste0("H", horizons, "w")
  stopifnot(all(need_names %in% names(bt_pooled_list)),
            all(need_names %in% names(bt_neutral_list)))

  out <- stats::setNames(vector("list", length(horizons)), need_names)

  # ---- loop ----
  for (h in horizons) {
    key <- paste0("H", h, "w")
    btP <- bt_pooled_list[[key]]
    btN <- bt_neutral_list[[key]]

    Yh <- PortfolioTesteR::make_labels(prices = prices_in, horizon = h, type = "log")

    covP <- if (nrow(btP$scores)) PortfolioTesteR::coverage_by_date(btP$scores) else data.table::data.table()
    covN <- if (nrow(btN$scores)) PortfolioTesteR::coverage_by_date(btN$scores) else data.table::data.table()
    icP  <- if (nrow(btP$scores)) PortfolioTesteR::ml_ic_series_on_scores(btP$scores, Yh) else data.table::data.table()
    icN  <- if (nrow(btN$scores)) PortfolioTesteR::ml_ic_series_on_scores(btN$scores, Yh) else data.table::data.table()

    S_P_oos <- scores_oos_only_(btP$scores, btP$weights)
    S_N_oos <- scores_oos_only_(btN$scores, btN$weights)

    icrollP_oos <- if (nrow(S_P_oos)) PortfolioTesteR::ml_plot_ic_roll(S_P_oos, Yh, window = ic_roll_window) else data.table::data.table()
    icrollN_oos <- if (nrow(S_N_oos)) PortfolioTesteR::ml_plot_ic_roll(S_N_oos, Yh, window = ic_roll_window) else data.table::data.table()

    perfP <- perf_splits_(btP, split_date = split_date, freq = freq)
    perfN <- perf_splits_(btN, split_date = split_date, freq = freq)

    turnsN <- turnover_series_(btN$weights)

    bN <- btN$backtest
    datesN <- as.Date(bN$dates, origin = "1970-01-01")
    rvecN  <- if (!is.null(bN$returns)) as.numeric(bN$returns) else as.numeric(bN$active_returns)
    retN   <- if (length(rvecN)) data.table::data.table(Date = datesN[seq_along(rvecN)], Ret = rvecN) else data.table::data.table()

    costsN <- cost_sweep_(retN, turnsN, cost_bps = cost_bps, freq = freq)

    out[[key]] <- list(
      bt_pooled            = btP,
      bt_neutral           = btN,
      coverage             = list(pooled = covP, neutral = covN),
      ic_series            = list(pooled = icP, neutral = icN),
      icroll_oos_26w       = list(pooled = icrollP_oos, neutral = icrollN_oos),
      masked_scores        = list(pooled = S_P_oos, neutral = S_N_oos),
      perf_tables          = list(pooled = perfP, neutral = perfN),
      turnover_neutral     = turnsN,
      cost_sweep_neutral   = costsN
    )
  }

  attr(out, "config") <- list(
    horizons           = horizons,
    split_date         = split_date,
    cost_bps           = cost_bps,
    freq               = freq,
    ic_roll_window     = ic_roll_window,
    masked_to_decision = mask_scores_to_decision_dates
  )
  out
}





# Small helper used in the chapter to mask scores to decision weeks (OOS-only)
# Put this near the top of ml_helpers.R
scores_oos_only <- function(scores_dt, weights_wide) {
  S <- data.table::as.data.table(scores_dt)
  W <- data.table::as.data.table(weights_wide)
  if (!nrow(S) || !nrow(W)) return(S[0])
  if (is.numeric(S$Date)) S[, Date := as.Date(Date, origin = "1970-01-01")]
  if (is.numeric(W$Date)) W[, Date := as.Date(Date, origin = "1970-01-01")]
  sy <- setdiff(names(W), "Date")
  if (!length(sy)) return(S[0])
  ddates <- W$Date[rowSums(as.matrix(W[, ..sy]) != 0) > 0]
  if (!length(ddates)) return(S[0])
  S[!(Date %in% ddates), (setdiff(names(S), "Date")) := NA][]
}
