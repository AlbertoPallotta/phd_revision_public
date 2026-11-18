# Dedicated thesis A1: selection union followed by allocation composition.
# Canonical recipe; the separate reproduction runner sources this installed file.
# This is not the weekly AND/Top-10/equal example_multi_factor.R.
# All execution is same-row, gross, whole-share, with zero-interest residual cash.

thesis_a1_legs <- function(momentum, sigma, n = 5L) {
  stopifnot(identical(names(momentum), names(sigma)),
            identical(momentum$Date, sigma$Date), nrow(momentum) == nrow(sigma))
  symbols <- setdiff(names(momentum), "Date")
  mom <- data.table::copy(data.table::as.data.table(momentum))
  vol <- data.table::copy(data.table::as.data.table(sigma))
  for (s in symbols) {
    # Valid zero/negative momentum is still eligible for the Top-N ranking.
    data.table::set(mom, which(!is.finite(mom[[s]])), s, NA_real_)
    data.table::set(vol, which(!is.finite(vol[[s]]) | vol[[s]] <= 0), s, NA_real_)
  }
  selected_momentum <- PortfolioTesteR::filter_top_n(mom, n = n)
  selected_volatility <- PortfolioTesteR::filter_top_n(vol, n = n, ascending = TRUE)
  selected_union <- PortfolioTesteR::combine_filters(
    selected_momentum, selected_volatility, op = "or")
  reciprocal <- data.table::copy(vol)
  for (s in symbols) {
    eligible <- selected_union[[s]] > 0 & is.finite(vol[[s]]) & vol[[s]] > 0
    q <- rep(0, nrow(vol))
    q[eligible] <- 1 / vol[[s]][eligible]
    if (any(!is.finite(q[eligible]) | q[eligible] <= 0))
      stop("A1 reciprocal outside finite positive numeric range")
    data.table::set(reciprocal, j = s, value = q)
  }
  # Fail rather than silently floor, cap or overflow a normalising denominator.
  qsum <- rowSums(as.matrix(reciprocal[, symbols, with = FALSE]))
  m <- as.matrix(mom[, symbols, with = FALSE])
  u <- as.matrix(selected_union[, symbols, with = FALSE])
  m[!is.finite(m) | m <= 0 | u == 0] <- 0
  if (any(!is.finite(qsum)) || any(!is.finite(rowSums(m))))
    stop("A1 allocation denominator is not finite")
  weights_momentum <- PortfolioTesteR::weight_by_signal(selected_union, mom)
  weights_volatility <- PortfolioTesteR::weight_by_signal(selected_union, reciprocal)
  # Both active => 60/40; one active => 100% survivor; neither => zero targets.
  weights <- PortfolioTesteR::combine_weights(
    list(weights_momentum, weights_volatility), weights = c(0.6, 0.4))
  w <- as.matrix(weights[, symbols, with = FALSE])
  stopifnot(all(is.finite(w)), all(w >= 0),
            all(abs(rowSums(w)) < 1e-12 | abs(rowSums(w) - 1) < 1e-12))
  list(momentum = mom, sigma_eligible = vol,
       selected_momentum = selected_momentum,
       selected_volatility = selected_volatility, selected_union = selected_union,
       reciprocal = reciprocal, weights_momentum = weights_momentum,
       weights_volatility = weights_volatility, weights = weights)
}

thesis_multifactor_a1 <- function(weekly = NULL, daily = NULL, execute = TRUE) {
  samples <- new.env(parent = emptyenv())
  if (is.null(weekly) || is.null(daily))
    utils::data(list = c("sample_prices_weekly", "sample_prices_daily"),
                package = "PortfolioTesteR", envir = samples)
  if (is.null(weekly)) weekly <- samples$sample_prices_weekly
  if (is.null(daily)) daily <- samples$sample_prices_daily
  weekly <- data.table::copy(data.table::as.data.table(weekly))
  daily <- data.table::copy(data.table::as.data.table(daily))
  symbols <- setdiff(names(weekly), "Date")
  stopifnot(identical(names(weekly), names(daily)))
  for (p in list(weekly, daily)) {
    stopifnot(inherits(p$Date, "Date"), !anyNA(p$Date),
              !anyDuplicated(p$Date), all(diff(p$Date) > 0),
              all(is.finite(as.matrix(p[, symbols, with = FALSE]))),
              all(as.matrix(p[, symbols, with = FALSE]) > 0))
  }
  grouped <- PortfolioTesteR::convert_to_nweeks(
    data.table::copy(weekly), n = 4, method = "last")
  momentum <- PortfolioTesteR::calc_momentum(
    data.table::copy(grouped), lookback = 12, na_policy = "propagate")
  daily_sigma <- PortfolioTesteR::calc_rolling_volatility(
    data.table::copy(daily), lookback = 60, method = "std")
  sigma <- PortfolioTesteR::align_to_timeframe(
    data.table::copy(daily_sigma), grouped$Date, method = "forward_fill")
  source_idx <- findInterval(as.numeric(grouped$Date), as.numeric(daily$Date))
  source_dates <- as.Date(rep(NA_real_, nrow(grouped)), origin = "1970-01-01")
  ok <- source_idx > 0
  source_dates[ok] <- as.Date(daily$Date[source_idx[ok]])
  stopifnot(all(is.na(source_dates) | source_dates <= grouped$Date))
  parts <- thesis_a1_legs(momentum, sigma)
  parts$prices <- grouped
  parts$daily_sigma <- daily_sigma
  parts$sigma_aligned <- sigma
  parts$alignment <- data.frame(Date = as.Date(grouped$Date),
                                source_row = source_idx, source_date = source_dates)
  parts$backtest <- if (execute) PortfolioTesteR::run_backtest(
    prices = data.table::copy(grouped), weights = data.table::copy(parts$weights),
    initial_capital = 100000, name = "Thesis A1 multifactor", verbose = FALSE) else NULL
  parts
}

# Rscript execution runs exactly one A1 backtest; sys.source only defines recipes.
if (sys.nframe() == 0L) {
  result <- thesis_multifactor_a1()
  print(result$backtest)
}
