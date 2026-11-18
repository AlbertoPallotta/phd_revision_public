# Canonical B3 recipe; no historical evaluations occur when this file is sourced.
# Full runners/recording/figures are in the separate thesis-reproduction companion.
thesis_b3_config <- function() {
  list(grid = list(lookback = c(8L, 12L, 16L, 20L), n_top = c(5L, 10L, 15L)),
       wf_grid = list(lookback = c(8L, 12L, 26L), n_top = c(5L, 10L)),
       is_periods = 52L, oos_periods = 26L, step = 26L, warmup_periods = 26L,
       initial_capital = 100000, momentum_policy = "propagate",
       objective = "metric_sharpe", precompute_all = TRUE, light_mode = TRUE)
}

thesis_b3_panels <- function(prices, params) {
  # This consumes the actual supplied price slice, including the OOS history tail.
  p <- data.table::copy(prices)
  m <- PortfolioTesteR::calc_momentum(p, lookback = params$lookback,
                                     na_policy = "propagate")
  selected <- PortfolioTesteR::filter_top_n(m, n = params$n_top)
  list(momentum = m, selected = selected,
       weights = PortfolioTesteR::weight_equally(selected))
}

thesis_b3_weights <- function(prices, params, ...) {
  # The optional indicator_cache / precomputed returns are deliberately unused.
  # IS begins within each 52-row slice. OOS must receive the explicit 26-row tail.
  thesis_b3_panels(prices, params)$weights
}
