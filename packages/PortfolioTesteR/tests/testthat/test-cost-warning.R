test_that("positive cost requests warn accurately and preserve gross returns", {
  dates <- as.Date("2020-01-01") + 0:3
  prices <- data.frame(Date = dates, A = c(100, 110, 105, 120),
                       B = c(100, 95, 110, 115))
  weights <- data.frame(Date = dates, A = c(1, 0, 0.5, 1),
                        B = c(0, 1, 0.5, 0))
  expect_no_warning(gross <- portfolio_returns(weights, prices, cost_bps = 0))
  expect_warning(
    requested_net <- portfolio_returns(weights, prices, cost_bps = 10),
    "transaction-cost deduction is not operational.*returning gross returns.*pt_collect_results"
  )
  expect_identical(requested_net, gross)
})

test_that("cost-forwarding wrappers retain warning-based execution", {
  data(sample_prices_weekly)
  features <- list(momentum = panel_lag(calc_momentum(sample_prices_weekly, 12L), 1L))
  labels <- make_labels(sample_prices_weekly, horizon = 4L, type = "log")
  model <- ml_make_model("linear")
  schedule <- list(is = 104L, oos = 4L, step = 4L)
  grid <- list(top_k = 5L, temperature = 12, method = "rank", transform = "none")
  warning_text <- "transaction-cost deduction is not operational.*returning gross returns.*pt_collect_results"

  expect_warning(tuned <- tune_ml_backtest(
    features, labels, sample_prices_weekly, model$fit, model$predict,
    schedule = schedule, grid = grid, caps = list(max_per_symbol = 1), cost_bps = 10
  ), warning_text)
  expect_s3_class(tuned, "data.table")
  expect_equal(nrow(tuned), 1L)

  expect_warning(sweep <- wf_sweep_tabular(
    features, labels, sample_prices_weekly, model$fit, model$predict,
    schedule = schedule, grid = grid, caps = list(max_per_symbol = 1),
    cost_bps = 10, max_windows = 1L
  ), warning_text)
  expect_s3_class(sweep, "data.table")
  expect_equal(nrow(sweep), 1L)
})
