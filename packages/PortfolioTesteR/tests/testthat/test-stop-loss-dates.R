test_that("triggered stops retain Date class before and after transaction binding", {
  prices <- data.table::data.table(Date = as.Date(c("2020-01-01", "2020-01-08")), A = c(100, 90))
  weights <- data.table::data.table(Date = prices$Date, A = c(1, 1))
  daily <- data.table::data.table(Date = as.Date(c("2020-01-02", "2020-01-03", "2020-01-08")),
                      A = c(98, 80, 90))

  # Observe the actual transaction rows before rbindlist can coerce their dates.
  # A private function copy avoids changing any package namespace or dependency.
  recorded <- NULL
  run <- run_backtest
  environment(run) <- new.env(parent = environment(run))
  environment(run)$rbindlist <- function(l, ...) {
    recorded <<- l
    data.table::rbindlist(l, ...)
  }
  result <- run(prices, weights, initial_capital = 1000, stop_loss = 0.15,
                stop_monitoring_prices = daily)
  stops <- result$transactions[result$transactions$Type == "StopLoss", ]

  expect_true(all(vapply(recorded, function(row) inherits(row$Date, "Date"), logical(1))))
  expect_s3_class(result$transactions$Date, "Date")
  expect_equal(nrow(stops), 1L)
  expect_identical(stops$Date, as.Date("2020-01-03"))
  expect_equal(unname(stops$Shares), -10)
  expect_equal(unname(stops$Price), 80)
  expect_equal(result$portfolio_values, c(1000, 800))
})
