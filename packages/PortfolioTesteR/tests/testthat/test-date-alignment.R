test_that("panel intersections preserve dates and align sparse inputs", {
  dates <- as.Date("2020-01-01") + 0:5
  first <- data.frame(Date = rev(dates), A = 6:1, B = 16:11)
  second <- data.frame(Date = dates[c(2, 4, 6)], A = c(20, 40, 60))
  labels <- data.frame(Date = dates[c(1, 2, 4, 5, 6)], A = c(1, 2, 4, 5, 6))
  expected <- dates[c(2, 4, 6)]
  before <- list(first, second, labels)

  common <- PortfolioTesteR:::.roll_align_common(before)
  expect_identical(common$dates, expected)
  expect_identical(common$symbols, "A")
  for (panel in common$panels) expect_identical(panel$Date, expected)
  expect_identical(common$panels[[1]]$A, c(2L, 4L, 6L))

  aligned <- PortfolioTesteR:::.align_features_labels(list(first, second), labels)
  expect_identical(aligned$dates, expected)
  for (panel in aligned$features) expect_identical(panel$Date, expected)
  expect_identical(aligned$labels$Date, expected)
  expect_identical(list(first, second, labels), before)
})

test_that("rolling sequence alignment retains the common calendar", {
  dates <- as.Date("2020-01-01") + 0:13
  features <- list(
    one = data.frame(Date = dates, A = seq_along(dates)),
    two = data.frame(Date = dates[2:13], A = (2:13) / 10)
  )
  labels <- data.frame(Date = dates[1:13], A = (1:13) / 100)
  scores <- roll_fit_predict_seq(
    features, labels, steps = 2L, horizon = 1L,
    fit_fn = function(X, y) mean(y),
    predict_fn = function(model, X) rep(model, nrow(X)),
    is_periods = 6L, oos_periods = 2L, step = 2L,
    min_train_samples = 2L
  )
  expect_identical(scores$Date, dates[2:13])
  expect_true(all(is.na(scores$A[1:6])))
  expect_true(any(is.finite(scores$A)))
})
