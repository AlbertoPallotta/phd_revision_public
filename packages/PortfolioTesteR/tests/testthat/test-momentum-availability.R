test_that("propagated endpoint momentum starts at lookback plus one", {
  for (lb in c(1L, 6L, 12L)) {
    p <- data.frame(Date = as.Date("2020-01-01") + 0:15,
                    A = 100 * 1.02^(0:15), Zero = rep(50, 16))
    before <- serialize(p, NULL)
    m <- calc_momentum(p, lb, na_policy = "propagate")
    expect_identical(serialize(p, NULL), before)
    expect_identical(m$Date, p$Date)
    expect_identical(names(m), names(p))
    expect_true(all(is.na(m$A[seq_len(lb)])))
    expect_equal(m$A[(lb + 1L):16], rep(1.02^lb - 1, 16 - lb), tolerance = 1e-12)
    expect_equal(m$Zero[(lb + 1L):16], rep(0, 16 - lb))
    expect_identical(which(is.finite(m$A))[1], lb + 1L)
  }
})

test_that("availability is per endpoint and asset, including late starts", {
  p <- data.frame(Date = as.Date("2020-01-01") + 0:5,
    A = c(100, NA, 121, 133.1, 146.41, 161.051),
    Late = c(NA, NA, NA, 10, 11, 12.1))
  m <- calc_momentum(p, 2, na_policy = "propagate")
  expect_equal(m$A, c(NA, NA, .21, NA, .21, .21), tolerance = 1e-12)
  expect_equal(m$Late, c(NA, NA, NA, NA, NA, .21), tolerance = 1e-12)
  # The missing intermediate A price does not invalidate its row-3 endpoints.
  expect_equal(m$A[3], .21)
  s <- filter_top_n(m, 1)
  expect_true(all(s$Late[1:5] == 0))
  expect_true(all(s$A[c(1, 2, 4)] == 0))
  expect_equal(rowSums(as.matrix(s[, -1])), c(0, 0, 1, 0, 1, 1))
})

test_that("nonfinite endpoints and zero denominators stay unavailable", {
  p <- data.frame(Date = as.Date("2020-01-01") + 0:10,
    A = c(100, NA, 120, Inf, 140, 0, 150, 150, NaN, -Inf, 200))
  m <- calc_momentum(p, 1, na_policy = "propagate")
  expect_equal(m$A, c(NA, NA, NA, NA, NA, -1, NA, 0, NA, NA, NA))
  huge <- data.frame(Date = p$Date[1:2], A = c(-1e308, 1e308))
  expect_true(all(is.na(calc_momentum(huge, 1, na_policy = "propagate")$A)))
  expect_error(calc_momentum(p, na_policy = "unknown"), "arg")
})

test_that("legacy default retains zero substitution and genuine zero returns", {
  p <- data.frame(Date = as.Date("2020-01-01") + 0:4, A = c(100, 100, NA, 200, 220))
  a <- calc_momentum(p, 1)
  b <- calc_momentum(p, 1, na_policy = "legacy_zero")
  expect_identical(a, b)
  expect_equal(a$A, c(0, 0, 0, 0, .1))
  expect_equal(calc_momentum(p, 1, na_policy = "propagate")$A, c(NA, 0, NA, NA, .1))
})

test_that("unavailable rows are unselected but real zero signals remain valid", {
  p <- data.table::data.table(Date = as.Date("2020-01-01") + 0:5,
                             A = rep(100, 6), B = rep(200, 6))
  before <- serialize(p, NULL)
  m <- calc_momentum(p, 2, na_policy = "propagate")
  s <- filter_top_n(m, 1)
  w <- weight_equally(s)
  expect_equal(rowSums(as.matrix(s[, -1])), c(0, 0, 1, 1, 1, 1))
  expect_equal(rowSums(as.matrix(w[, -1])), c(0, 0, 1, 1, 1, 1))
  threshold <- filter_above(m, value = -.1)
  expect_true(all(as.matrix(threshold[1:2, -1]) == 0))
  expect_true(all(as.matrix(threshold[3:6, -1]) == 1))
  expect_identical(serialize(p, NULL), before)
})

test_that("sector ranking retains unavailable momentum rows", {
  p <- data.frame(Date = as.Date("2020-01-01") + 0:5,
    A = 100 * 1.02^(0:5), B = 100 * 1.01^(0:5), C = rep(100, 6))
  m <- calc_momentum(p, 2, na_policy = "propagate")
  sectors <- data.frame(Symbol = c("A", "B", "C"), Sector = rep("Example", 3))
  r <- rank_within_sector(m, sector_mapping = sectors)
  expect_true(all(is.na(as.matrix(r[1:2, -1]))))
  s <- filter_above(r, value = 70)
  expect_true(all(as.matrix(s[1:2, -1]) == 0))
  expect_true(all(s$A[3:6] == 1))
  expect_true(all(s$C[3:6] == 0))
})

test_that("opt-in momentum backtest keeps cash until history is available", {
  p <- data.table::data.table(Date = as.Date("2020-01-01") + 7 * 0:6,
    A = c(100, 110, 121, 133.1, 146.41, 161.051, 177.1561), B = rep(100, 7))
  before <- serialize(p, NULL)
  m <- calc_momentum(p, 2, na_policy = "propagate")
  w <- weight_equally(filter_top_n(m, 1))
  bt <- run_backtest(p, w, initial_capital = 10000)
  expect_identical(bt$first_trade_date, p$Date[3])
  expect_equal(bt$warmup_periods, 2)
  expect_true(all(as.matrix(bt$positions[1:2, -1]) == 0))
  expect_equal(bt$cash[1:2], rep(10000, 2))
  expect_equal(bt$portfolio_values[1:3], rep(10000, 3))
  expect_true(all(bt$transactions$Date >= p$Date[3]))
  expect_equal(bt$positions$A[3], floor(10000 / 121))
  expect_equal(bt$cash[3], 10000 - floor(10000 / 121) * 121)
  expect_equal(rowSums(as.matrix(bt$positions[, -1]) * as.matrix(p[, -1])) + bt$cash,
               bt$portfolio_values, tolerance = 1e-8)
  expect_identical(serialize(p, NULL), before)
})
