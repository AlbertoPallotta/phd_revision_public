test_that("linear factories do not probe optional backends", {
  local_mocked_bindings(
    .package = "PortfolioTesteR",
    .pt_ml_backend_available = function(...) stop("unexpected backend probe"),
    .pt_keras_available = function(...) stop("unexpected Keras probe")
  )
  expect_no_condition(tabular <- ml_make_model("linear"))
  expect_no_condition(sequence <- ml_make_seq_model("linear"))
  expect_identical(sequence$requested_type, "linear")
  expect_identical(sequence$actual_type, "linear")
  X <- cbind(x = 1:8, z = c(1, 0, 2, 0, 3, 0, 4, 0))
  y <- 2 + 3 * X[, 1] - X[, 2]
  expect_equal(tabular$predict(tabular$fit(X, y), X), y)
  expect_equal(sequence$predict(sequence$fit(X, y), X), y)
})

test_that("missing tabular backends give actionable errors", {
  local_mocked_bindings(.package = "PortfolioTesteR", .pt_ml_backend_available = function(...) FALSE)
  packages <- c(ridge = "glmnet", rf = "ranger", xgboost = "xgboost")
  for (type in names(packages)) {
    expect_error(ml_make_model(type), paste0("optional package '", packages[[type]], "'"))
    expect_error(ml_make_model(type), "install.packages", fixed = TRUE)
    expect_error(ml_make_model(type), "ml_make_model('linear')", fixed = TRUE)
  }
})

test_that("absent and unusable sequence backends announce the same linear fallback", {
  X <- cbind(x = 1:8, z = c(1, 0, 2, 0, 3, 0, 4, 0))
  y <- 2 + 3 * X[, 1] - X[, 2]
  for (installed in c(FALSE, TRUE)) {
    local_mocked_bindings(
    .package = "PortfolioTesteR",
      .pt_ml_backend_available = function(...) installed,
      .pt_keras_available = function(...) {
        if (!installed) stop("Keras must not be probed when absent")
        FALSE
      }
    )
    for (type in c("gru", "lstm", "cnn1d")) {
      expect_message(m <- ml_make_seq_model(type), "using the linear fallback", fixed = TRUE)
      expect_identical(m$requested_type, type)
      expect_identical(m$actual_type, "linear")
      fitted <- m$fit(X, y)
      expect_identical(fitted$type, "lin")
      expect_equal(m$predict(fitted, X), y)
    }
  }
})

test_that("available sequence factory records the requested model without fitting", {
  local_mocked_bindings(
    .package = "PortfolioTesteR",
    .pt_ml_backend_available = function(...) TRUE,
    .pt_keras_available = function(...) TRUE
  )
  for (type in c("gru", "lstm", "cnn1d")) {
    expect_no_condition(m <- ml_make_seq_model(type))
    expect_identical(m$requested_type, type)
    expect_identical(m$actual_type, type)
    expect_true(is.function(m$fit))
    expect_true(is.function(m$predict))
  }
})

test_that("ensemble handles missing predictions without optional packages", {
  first <- list(fit = function(X, y) NULL,
                predict = function(model, X) c(2, NA, 6, NA))
  second <- list(fit = function(X, y) NULL,
                 predict = function(model, X) c(4, 8, NA, NA))
  ensemble <- ml_make_ensemble(first, second, weights = c(1, 3))
  X <- matrix(0, 4, 1)
  expect_equal(ensemble$predict(ensemble$fit(X, rep(0, 4)), X),
               c(3.5, 8, 6, NA_real_))
})
