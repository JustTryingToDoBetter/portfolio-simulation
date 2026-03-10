library(testthat)

# Backtest tests use small matrices to keep wall-clock time short.
# backtest_var() requires n_sims >= 1000; window >= 20; nrow > window.

make_backtest_mat <- function(n_rows = 60, n_cols = 2, seed = 42L) {
  make_returns_matrix(n_rows = n_rows, n_cols = n_cols,
                      col_names = c("A", "B")[seq_len(n_cols)], seed = seed)
}

# ---------------------------------------------------------------------------
# backtest_var() tests
# ---------------------------------------------------------------------------

test_that("backtest_var returns expected list fields", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 1L)
  res <- backtest_var(mat, weights = c(0.5, 0.5), window = 30L,
                      alpha = 0.95, model = "bootstrap", n_sims = 1000L)
  expect_type(res, "list")
  expected <- c("breach_rate", "breach_count", "total_tested", "vars",
                "port_ret", "breaches")
  expect_true(all(expected %in% names(res)))
})

test_that("backtest_var breach_rate is in [0, 1]", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 2L)
  res <- backtest_var(mat, weights = c(0.5, 0.5), window = 30L,
                      alpha = 0.95, model = "bootstrap", n_sims = 1000L)
  expect_true(res$breach_rate >= 0 && res$breach_rate <= 1)
})

test_that("backtest_var breach vector length equals total_tested", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 3L)
  res <- backtest_var(mat, weights = c(0.5, 0.5), window = 30L,
                      alpha = 0.95, model = "bootstrap", n_sims = 1000L)
  expect_equal(length(res$breaches), res$total_tested)
})

test_that("backtest_var total_tested equals nrow - window", {
  window <- 30L
  mat    <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 4L)
  res    <- backtest_var(mat, weights = c(0.5, 0.5), window = window,
                         alpha = 0.95, model = "bootstrap", n_sims = 1000L)
  expect_equal(res$total_tested, nrow(mat) - window)
})

test_that("backtest_var VaR estimates are all numeric and finite", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 5L)
  res <- backtest_var(mat, weights = c(0.5, 0.5), window = 30L,
                      alpha = 0.95, model = "bootstrap", n_sims = 1000L)
  expect_true(all(is.finite(res$vars)))
})

test_that("backtest_var works with model = 'mvn'", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 6L)
  res <- backtest_var(mat, weights = c(0.5, 0.5), window = 30L,
                      alpha = 0.95, model = "mvn", n_sims = 1000L)
  expect_true(res$breach_rate >= 0 && res$breach_rate <= 1)
})

test_that("backtest_var errors when matrix has too few rows for the window", {
  mat <- make_backtest_mat(n_rows = 30, n_cols = 2, seed = 7L)
  expect_error(
    backtest_var(mat, weights = c(0.5, 0.5), window = 30L, n_sims = 1000L)
  )
})

test_that("backtest_var errors when window < 20", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 8L)
  expect_error(
    backtest_var(mat, weights = c(0.5, 0.5), window = 10L, n_sims = 1000L)
  )
})

test_that("backtest_var errors when n_sims < 1000", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 9L)
  expect_error(
    backtest_var(mat, weights = c(0.5, 0.5), window = 30L, n_sims = 500L)
  )
})

test_that("backtest_var errors when weights do not sum to 1", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 10L)
  expect_error(
    backtest_var(mat, weights = c(0.3, 0.3), window = 30L, n_sims = 1000L)
  )
})

test_that("backtest_var errors on invalid model name", {
  mat <- make_backtest_mat(n_rows = 55, n_cols = 2, seed = 11L)
  expect_error(
    backtest_var(mat, weights = c(0.5, 0.5), window = 30L,
                 n_sims = 1000L, model = "garch")
  )
})

test_that("backtest_var errors on non-matrix input", {
  expect_error(
    backtest_var(data.frame(a = 1:60, b = 1:60), weights = c(0.5, 0.5),
                 window = 30L, n_sims = 1000L)
  )
})
