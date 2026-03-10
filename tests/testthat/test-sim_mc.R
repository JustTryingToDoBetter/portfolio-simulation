library(testthat)

# ---------------------------------------------------------------------------
# simulate_portfolio_mvn() tests
# ---------------------------------------------------------------------------

test_that("simulate_portfolio_mvn output length equals n_sims", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 1L)
  set.seed(10L)
  out <- simulate_portfolio_mvn(mat, weights = c(0.5, 0.5), n_sims = 500L)
  expect_length(out, 500L)
})

test_that("simulate_portfolio_mvn output is all finite", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 2L)
  set.seed(11L)
  out <- simulate_portfolio_mvn(mat, weights = c(0.5, 0.5), n_sims = 300L)
  expect_true(all(is.finite(out)))
})

test_that("simulate_portfolio_mvn output is numeric", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 3L)
  set.seed(12L)
  out <- simulate_portfolio_mvn(mat, weights = c(0.5, 0.5), n_sims = 200L)
  expect_type(out, "double")
})

test_that("simulate_portfolio_mvn is reproducible with a fixed seed", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 4L)
  set.seed(42L); r1 <- simulate_portfolio_mvn(mat, c(0.5, 0.5), n_sims = 200L)
  set.seed(42L); r2 <- simulate_portfolio_mvn(mat, c(0.5, 0.5), n_sims = 200L)
  expect_equal(r1, r2)
})

test_that("simulate_portfolio_mvn errors when weights length differs from ncol", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 5L)
  expect_error(simulate_portfolio_mvn(mat, weights = c(0.25, 0.25, 0.5)))
})

test_that("simulate_portfolio_mvn errors when weights do not sum to 1", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 6L)
  expect_error(simulate_portfolio_mvn(mat, weights = c(0.3, 0.3)))
})

test_that("simulate_portfolio_mvn errors when input is not a matrix", {
  expect_error(simulate_portfolio_mvn(list(a = 1:10), weights = 1))
})

test_that("simulate_portfolio_mvn errors on matrix with fewer than 2 rows", {
  mat <- make_returns_matrix(n_rows = 1, n_cols = 2,
                             col_names = c("A", "B"), seed = 7L)
  expect_error(simulate_portfolio_mvn(mat, weights = c(0.5, 0.5)))
})

# ---------------------------------------------------------------------------
# simulate_portfolio_bootstrap() tests
# ---------------------------------------------------------------------------

test_that("simulate_portfolio_bootstrap output length equals n_sims", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 8L)
  set.seed(20L)
  out <- simulate_portfolio_bootstrap(mat, weights = c(0.5, 0.5), n_sims = 400L)
  expect_length(out, 400L)
})

test_that("simulate_portfolio_bootstrap output is all finite", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 9L)
  set.seed(21L)
  out <- simulate_portfolio_bootstrap(mat, weights = c(0.5, 0.5), n_sims = 300L)
  expect_true(all(is.finite(out)))
})

test_that("simulate_portfolio_bootstrap is reproducible with a fixed seed", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 10L)
  set.seed(42L); r1 <- simulate_portfolio_bootstrap(mat, c(0.5, 0.5), n_sims = 200L)
  set.seed(42L); r2 <- simulate_portfolio_bootstrap(mat, c(0.5, 0.5), n_sims = 200L)
  expect_equal(r1, r2)
})

test_that("bootstrap stress path (vol_scale > 1) returns correct length and finite values", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 11L)
  set.seed(22L)
  out <- simulate_portfolio_bootstrap(mat, weights = c(0.5, 0.5),
                                      n_sims = 300L, vol_scale = 1.5)
  expect_length(out, 300L)
  expect_true(all(is.finite(out)))
})

test_that("bootstrap stress produces higher variance than baseline", {
  mat <- make_returns_matrix(n_rows = 100, n_cols = 2,
                             col_names = c("A", "B"), seed = 12L)
  set.seed(42L)
  base   <- simulate_portfolio_bootstrap(mat, c(0.5, 0.5), n_sims = 2000L, vol_scale = 1.0)
  set.seed(42L)
  stress <- simulate_portfolio_bootstrap(mat, c(0.5, 0.5), n_sims = 2000L, vol_scale = 2.0)
  expect_true(var(stress) > var(base))
})

test_that("simulate_portfolio_bootstrap errors when weights do not sum to 1", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 13L)
  expect_error(simulate_portfolio_bootstrap(mat, weights = c(0.6, 0.6)))
})

test_that("simulate_portfolio_bootstrap errors on non-positive vol_scale", {
  mat <- make_returns_matrix(n_rows = 60, n_cols = 2,
                             col_names = c("A", "B"), seed = 14L)
  expect_error(simulate_portfolio_bootstrap(mat, c(0.5, 0.5), vol_scale = -1))
  expect_error(simulate_portfolio_bootstrap(mat, c(0.5, 0.5), vol_scale = 0))
})

test_that("simulate_portfolio_bootstrap errors when input is not a matrix", {
  expect_error(simulate_portfolio_bootstrap(data.frame(a = 1:10), weights = 1))
})
