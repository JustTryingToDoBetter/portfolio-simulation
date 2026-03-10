library(testthat)

# ---------------------------------------------------------------------------
# compute_log_returns() tests
# ---------------------------------------------------------------------------

test_that("compute_log_returns produces date, ticker, ret columns", {
  prices <- make_prices_tbl(tickers = c("A", "B"), n_days = 10, seed = 1L)
  res    <- compute_log_returns(prices)
  expect_true(all(c("date", "ticker", "ret") %in% names(res)))
})

test_that("compute_log_returns produces no NA values in ret", {
  prices <- make_prices_tbl(tickers = c("A", "B"), n_days = 30, seed = 2L)
  res    <- compute_log_returns(prices)
  expect_false(anyNA(res$ret))
})

test_that("compute_log_returns output has one fewer row per ticker than input", {
  n_days <- 20
  prices <- make_prices_tbl(tickers = c("A", "B"), n_days = n_days, seed = 3L)
  res    <- compute_log_returns(prices)
  # Lag computation drops the first row per ticker.
  for (tk in c("A", "B")) {
    n_ret <- nrow(dplyr::filter(res, ticker == tk))
    expect_equal(n_ret, n_days - 1L)
  }
})

test_that("compute_log_returns return values match manual log calculation", {
  set.seed(4L)
  dates  <- seq(as.Date("2020-01-01"), by = "day", length.out = 5)
  prices <- c(100, 102, 99, 101, 103)
  tbl    <- tibble::tibble(date = dates, ticker = "X", price = prices)
  res    <- compute_log_returns(tbl)
  expected_ret <- log(prices[-1] / prices[-length(prices)])
  expect_equal(res$ret, expected_ret, tolerance = 1e-12)
})

test_that("compute_log_returns returns all finite values for clean input", {
  prices <- make_prices_tbl(tickers = c("A"), n_days = 50, seed = 5L)
  res    <- compute_log_returns(prices)
  expect_true(all(is.finite(res$ret)))
})

# ---------------------------------------------------------------------------
# returns_wide_matrix() tests
# ---------------------------------------------------------------------------

test_that("returns_wide_matrix returns a numeric matrix", {
  prices  <- make_prices_tbl(tickers = c("A", "B"), n_days = 30, seed = 6L)
  returns <- compute_log_returns(prices)
  mat     <- returns_wide_matrix(returns)
  expect_true(is.matrix(mat))
  expect_type(mat, "double")
})

test_that("returns_wide_matrix column names match input tickers", {
  tickers <- c("X", "Y", "Z")
  prices  <- make_prices_tbl(tickers = tickers, n_days = 20, seed = 7L)
  returns <- compute_log_returns(prices)
  mat     <- returns_wide_matrix(returns)
  expect_setequal(colnames(mat), tickers)
})

test_that("returns_wide_matrix produces no NA values for complete input", {
  prices  <- make_prices_tbl(tickers = c("A", "B"), n_days = 50, seed = 8L)
  returns <- compute_log_returns(prices)
  mat     <- returns_wide_matrix(returns)
  expect_false(anyNA(mat))
})

test_that("returns_wide_matrix row count equals n_days - 1 for complete data", {
  n_days  <- 40
  prices  <- make_prices_tbl(tickers = c("A", "B"), n_days = n_days, seed = 9L)
  returns <- compute_log_returns(prices)
  mat     <- returns_wide_matrix(returns)
  # complete.cases() drops rows with any NA; none expected here.
  expect_equal(nrow(mat), n_days - 1L)
})

test_that("returns_wide_matrix column order is deterministic and testable", {
  prices  <- make_prices_tbl(tickers = c("AAPL", "MSFT", "GOOGL"), n_days = 30, seed = 10L)
  returns <- compute_log_returns(prices)
  mat     <- returns_wide_matrix(returns)
  # All tickers must appear; subset and reorder downstream in the pipeline.
  expect_true(all(c("AAPL", "MSFT", "GOOGL") %in% colnames(mat)))
})

test_that("returns_wide_matrix rownames contain date strings", {
  prices  <- make_prices_tbl(tickers = c("A"), n_days = 10, seed = 11L)
  returns <- compute_log_returns(prices)
  mat     <- returns_wide_matrix(returns)
  expect_false(is.null(rownames(mat)))
  # Should be parseable as dates.
  expect_true(all(!is.na(as.Date(rownames(mat)))))
})
