# Shared test fixtures for the portfolio simulation testthat suite.
# Sourced automatically by testthat::test_dir() before all test files.
#
# Notes:
#   - All generators use explicit seeds for determinism.
#   - No live API calls anywhere.
#   - testthat::test_dir() sets CWD to tests/testthat; we detect the project
#     root explicitly so R/ source paths resolve correctly.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

# ---------------------------------------------------------------------------
# Project root detection
# Walk up from CWD until we find renv.lock (reliable project marker).
# ---------------------------------------------------------------------------
.find_proj_root <- function() {
  d <- normalizePath(getwd())
  for (i in seq_len(10)) {
    if (file.exists(file.path(d, "renv.lock"))) return(d)
    parent <- dirname(d)
    if (parent == d) break   # filesystem root
    d <- parent
  }
  stop("Cannot find project root (no renv.lock in ancestor directories).")
}
.proj_root <- .find_proj_root()

# Source R/ modules using the project root as working directory so that
# internal source() calls inside R/ files (e.g. backtest.R -> sim_mc.R)
# also resolve correctly.
withr::with_dir(.proj_root, {
  r_files <- c(
    "R/logging.R",
    "R/benchmark.R",
    "R/globals.R",
    "R/returns.R",
    "R/risk_metrics.R",
    "R/sim_mc.R",
    "R/validation.R",
    "R/config.R"
  )
  for (f in r_files) source(f, local = FALSE)
  # backtest.R re-sources sim_mc.R internally (harmless); comes after sim_mc.R.
  source("R/backtest.R", local = FALSE)
})

# ---------------------------------------------------------------------------
# make_returns_matrix
#
# Generate a deterministic numeric matrix of synthetic log returns.
# col_names length must equal n_cols.
# ---------------------------------------------------------------------------
make_returns_matrix <- function(n_rows    = 300,
                                n_cols    = 4,
                                col_names = c("AAPL", "MSFT", "GOOGL", "AMZN"),
                                seed      = 42L) {
  stopifnot(length(col_names) == n_cols)
  set.seed(seed)
  mat <- matrix(
    rnorm(n_rows * n_cols, mean = 0.0003, sd = 0.015),
    nrow = n_rows, ncol = n_cols
  )
  colnames(mat) <- col_names
  rownames(mat) <- as.character(
    seq(as.Date("2020-01-02"), by = "day", length.out = n_rows)
  )
  mat
}

# ---------------------------------------------------------------------------
# make_prices_tbl
#
# Generate a deterministic long-format prices tibble for use with
# compute_log_returns(). Prices follow a random-walk path starting at 100.
# ---------------------------------------------------------------------------
make_prices_tbl <- function(tickers = c("AAPL", "MSFT", "GOOGL", "AMZN"),
                            n_days  = 300,
                            seed    = 42L) {
  set.seed(seed)
  start_date <- as.Date("2020-01-02")
  dates      <- seq(start_date, by = "day", length.out = n_days)
  do.call(rbind, lapply(tickers, function(sym) {
    prices <- cumprod(1 + rnorm(n_days, mean = 0.0003, sd = 0.015)) * 100
    tibble(date = dates, ticker = sym, price = prices)
  }))
}

# ---------------------------------------------------------------------------
# Breach-sequence fixtures
# ---------------------------------------------------------------------------

# Random binary breach sequence at a specified rate.
make_breach_seq <- function(n = 100, breach_rate = 0.05, seed = 42L) {
  set.seed(seed)
  as.integer(runif(n) < breach_rate)
}

# Alternating 0-1 sequence (maximally dependent).
make_alternating_breach_seq <- function(n = 100) {
  as.integer(rep(c(0L, 1L), length.out = n))
}

# All zeros: no breaches.
make_zero_breach_seq <- function(n = 100) integer(n)

# All ones: every observation is a breach.
make_all_breach_seq <- function(n = 100) rep(1L, n)


# ---------------------------------------------------------------------------
# make_returns_matrix
#
# Generate a deterministic numeric matrix of synthetic log returns.
# col_names length must equal n_cols.
# ---------------------------------------------------------------------------
make_returns_matrix <- function(n_rows    = 300,
                                n_cols    = 4,
                                col_names = c("AAPL", "MSFT", "GOOGL", "AMZN"),
                                seed      = 42L) {
  stopifnot(length(col_names) == n_cols)
  set.seed(seed)
  mat <- matrix(
    rnorm(n_rows * n_cols, mean = 0.0003, sd = 0.015),
    nrow = n_rows, ncol = n_cols
  )
  colnames(mat) <- col_names
  rownames(mat) <- as.character(
    seq(as.Date("2020-01-02"), by = "day", length.out = n_rows)
  )
  mat
}

# ---------------------------------------------------------------------------
# make_prices_tbl
#
# Generate a deterministic long-format prices tibble for use with
# compute_log_returns(). Prices follow a random-walk-like path starting at 100.
# ---------------------------------------------------------------------------
make_prices_tbl <- function(tickers = c("AAPL", "MSFT", "GOOGL", "AMZN"),
                            n_days  = 300,
                            seed    = 42L) {
  set.seed(seed)
  start_date <- as.Date("2020-01-02")
  dates      <- seq(start_date, by = "day", length.out = n_days)
  do.call(rbind, lapply(tickers, function(sym) {
    prices <- cumprod(1 + rnorm(n_days, mean = 0.0003, sd = 0.015)) * 100
    tibble(date = dates, ticker = sym, price = prices)
  }))
}

# ---------------------------------------------------------------------------
# Breach-sequence fixtures
# ---------------------------------------------------------------------------

# Random binary breach sequence at a specified rate.
make_breach_seq <- function(n = 100, breach_rate = 0.05, seed = 42L) {
  set.seed(seed)
  as.integer(runif(n) < breach_rate)
}

# Alternating 0-1 sequence (maximally dependent).
make_alternating_breach_seq <- function(n = 100) {
  as.integer(rep(c(0L, 1L), length.out = n))
}

# All zeros: no breaches.
make_zero_breach_seq <- function(n = 100) integer(n)

# All ones: every observation is a breach.
make_all_breach_seq <- function(n = 100) rep(1L, n)
