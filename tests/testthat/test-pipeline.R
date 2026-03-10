library(testthat)

# Pipeline integration tests.
# Uses synthetic data via an injected fetch_fn to avoid any live API calls.

# Source main.R to load run_pipeline(). The entrypoint at the bottom is guarded
# by sys.nframe() == 0L and will NOT execute when sourced from testthat.
# Use the project root as CWD so that main.R's own source() calls resolve.
withr::with_dir(.proj_root, {
  suppressMessages(source("main.R"))
})

# A synthetic fetch function that ignores tickers/from and returns a
# deterministic prices tibble sized for fast tests.
make_synthetic_fetch <- function(tickers, n_days = 80, seed = 42L) {
  function(tickers_arg, from, cache_dir) {
    make_prices_tbl(tickers = tickers, n_days = n_days, seed = seed)
  }
}

# Minimal fast config for integration tests.
fast_cfg <- function(tickers   = c("AAPL", "MSFT"),
                     weights   = c(AAPL = 0.5, MSFT = 0.5),
                     window    = 30L,
                     n_sims    = 500L,
                     bt_sims   = 1000L) {
  list(
    tickers            = tickers,
    weights            = weights,
    from               = "2020-01-01",
    alpha              = 0.95,
    n_sims             = n_sims,
    backtest_window    = window,
    backtest_sims      = bt_sims,
    cache_dir          = tempdir(),
    model              = "bootstrap",
    vol_scale_baseline = 1.0,
    vol_scale_stress   = 1.25,
    enable_garch       = FALSE
  )
}

# ---------------------------------------------------------------------------
# Happy path: successful end-to-end run with synthetic data
# ---------------------------------------------------------------------------

test_that("pipeline happy path completes and returns a structured artifact", {
  cfg    <- fast_cfg()
  fetch  <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 1L)
  result <- run_pipeline(cfg, seed = 42L, fetch_fn = fetch)

  expect_type(result, "list")
  required <- c("risk_mvn", "risk_boot", "risk_boot_stress", "backtest",
                "kupiec", "timings", "seed", "run_timestamp",
                "matrix_cols", "n_obs")
  expect_true(all(required %in% names(result)))
})

test_that("pipeline artifact seed matches the supplied seed", {
  cfg    <- fast_cfg()
  fetch  <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 2L)
  result <- run_pipeline(cfg, seed = 7L, fetch_fn = fetch)
  expect_equal(result$seed, 7L)
})

test_that("pipeline risk estimates are finite numeric values", {
  cfg    <- fast_cfg()
  fetch  <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 3L)
  result <- run_pipeline(cfg, seed = 42L, fetch_fn = fetch)

  expect_true(is.finite(result$risk_mvn$VaR))
  expect_true(is.finite(result$risk_mvn$CVaR))
  expect_true(is.finite(result$risk_boot$VaR))
  expect_true(is.finite(result$risk_boot$CVaR))
  expect_true(is.finite(result$risk_boot_stress$VaR))
})

test_that("pipeline backtest breach_rate is in [0, 1]", {
  cfg    <- fast_cfg()
  fetch  <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 4L)
  result <- run_pipeline(cfg, seed = 42L, fetch_fn = fetch)
  expect_true(result$backtest$breach_rate >= 0)
  expect_true(result$backtest$breach_rate <= 1)
})

test_that("pipeline matrix_cols matches cfg$tickers order", {
  cfg    <- fast_cfg()
  fetch  <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 5L)
  result <- run_pipeline(cfg, seed = 42L, fetch_fn = fetch)
  expect_equal(result$matrix_cols, cfg$tickers)
})

test_that("pipeline timings is a data.frame with step and seconds", {
  cfg    <- fast_cfg()
  fetch  <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 6L)
  result <- run_pipeline(cfg, seed = 42L, fetch_fn = fetch)
  expect_s3_class(result$timings, "data.frame")
  expect_named(result$timings, c("step", "seconds"))
})

test_that("pipeline artifact is saved to outputs/latest_run.rds", {
  cfg    <- fast_cfg()
  fetch  <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 7L)
  run_pipeline(cfg, seed = 42L, fetch_fn = fetch)
  expect_true(file.exists("outputs/latest_run.rds"))
})

test_that("pipeline artifact on disk contains expected fields", {
  cfg    <- fast_cfg()
  fetch  <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 8L)
  run_pipeline(cfg, seed = 42L, fetch_fn = fetch)
  art <- readRDS("outputs/latest_run.rds")
  expect_true(all(c("risk_mvn", "risk_boot", "kupiec", "timings", "seed") %in% names(art)))
})

# ---------------------------------------------------------------------------
# Config validation failures
# ---------------------------------------------------------------------------

test_that("pipeline errors on weights that do not sum to 1", {
  cfg         <- fast_cfg()
  cfg$weights <- c(AAPL = 0.4, MSFT = 0.4)   # sum = 0.8
  fetch       <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 9L)
  expect_error(run_pipeline(cfg, seed = 42L, fetch_fn = fetch))
})

test_that("pipeline errors on empty tickers", {
  cfg         <- fast_cfg()
  cfg$tickers <- character(0)
  cfg$weights <- numeric(0)
  fetch       <- make_synthetic_fetch(c("AAPL"), n_days = 80, seed = 10L)
  expect_error(run_pipeline(cfg, seed = 42L, fetch_fn = fetch))
})

test_that("pipeline errors on invalid alpha", {
  cfg       <- fast_cfg()
  cfg$alpha <- 1.5
  fetch     <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 11L)
  expect_error(run_pipeline(cfg, seed = 42L, fetch_fn = fetch))
})

test_that("pipeline errors on invalid model name", {
  cfg        <- fast_cfg()
  cfg$model  <- "garch"
  fetch      <- make_synthetic_fetch(cfg$tickers, n_days = 80, seed = 12L)
  expect_error(run_pipeline(cfg, seed = 42L, fetch_fn = fetch))
})

# ---------------------------------------------------------------------------
# Returns matrix validation failures
# ---------------------------------------------------------------------------

test_that("pipeline errors when a required ticker column is missing from matrix", {
  cfg     <- fast_cfg(tickers = c("AAPL", "MSFT"))
  # fetch_fn returns only one ticker so matrix will be missing "MSFT".
  fetch   <- make_synthetic_fetch(c("AAPL"), n_days = 80, seed = 13L)
  expect_error(run_pipeline(cfg, seed = 42L, fetch_fn = fetch))
})

test_that("pipeline errors when returns matrix has insufficient rows for backtest", {
  # window = 30, n_days = 25 -> returns matrix has 24 rows < window 30.
  cfg   <- fast_cfg(window = 30L)
  fetch <- make_synthetic_fetch(cfg$tickers, n_days = 25, seed = 14L)
  expect_error(run_pipeline(cfg, seed = 42L, fetch_fn = fetch))
})

test_that("pipeline errors when returns matrix contains Inf values", {
  cfg <- fast_cfg()
  # Inject a fetch function that produces a prices tibble with a zero price
  # (log return = log(0/prev) = -Inf).
  bad_fetch <- function(tickers_arg, from, cache_dir) {
    tbl          <- make_prices_tbl(tickers = cfg$tickers, n_days = 80, seed = 15L)
    tbl$price[5] <- 0    # forces a -Inf log return
    tbl
  }
  expect_error(run_pipeline(cfg, seed = 42L, fetch_fn = bad_fetch))
})
