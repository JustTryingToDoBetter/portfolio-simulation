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
  source("R/backtest.R",   local = FALSE)
  source("R/plots.R",      local = FALSE)
  source("R/reporting.R",  local = FALSE)
  source("R/dashboard_helpers.R", local = FALSE)
  source("R/dashboard_data.R",    local = FALSE)
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


# ---------------------------------------------------------------------------
# make_mini_artifact
#
# Construct a lightweight but structurally complete run artifact for use in
# plotting and reporting tests.  No live API calls; all values are synthetic.
# The shape mirrors the real artifact produced by run_pipeline() so tests
# remain valid if the pipeline schema evolves.
# ---------------------------------------------------------------------------
make_mini_artifact <- function(seed = 42L, n_sims = 200L, n_bt = 50L) {
  set.seed(seed)

  # Small simulation vectors (returns, not losses).
  sim_mvn    <- rnorm(n_sims, mean = 0.0002, sd = 0.010)
  sim_boot   <- rnorm(n_sims, mean = 0.0002, sd = 0.012)
  sim_stress <- rnorm(n_sims, mean = 0.0002, sd = 0.015)

  # Inline VaR/CVaR so the fixture does not depend on load order.
  mini_risk <- function(sims, alpha = 0.95) {
    losses <- -sims
    v      <- as.numeric(stats::quantile(losses, probs = alpha, type = 7))
    cvar   <- mean(losses[losses >= v])
    list(VaR = v, CVaR = cvar, CVar = cvar)
  }

  risk_mvn    <- mini_risk(sim_mvn)
  risk_boot   <- mini_risk(sim_boot)
  risk_stress <- mini_risk(sim_stress)

  # Synthetic rolling backtest series.
  set.seed(seed + 1L)
  bt_dates <- seq(as.Date("2022-01-03"), by = "day", length.out = n_bt)
  port_ret <- rnorm(n_bt, mean = 0.0003, sd = 0.012)
  bt_vars  <- abs(rnorm(n_bt, mean = 0.016, sd = 0.003))
  breaches <- (-port_ret) > bt_vars

  timings <- data.frame(
    step    = c("fetch_prices", "compute_returns", "simulate_mvn",
                "simulate_bootstrap", "risk_metrics",
                "sim_boot_stress", "risk_boot_stress", "rolling_var_backtest"),
    seconds = c(0.48, 0.09, 1.15, 1.09, 0.04, 1.11, 0.04, 7.80),
    stringsAsFactors = FALSE
  )

  list(
    cfg = list(
      tickers            = c("AAPL", "MSFT"),
      weights            = c(0.5, 0.5),
      from               = "2020-01-01",
      alpha              = 0.95,
      n_sims             = as.integer(n_sims),
      backtest_window    = 30L,
      backtest_sims      = 1000L,
      model              = "bootstrap",
      vol_scale_baseline = 1.0,
      vol_scale_stress   = 1.25,
      enable_garch       = FALSE
    ),
    seed          = as.integer(seed),
    run_timestamp = "2026-03-10T12:00:00+0000",
    git_commit    = "abc1234",
    matrix_cols   = c("AAPL", "MSFT"),
    n_obs         = 500L,
    date_range    = c("2020-01-02", "2021-12-31"),
    sample_metadata = list(
      tickers        = c("AAPL", "MSFT"),
      weights        = c(0.5, 0.5),
      start_date     = "2020-01-02",
      end_date       = "2021-12-31",
      n_obs          = 500L,
      alpha          = 0.95,
      backtest_model = "bootstrap",
      run_timestamp  = "2026-03-10T12:00:00+0000",
      seed           = as.integer(seed)
    ),
    model_metrics = list(
      mvn              = risk_mvn,
      bootstrap        = risk_boot,
      stress_bootstrap = risk_stress,
      garch            = NULL
    ),
    simulations = list(
      mvn              = sim_mvn,
      bootstrap        = sim_boot,
      stress_bootstrap = sim_stress,
      garch            = NULL
    ),
    risk_mvn         = risk_mvn,
    risk_boot        = risk_boot,
    risk_boot_stress = risk_stress,
    backtest = list(
      breach_rate          = mean(breaches),
      breach_count         = sum(breaches),
      total_tested         = as.integer(n_bt),
      vars                 = bt_vars,
      var_series           = bt_vars,
      port_ret             = port_ret,
      realized_return      = port_ret,
      realized_loss        = -port_ret,
      breaches             = breaches,
      breach_indicator     = as.integer(breaches),
      evaluation_index     = seq_len(n_bt),
      dates                = bt_dates,
      model                = "bootstrap",
      alpha                = 0.95,
      window               = 30L,
      n_sims               = 1000L,
      expected_breach_rate = 0.05
    ),
    kupiec = list(
      test_name               = "Kupiec Unconditional Coverage",
      alpha                   = 0.95,
      expected_violation_rate = 0.05,
      observed_violations     = sum(breaches),
      total_observations      = as.integer(n_bt),
      observed_violation_rate = mean(breaches),
      statistic               = 1.23,
      p_value                 = 0.267
    ),
    christoffersen = list(
      test_name               = "Christoffersen Conditional Coverage",
      alpha                   = 0.95,
      statistic               = 1.73,
      p_value                 = 0.421,
      uc_statistic            = 1.23,
      uc_p_value              = 0.267,
      ind_statistic           = 0.50,
      ind_p_value             = 0.480,
      transition_counts       = list(n00 = 43L, n01 = 3L, n10 = 3L, n11 = 0L),
      observed_violations     = sum(breaches),
      total_observations      = as.integer(n_bt),
      observed_violation_rate = mean(breaches),
      expected_violation_rate = 0.05
    ),
    timings      = timings,
    garch        = NULL,
    garch_status = list(enabled = FALSE, succeeded = FALSE, message = "disabled")
  )
}
