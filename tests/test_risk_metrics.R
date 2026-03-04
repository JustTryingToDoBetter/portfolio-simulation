risk_metrics_env <- new.env(parent = baseenv())
backtest_env <- new.env(parent = baseenv())

source("R/risk_metrics.R", local = risk_metrics_env)
source("R/backtest.R", local = backtest_env)

expect_true <- function(condition, message = "Expectation failed") {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

expect_equal_num <- function(x, y, tol = 1e-8, message = "Values are not equal") {
  if (!isTRUE(all.equal(x, y, tolerance = tol))) {
    stop(sprintf("%s: x=%s y=%s", message, x, y), call. = FALSE)
  }
}

test_var_cvar_basic <- function() {
  pnl <- c(0.01, -0.02, 0.015, -0.01, 0.005, -0.03, 0.02, -0.015, 0.01, -0.005, 0.004, -0.012)
  alpha <- 0.95

  out <- risk_metrics_env$var_cvar(pnl, alpha = alpha)
  expected_var <- as.numeric(stats::quantile(-pnl, probs = alpha, type = 7, names = FALSE))

  expect_true(all(c("VaR", "CVaR", "CVar") %in% names(out)), "var_cvar output names are incomplete")
  expect_equal_num(out$VaR, expected_var, message = "VaR does not match expected quantile")
  expect_true(out$CVaR >= out$VaR, "CVaR should be >= VaR")
  expect_true(out$VaR >= 0, "VaR should be non-negative")
}

test_rolling_var_backtest_basic <- function() {
  returns <- rep(c(0.01, -0.01, 0.02, -0.02, 0.005), 80)
  alpha <- 0.95
  window <- 50

  bt <- backtest_env$rolling_var_backtest(returns, window = window, alpha = alpha)
  summary <- backtest_env$backtest_summary(bt, alpha = alpha)

  expect_true(nrow(bt) == length(returns) - window, "Backtest row count mismatch")
  expect_true(all(c("index", "realized_return", "realized_loss", "VaR", "breach") %in% names(bt)), "Backtest columns missing")
  expect_true(all(bt$VaR >= 0), "All VaR forecasts should be non-negative")
  expect_true(is.numeric(summary$observed_breach_rate), "Observed breach rate should be numeric")
  expect_true(summary$observed_breach_rate >= 0 && summary$observed_breach_rate <= 1, "Breach rate must be in [0,1]")
  expect_equal_num(summary$expected_breach_rate, 1 - alpha, message = "Expected breach rate mismatch")
}

test_var_cvar_basic()
test_rolling_var_backtest_basic()

cat("tests/test_risk_metrics.R: PASS\n")