library(testthat)

# ---------------------------------------------------------------------------
# var_cvar() tests
# Convention: pnl is profit/loss; VaR = quantile(-pnl, alpha).
# ---------------------------------------------------------------------------

test_that("var_cvar returns a list with the expected field names", {
  set.seed(1L)
  pnl <- rnorm(500, mean = 0, sd = 0.01)
  res <- var_cvar(pnl, alpha = 0.95)
  expect_type(res, "list")
  expect_true(all(c("VaR", "CVaR", "CVar") %in% names(res)))
})

test_that("VaR is non-negative for a typical mixed-return vector", {
  set.seed(2L)
  pnl <- rnorm(500, mean = 0, sd = 0.01)
  res <- var_cvar(pnl, alpha = 0.95)
  # quantile(-pnl, 0.95) for a symmetric distribution centred at 0 is positive.
  expect_true(res$VaR >= 0)
})

test_that("CVaR >= VaR", {
  set.seed(3L)
  pnl <- rnorm(500, mean = 0, sd = 0.01)
  res <- var_cvar(pnl, alpha = 0.95)
  expect_true(res$CVaR >= res$VaR)
})

test_that("CVar field matches CVaR (backward-compat alias)", {
  set.seed(4L)
  pnl <- rnorm(100, mean = 0, sd = 0.01)
  res <- var_cvar(pnl, alpha = 0.95)
  expect_equal(res$CVar, res$CVaR)
})

test_that("var_cvar produces the correct VaR for a known fixed input", {
  # 50 losses of 0.05, 50 gains of 0.01 -> losses = c(0.05*50, -0.01*50)
  # sorted ascending: -0.01*50 then 0.05*50.
  # type-7 quantile at 0.95 with n=100: h = 0.95*99+1 = 95.05 -> x[95] = 0.05.
  pnl <- c(rep(-0.05, 50), rep(0.01, 50))
  res <- var_cvar(pnl, alpha = 0.95)
  expect_equal(res$VaR, 0.05, tolerance = 1e-10)
  # CVaR = mean of losses >= 0.05 = mean(rep(0.05, 50)) = 0.05.
  expect_equal(res$CVaR, 0.05, tolerance = 1e-10)
})

test_that("var_cvar matches manual quantile calculation", {
  set.seed(5L)
  pnl   <- rnorm(200, mean = 0, sd = 0.015)
  alpha <- 0.99
  res   <- var_cvar(pnl, alpha = alpha)
  expected_var <- as.numeric(quantile(-pnl, probs = alpha, type = 7, names = FALSE))
  expect_equal(res$VaR, expected_var, tolerance = 1e-12)
})

test_that("var_cvar errors on alpha = 0", {
  expect_error(var_cvar(rnorm(100), alpha = 0))
})

test_that("var_cvar errors on alpha = 1", {
  expect_error(var_cvar(rnorm(100), alpha = 1))
})

test_that("var_cvar errors on alpha outside (0, 1)", {
  expect_error(var_cvar(rnorm(100), alpha = 1.5))
  expect_error(var_cvar(rnorm(100), alpha = -0.1))
})

test_that("var_cvar errors on input with fewer than 10 observations", {
  expect_error(var_cvar(rnorm(9), alpha = 0.95))
})

test_that("var_cvar errors on non-numeric input", {
  expect_error(var_cvar(c("a", "b", "c"), alpha = 0.95))
})
