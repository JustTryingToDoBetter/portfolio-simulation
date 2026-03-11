library(testthat)

# Tests for R/plots.R
# All tests use make_mini_artifact() from helper-fixtures.R to avoid any live
# API calls or heavy computation. ggplot2 is required; skip gracefully if absent.

skip_if_no_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    skip("ggplot2 not available")
}

# ---------------------------------------------------------------------------
# plot_loss_distribution
# ---------------------------------------------------------------------------

test_that("plot_loss_distribution returns a ggplot for bootstrap model", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  p   <- plot_loss_distribution(art, model = "bootstrap")
  expect_s3_class(p, "ggplot")
})

test_that("plot_loss_distribution returns a ggplot for mvn model", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  p   <- plot_loss_distribution(art, model = "mvn")
  expect_s3_class(p, "ggplot")
})

test_that("plot_loss_distribution returns a ggplot for stress_bootstrap model", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  p   <- plot_loss_distribution(art, model = "stress_bootstrap")
  expect_s3_class(p, "ggplot")
})

test_that("plot_loss_distribution errors for invalid model name", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  expect_error(
    plot_loss_distribution(art, model = "bad_model"),
    regexp = "`model` must be one of"
  )
})

test_that("plot_loss_distribution errors when simulations field is missing", {
  skip_if_no_ggplot2()
  art              <- make_mini_artifact()
  art$simulations  <- NULL
  expect_error(
    plot_loss_distribution(art, model = "bootstrap"),
    regexp = "missing required field.*simulations"
  )
})

test_that("plot_loss_distribution errors when GARCH sims are NULL", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  # GARCH is NULL in the mini artifact by design.
  expect_error(
    plot_loss_distribution(art, model = "garch"),
    regexp = "No simulation samples stored for model 'garch'"
  )
})

test_that("plot_loss_distribution saves PNG and returns ggplot", {
  skip_if_no_ggplot2()
  art  <- make_mini_artifact()
  tmp  <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  p    <- plot_loss_distribution(art, model = "bootstrap", save_to = tmp)
  expect_s3_class(p, "ggplot")
  expect_true(file.exists(tmp))
})

# ---------------------------------------------------------------------------
# plot_backtest_var
# ---------------------------------------------------------------------------

test_that("plot_backtest_var returns a ggplot", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  p   <- plot_backtest_var(art)
  expect_s3_class(p, "ggplot")
})

test_that("plot_backtest_var errors when backtest field is missing", {
  skip_if_no_ggplot2()
  art          <- make_mini_artifact()
  art$backtest <- NULL
  expect_error(
    plot_backtest_var(art),
    regexp = "missing required field.*backtest"
  )
})

test_that("plot_backtest_var errors when port_ret is missing", {
  skip_if_no_ggplot2()
  art                  <- make_mini_artifact()
  art$backtest$port_ret <- NULL
  expect_error(
    plot_backtest_var(art),
    regexp = "missing required field.*backtest\\$port_ret"
  )
})

test_that("plot_backtest_var works when backtest dates are absent", {
  skip_if_no_ggplot2()
  art               <- make_mini_artifact()
  art$backtest$dates <- NULL
  p <- plot_backtest_var(art)
  expect_s3_class(p, "ggplot")
})

test_that("plot_backtest_var saves PNG", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  plot_backtest_var(art, save_to = tmp)
  expect_true(file.exists(tmp))
})

# ---------------------------------------------------------------------------
# plot_breach_timeline
# ---------------------------------------------------------------------------

test_that("plot_breach_timeline returns a ggplot", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  p   <- plot_breach_timeline(art)
  expect_s3_class(p, "ggplot")
})

test_that("plot_breach_timeline errors when breaches field is missing", {
  skip_if_no_ggplot2()
  art                   <- make_mini_artifact()
  art$backtest$breaches <- NULL
  expect_error(
    plot_breach_timeline(art),
    regexp = "missing required field.*backtest\\$breaches"
  )
})

test_that("plot_breach_timeline works when all breaches are FALSE", {
  skip_if_no_ggplot2()
  art                   <- make_mini_artifact()
  art$backtest$breaches <- rep(FALSE, length(art$backtest$breaches))
  p <- plot_breach_timeline(art)
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# plot_model_risk_comparison
# ---------------------------------------------------------------------------

test_that("plot_model_risk_comparison returns a ggplot", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  p   <- plot_model_risk_comparison(art)
  expect_s3_class(p, "ggplot")
})

test_that("plot_model_risk_comparison handles absent GARCH gracefully", {
  skip_if_no_ggplot2()
  art                       <- make_mini_artifact()
  art$model_metrics$garch   <- NULL
  p <- plot_model_risk_comparison(art)
  expect_s3_class(p, "ggplot")
})

test_that("plot_model_risk_comparison errors when model_metrics is NULL", {
  skip_if_no_ggplot2()
  art               <- make_mini_artifact()
  art$model_metrics <- NULL
  expect_error(
    plot_model_risk_comparison(art),
    regexp = "missing required field.*model_metrics"
  )
})

test_that("plot_model_risk_comparison errors when all metrics are NULL", {
  skip_if_no_ggplot2()
  art               <- make_mini_artifact()
  art$model_metrics <- list(mvn = NULL, bootstrap = NULL,
                            stress_bootstrap = NULL, garch = NULL)
  expect_error(
    plot_model_risk_comparison(art),
    regexp = "No model metrics found"
  )
})

# ---------------------------------------------------------------------------
# plot_benchmark_timings
# ---------------------------------------------------------------------------

test_that("plot_benchmark_timings returns a ggplot", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  p   <- plot_benchmark_timings(art)
  expect_s3_class(p, "ggplot")
})

test_that("plot_benchmark_timings errors when timings is NULL", {
  skip_if_no_ggplot2()
  art         <- make_mini_artifact()
  art$timings <- NULL
  expect_error(
    plot_benchmark_timings(art),
    regexp = "missing required field.*timings"
  )
})

test_that("plot_benchmark_timings errors when timings has wrong columns", {
  skip_if_no_ggplot2()
  art         <- make_mini_artifact()
  art$timings <- data.frame(stage = "a", duration = 1.0)
  expect_error(
    plot_benchmark_timings(art),
    regexp = "columns `step` and `seconds`"
  )
})

test_that("plot_benchmark_timings errors when timings data.frame is empty", {
  skip_if_no_ggplot2()
  art         <- make_mini_artifact()
  art$timings <- data.frame(step = character(0), seconds = numeric(0))
  expect_error(
    plot_benchmark_timings(art),
    regexp = "no rows"
  )
})

test_that("plot_benchmark_timings saves PNG", {
  skip_if_no_ggplot2()
  art <- make_mini_artifact()
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  plot_benchmark_timings(art, save_to = tmp)
  expect_true(file.exists(tmp))
})
