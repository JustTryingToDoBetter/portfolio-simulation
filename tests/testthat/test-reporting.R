library(testthat)

# Tests for R/reporting.R
# All tests use make_mini_artifact() from helper-fixtures.R; no live API calls.

# ---------------------------------------------------------------------------
# summarize_run_artifact
# ---------------------------------------------------------------------------

test_that("summarize_run_artifact returns a list", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  expect_type(s, "list")
})

test_that("summarize_run_artifact contains expected top-level fields", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  required <- c(
    "run_timestamp", "seed", "tickers", "weights",
    "start_date", "end_date", "n_obs", "alpha",
    "backtest_model", "risk_summary",
    "breach_count", "total_tested", "breach_rate", "expected_rate",
    "kupiec_pval", "cc_pval",
    "timing_df", "total_runtime_s",
    "garch_enabled", "garch_succeeded", "garch_message"
  )
  expect_true(all(required %in% names(s)))
})

test_that("summarize_run_artifact captures correct tickers", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  expect_equal(s$tickers, c("AAPL", "MSFT"))
})

test_that("summarize_run_artifact captures correct alpha", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  expect_equal(s$alpha, 0.95)
})

test_that("summarize_run_artifact risk_summary contains non-null bootstrap entry", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  expect_true("bootstrap" %in% names(s$risk_summary))
  expect_true(is.finite(s$risk_summary$bootstrap$VaR))
  expect_true(is.finite(s$risk_summary$bootstrap$CVaR))
})

test_that("summarize_run_artifact garch_enabled is FALSE when disabled", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  expect_false(s$garch_enabled)
})

test_that("summarize_run_artifact kupiec_pval is finite", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  expect_true(is.finite(s$kupiec_pval))
})

test_that("summarize_run_artifact cc_pval captures christoffersen p-value", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  expect_true(is.finite(s$cc_pval))
})

test_that("summarize_run_artifact cc_pval is NA when christoffersen is NULL", {
  art                <- make_mini_artifact()
  art$christoffersen <- NULL
  s  <- summarize_run_artifact(art)
  expect_true(is.na(s$cc_pval))
})

test_that("summarize_run_artifact total_runtime_s matches sum of timings", {
  art <- make_mini_artifact()
  s   <- summarize_run_artifact(art)
  expect_equal(s$total_runtime_s, sum(art$timings$seconds))
})

test_that("summarize_run_artifact errors on non-list input", {
  expect_error(summarize_run_artifact("not a list"), regexp = "must be a list")
})

test_that("summarize_run_artifact tolerates missing sample_metadata", {
  art                 <- make_mini_artifact()
  art$sample_metadata <- NULL
  s <- summarize_run_artifact(art)
  # Should fall back to cfg values without error.
  expect_equal(s$alpha, 0.95)
})

# ---------------------------------------------------------------------------
# write_run_summary_markdown
# ---------------------------------------------------------------------------

test_that("write_run_summary_markdown creates the report file", {
  art     <- make_mini_artifact()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  path <- write_run_summary_markdown(art, output_dir = tmp_dir)
  expect_true(file.exists(path))
})

test_that("write_run_summary_markdown returns the file path invisibly", {
  art     <- make_mini_artifact()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  path <- write_run_summary_markdown(art, output_dir = tmp_dir)
  expect_true(is.character(path))
  expect_true(nchar(path) > 0)
})

test_that("write_run_summary_markdown creates output directory when absent", {
  art     <- make_mini_artifact()
  tmp_dir <- file.path(tempdir(), paste0("report_test_", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  expect_false(dir.exists(tmp_dir))
  write_run_summary_markdown(art, output_dir = tmp_dir)
  expect_true(dir.exists(tmp_dir))
})

test_that("write_run_summary_markdown output contains expected sections", {
  art     <- make_mini_artifact()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  path    <- write_run_summary_markdown(art, output_dir = tmp_dir)
  content <- paste(readLines(path), collapse = "\n")

  expect_match(content, "Portfolio Risk Simulation Report",  fixed = TRUE)
  expect_match(content, "Portfolio Configuration",           fixed = TRUE)
  expect_match(content, "Risk Metrics by Model",             fixed = TRUE)
  expect_match(content, "Backtest Validation",               fixed = TRUE)
  expect_match(content, "Kupiec",                            fixed = TRUE)
  expect_match(content, "Benchmark Summary",                 fixed = TRUE)
  expect_match(content, "Key Observations",                  fixed = TRUE)
})

test_that("write_run_summary_markdown includes ticker names", {
  art     <- make_mini_artifact()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  path    <- write_run_summary_markdown(art, output_dir = tmp_dir)
  content <- paste(readLines(path), collapse = "\n")

  expect_match(content, "AAPL", fixed = TRUE)
  expect_match(content, "MSFT", fixed = TRUE)
})

test_that("write_run_summary_markdown report omits Stress section when stress missing", {
  art                                   <- make_mini_artifact()
  art$model_metrics$stress_bootstrap    <- NULL
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Should still write without error; stress section simply absent.
  path <- write_run_summary_markdown(art, output_dir = tmp_dir)
  expect_true(file.exists(path))
})

test_that("write_run_summary_markdown errors on non-list artifact", {
  expect_error(
    write_run_summary_markdown(42, output_dir = tempdir()),
    regexp = "must be a list"
  )
})

test_that("write_run_summary_markdown errors on empty output_dir", {
  art <- make_mini_artifact()
  expect_error(
    write_run_summary_markdown(art, output_dir = ""),
    regexp = "non-empty string"
  )
})

test_that("write_run_summary_markdown respects custom filename", {
  art     <- make_mini_artifact()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  path <- write_run_summary_markdown(art, output_dir = tmp_dir,
                                     filename = "custom_report.md")
  expect_true(grepl("custom_report.md", path, fixed = TRUE))
  expect_true(file.exists(path))
})

# ---------------------------------------------------------------------------
# Christoffersen section presence
# ---------------------------------------------------------------------------

test_that("report includes Christoffersen section when cc data present", {
  art     <- make_mini_artifact()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  path    <- write_run_summary_markdown(art, output_dir = tmp_dir)
  content <- paste(readLines(path), collapse = "\n")
  expect_match(content, "Christoffersen", fixed = TRUE)
})

test_that("report omits Christoffersen section when cc is NULL", {
  art                <- make_mini_artifact()
  art$christoffersen <- NULL
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  path    <- write_run_summary_markdown(art, output_dir = tmp_dir)
  content <- paste(readLines(path), collapse = "\n")
  # Section should not appear.
  expect_false(grepl("Christoffersen Conditional Coverage", content, fixed = TRUE))
})
