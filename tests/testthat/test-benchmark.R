library(testthat)

# ---------------------------------------------------------------------------
# time_it() tests
# ---------------------------------------------------------------------------

test_that("time_it returns a list with label, value, and seconds fields", {
  res <- time_it("my_step", 42)
  expect_type(res, "list")
  expect_named(res, c("label", "value", "seconds"))
})

test_that("time_it preserves the label exactly", {
  res <- time_it("step_label", NULL)
  expect_equal(res$label, "step_label")
})

test_that("time_it returns the wrapped expression value unchanged", {
  res <- time_it("test", 1 + 1)
  expect_equal(res$value, 2)
})

test_that("time_it returns the value for a complex expression", {
  mat <- matrix(1:9, 3, 3)
  res <- time_it("mat_op", mat %*% mat)
  expect_equal(res$value, mat %*% mat)
})

test_that("time_it seconds is a non-negative numeric scalar", {
  res <- time_it("noop", invisible(NULL))
  expect_type(res$seconds, "double")
  expect_length(res$seconds, 1L)
  expect_true(res$seconds >= 0)
})

test_that("time_it errors on empty label", {
  expect_error(time_it("", 1))
})

test_that("time_it errors on non-string label", {
  expect_error(time_it(123, 1))
})

# ---------------------------------------------------------------------------
# summarize_benchmarks() tests
# ---------------------------------------------------------------------------

test_that("summarize_benchmarks returns a data.frame with step and seconds columns", {
  bench <- list(
    time_it("step_a", Sys.sleep(0)),
    time_it("step_b", Sys.sleep(0))
  )
  res <- summarize_benchmarks(bench)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("step", "seconds"))
})

test_that("summarize_benchmarks row count equals number of benchmarks", {
  bench <- list(
    time_it("s1", NULL),
    time_it("s2", NULL),
    time_it("s3", NULL)
  )
  res <- summarize_benchmarks(bench)
  expect_equal(nrow(res), 3L)
})

test_that("summarize_benchmarks step names match labels in order", {
  bench <- list(time_it("alpha", NULL), time_it("beta", NULL))
  res   <- summarize_benchmarks(bench)
  expect_equal(res$step, c("alpha", "beta"))
})

test_that("summarize_benchmarks seconds column is numeric", {
  bench <- list(time_it("x", NULL))
  res   <- summarize_benchmarks(bench)
  expect_type(res$seconds, "double")
})

test_that("summarize_benchmarks returns empty data.frame for empty input", {
  res <- summarize_benchmarks(list())
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0L)
  expect_named(res, c("step", "seconds"))
})
