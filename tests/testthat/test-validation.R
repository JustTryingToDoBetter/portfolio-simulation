library(testthat)

# ---------------------------------------------------------------------------
# compute_transition_counts() tests
# ---------------------------------------------------------------------------

test_that("compute_transition_counts returns a list with n00/n01/n10/n11", {
  res <- compute_transition_counts(c(0L, 1L, 0L, 1L))
  expect_type(res, "list")
  expect_named(res, c("n00", "n01", "n10", "n11"))
})

test_that("transition counts are correct for alternating 0-1 sequence", {
  # 0,1,0,1,0,1 -> transitions: 0->1, 1->0, 0->1, 1->0, 0->1
  v   <- c(0L, 1L, 0L, 1L, 0L, 1L)
  res <- compute_transition_counts(v)
  expect_equal(res$n01, 3L)
  expect_equal(res$n10, 2L)
  expect_equal(res$n00, 0L)
  expect_equal(res$n11, 0L)
})

test_that("transition counts are correct for all-zeros sequence", {
  v   <- make_zero_breach_seq(10)
  res <- compute_transition_counts(v)
  expect_equal(res$n00, 9L)
  expect_equal(res$n01, 0L)
  expect_equal(res$n10, 0L)
  expect_equal(res$n11, 0L)
})

test_that("transition counts are correct for all-ones sequence", {
  v   <- make_all_breach_seq(10)
  res <- compute_transition_counts(v)
  expect_equal(res$n11, 9L)
  expect_equal(res$n00, 0L)
  expect_equal(res$n01, 0L)
  expect_equal(res$n10, 0L)
})

test_that("compute_transition_counts errors on sequence shorter than 2", {
  expect_error(compute_transition_counts(1L))
})

test_that("compute_transition_counts errors on non-binary values", {
  expect_error(compute_transition_counts(c(0L, 2L, 1L)))
})

# ---------------------------------------------------------------------------
# kupiec_uc_test() tests
# ---------------------------------------------------------------------------

test_that("kupiec_uc_test returns expected fields", {
  res <- kupiec_uc_test(make_breach_seq(200), alpha = 0.95)
  expect_type(res, "list")
  expected_names <- c("test_name", "alpha", "expected_violation_rate",
                      "observed_violations", "total_observations",
                      "observed_violation_rate", "statistic", "p_value")
  expect_true(all(expected_names %in% names(res)))
})

test_that("kupiec_uc_test: zero breaches produces non-negative LR statistic", {
  res <- kupiec_uc_test(make_zero_breach_seq(200), alpha = 0.95)
  expect_true(res$statistic >= 0)
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

test_that("kupiec_uc_test does not reject when breach rate matches expected", {
  # Exactly 5% breaches in 200 obs = expected for alpha = 0.95.
  v <- integer(200)
  v[1:10] <- 1L
  res <- kupiec_uc_test(v, alpha = 0.95)
  expect_true(res$p_value > 0.05)
})

test_that("kupiec_uc_test rejects when breach rate is far too high", {
  # 40% breach rate is far from expected 5%.
  v        <- integer(200)
  v[1:80]  <- 1L
  res      <- kupiec_uc_test(v, alpha = 0.95)
  expect_true(res$p_value < 0.01)
})

test_that("kupiec_uc_test observed fields are consistent", {
  v   <- make_breach_seq(200, breach_rate = 0.05, seed = 99L)
  res <- kupiec_uc_test(v, alpha = 0.95)
  expect_equal(res$total_observations, 200L)
  expect_equal(res$observed_violations, sum(v))
  expect_equal(res$observed_violation_rate, mean(v))
  expect_equal(res$expected_violation_rate, 0.05)
})

test_that("kupiec_uc_test errors on invalid alpha", {
  expect_error(kupiec_uc_test(make_breach_seq(100), alpha = 0))
  expect_error(kupiec_uc_test(make_breach_seq(100), alpha = 1))
  expect_error(kupiec_uc_test(make_breach_seq(100), alpha = 1.5))
})

test_that("kupiec_uc_test errors on non-binary violations", {
  # Use 2, which stays non-binary after as.integer() coercion.
  expect_error(kupiec_uc_test(c(0L, 2L, 1L), alpha = 0.95))
})

test_that("kupiec_uc_test errors on single-element input", {
  expect_error(kupiec_uc_test(1L, alpha = 0.95))
})

# ---------------------------------------------------------------------------
# christoffersen_independence_test() tests
# ---------------------------------------------------------------------------

test_that("christoffersen_independence_test returns expected fields", {
  res <- christoffersen_independence_test(make_breach_seq(200), alpha = 0.95)
  expect_type(res, "list")
  expected_names <- c("test_name", "alpha", "statistic", "p_value",
                      "transition_counts", "pi01", "pi11", "pi_hat")
  expect_true(all(expected_names %in% names(res)))
})

test_that("independence test: transition counts in output match compute_transition_counts", {
  v    <- make_breach_seq(200, seed = 11L)
  res  <- christoffersen_independence_test(v, alpha = 0.95)
  ctc  <- compute_transition_counts(v)
  expect_equal(res$transition_counts$n00, ctc$n00)
  expect_equal(res$transition_counts$n01, ctc$n01)
  expect_equal(res$transition_counts$n10, ctc$n10)
  expect_equal(res$transition_counts$n11, ctc$n11)
})

test_that("independence test does not crash on all-zero breach sequence", {
  # All zeros: no breaches so no 0->1 or 1->0 transitions; clipping protects log(0).
  res <- christoffersen_independence_test(make_zero_breach_seq(50), alpha = 0.95)
  expect_true(is.finite(res$statistic))
  expect_true(is.finite(res$p_value))
})

test_that("independence test rejects independence for alternating sequence", {
  # Alternating 0-1 is maximally dependent; LR_ind should be large.
  v   <- make_alternating_breach_seq(100)
  res <- christoffersen_independence_test(v, alpha = 0.95)
  expect_true(res$statistic > 1)
})

test_that("independence test errors on invalid alpha", {
  expect_error(christoffersen_independence_test(make_breach_seq(50), alpha = 0))
  expect_error(christoffersen_independence_test(make_breach_seq(50), alpha = 1.2))
})

# ---------------------------------------------------------------------------
# christoffersen_cc_test() tests
# ---------------------------------------------------------------------------

test_that("christoffersen_cc_test returns expected fields", {
  res <- christoffersen_cc_test(make_breach_seq(200), alpha = 0.95)
  expect_type(res, "list")
  expected_names <- c("test_name", "alpha", "statistic", "p_value",
                      "uc_statistic", "uc_p_value",
                      "ind_statistic", "ind_p_value",
                      "transition_counts",
                      "observed_violations", "total_observations",
                      "observed_violation_rate", "expected_violation_rate")
  expect_true(all(expected_names %in% names(res)))
})

test_that("cc test statistic equals uc + ind statistics", {
  v     <- make_breach_seq(200, seed = 77L)
  res   <- christoffersen_cc_test(v, alpha = 0.95)
  expect_equal(res$statistic, res$uc_statistic + res$ind_statistic,
               tolerance = 1e-12)
})

test_that("cc test p_value is based on chi-squared(2)", {
  v   <- make_breach_seq(200, seed = 88L)
  res <- christoffersen_cc_test(v, alpha = 0.95)
  expected_p <- 1 - pchisq(res$statistic, df = 2)
  expect_equal(res$p_value, expected_p, tolerance = 1e-12)
})

test_that("cc test errors on invalid alpha", {
  expect_error(christoffersen_cc_test(make_breach_seq(50), alpha = 0))
})

test_that("christoffersen_conditional_coverage (legacy) still works", {
  # Backward-compat wrapper must keep returning the original list shape.
  v   <- make_breach_seq(200, seed = 55L)
  res <- christoffersen_conditional_coverage(v, alpha = 0.95)
  expect_true(all(c("kupiec", "independence", "conditional_coverage") %in% names(res)))
  expect_named(res$kupiec, c("statistic", "p_value"))
})
