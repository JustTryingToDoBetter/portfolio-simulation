# Backtesting validation utilities:
#   - Kupiec (1995) unconditional coverage
#   - Christoffersen (1998) independence and conditional coverage
#
# Conventions:
#   violations: binary vector where 1 = VaR breach, 0 = no breach
#   alpha:      VaR confidence level, e.g. 0.95 (expected rate = 1 - alpha)

# ---------------------------------------------------------------------------
# Internal helpers (not exported)
# ---------------------------------------------------------------------------

# Clip probability to (eps, 1-eps) to prevent log(0) in LR calculations.
.clip_prob <- function(z, eps = 1e-12) pmin(pmax(z, eps), 1 - eps)

# Validate a binary violation sequence and coerce to integer.
.validate_violations <- function(violations) {
  v <- as.integer(violations)
  if (length(v) < 2)
    stop("`violations` must contain at least 2 observations.", call. = FALSE)
  if (!all(v %in% c(0L, 1L)))
    stop("`violations` must be binary (0 or 1 only).", call. = FALSE)
  v
}

# ---------------------------------------------------------------------------
# compute_transition_counts
#
# Compute first-order Markov transition counts from a binary breach sequence.
# Returns a named list: n00, n01, n10, n11.
# ---------------------------------------------------------------------------
compute_transition_counts <- function(violations) {
  v      <- .validate_violations(violations)
  v_lag  <- v[-length(v)]
  v_curr <- v[-1]
  list(
    n00 = sum(v_lag == 0L & v_curr == 0L),
    n01 = sum(v_lag == 0L & v_curr == 1L),
    n10 = sum(v_lag == 1L & v_curr == 0L),
    n11 = sum(v_lag == 1L & v_curr == 1L)
  )
}

# ---------------------------------------------------------------------------
# kupiec_uc_test
#
# Kupiec (1995) unconditional coverage likelihood-ratio test.
# H0: violation rate equals the model-implied rate (1 - alpha).
# df = 1.
# ---------------------------------------------------------------------------
kupiec_uc_test <- function(violations, alpha = 0.95) {
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be a single number in (0, 1).", call. = FALSE)

  v     <- .validate_violations(violations)
  p     <- 1 - alpha
  T_obs <- length(v)
  x     <- sum(v)
  phat  <- x / T_obs

  p_c    <- .clip_prob(p)
  phat_c <- .clip_prob(phat)

  logL_null <- (T_obs - x) * log(1 - p_c)    + x * log(p_c)
  logL_alt  <- (T_obs - x) * log(1 - phat_c) + x * log(phat_c)

  LR_uc <- -2 * (logL_null - logL_alt)
  p_val <- 1 - pchisq(LR_uc, df = 1)

  list(
    test_name               = "Kupiec Unconditional Coverage",
    alpha                   = alpha,
    expected_violation_rate = p,
    observed_violations     = x,
    total_observations      = T_obs,
    observed_violation_rate = phat,
    statistic               = LR_uc,
    p_value                 = p_val
  )
}

# ---------------------------------------------------------------------------
# christoffersen_independence_test
#
# Christoffersen (1998) independence test.
# H0: consecutive violations are serially independent.
# df = 1.
# ---------------------------------------------------------------------------
christoffersen_independence_test <- function(violations, alpha = 0.95) {
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be a single number in (0, 1).", call. = FALSE)

  v      <- .validate_violations(violations)
  counts <- compute_transition_counts(v)
  n00    <- counts$n00; n01 <- counts$n01
  n10    <- counts$n10; n11 <- counts$n11

  # Conditional transition probabilities; guard division by zero.
  pi01 <- if ((n00 + n01) > 0) n01 / (n00 + n01) else 0
  pi11 <- if ((n10 + n11) > 0) n11 / (n10 + n11) else 0
  pi   <- (n01 + n11) / max(n00 + n01 + n10 + n11, 1L)

  pi01_c <- .clip_prob(pi01)
  pi11_c <- .clip_prob(pi11)
  pi_c   <- .clip_prob(pi)

  logL_null <- (n00 + n10) * log(1 - pi_c)   + (n01 + n11) * log(pi_c)
  logL_alt  <-  n00 * log(1 - pi01_c) + n01 * log(pi01_c) +
                n10 * log(1 - pi11_c) + n11 * log(pi11_c)

  LR_ind <- -2 * (logL_null - logL_alt)
  p_val  <- 1 - pchisq(LR_ind, df = 1)

  list(
    test_name         = "Christoffersen Independence",
    alpha             = alpha,
    statistic         = LR_ind,
    p_value           = p_val,
    transition_counts = counts,
    pi01              = pi01,
    pi11              = pi11,
    pi_hat            = pi
  )
}

# ---------------------------------------------------------------------------
# christoffersen_cc_test
#
# Christoffersen (1998) conditional coverage test.
# Combines unconditional coverage (Kupiec) and independence into a joint test.
# H0: violations are iid with the correct rate.
# df = 2 (LR_cc = LR_uc + LR_ind).
# ---------------------------------------------------------------------------
christoffersen_cc_test <- function(violations, alpha = 0.95) {
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be a single number in (0, 1).", call. = FALSE)

  uc  <- kupiec_uc_test(violations, alpha = alpha)
  ind <- christoffersen_independence_test(violations, alpha = alpha)

  LR_cc <- uc$statistic + ind$statistic
  p_cc  <- 1 - pchisq(LR_cc, df = 2)

  list(
    test_name               = "Christoffersen Conditional Coverage",
    alpha                   = alpha,
    statistic               = LR_cc,
    p_value                 = p_cc,
    uc_statistic            = uc$statistic,
    uc_p_value              = uc$p_value,
    ind_statistic           = ind$statistic,
    ind_p_value             = ind$p_value,
    transition_counts       = ind$transition_counts,
    observed_violations     = uc$observed_violations,
    total_observations      = uc$total_observations,
    observed_violation_rate = uc$observed_violation_rate,
    expected_violation_rate = uc$expected_violation_rate
  )
}

# ---------------------------------------------------------------------------
# christoffersen_conditional_coverage (backward-compatible combined function)
#
# Kept for callers that used the original monolithic function.
# New code should prefer christoffersen_cc_test().
# ---------------------------------------------------------------------------
christoffersen_conditional_coverage <- function(violations, alpha = 0.95) {
  # violations: binary vector where 1 = VaR breach, 0 = no breach
  # alpha: VaR confidence level, e.g. 0.95

  v <- as.integer(violations)

  if (length(v) < 2)
    stop("violations must contain at least 2 observations")
  if (!all(v %in% c(0L, 1L)))
    stop("violations must be a binary vector containing only 0 and 1")

  p <- 1 - alpha
  T <- length(v)
  x <- sum(v)

  # Guard against log(0) issues by clipping probabilities slightly.
  eps       <- 1e-12
  clip_prob <- function(z) pmin(pmax(z, eps), 1 - eps)

  phat   <- x / T
  p_c    <- clip_prob(p)
  phat_c <- clip_prob(phat)

  logL_null_uc <- (T - x) * log(1 - p_c)    + x * log(p_c)
  logL_alt_uc  <- (T - x) * log(1 - phat_c) + x * log(phat_c)

  LR_uc <- -2 * (logL_null_uc - logL_alt_uc)
  p_uc  <- 1 - pchisq(LR_uc, df = 1)

  v_lag  <- v[-length(v)]
  v_curr <- v[-1]

  n00 <- sum(v_lag == 0L & v_curr == 0L)
  n01 <- sum(v_lag == 0L & v_curr == 1L)
  n10 <- sum(v_lag == 1L & v_curr == 0L)
  n11 <- sum(v_lag == 1L & v_curr == 1L)

  pi01 <- if ((n00 + n01) > 0) n01 / (n00 + n01) else 0
  pi11 <- if ((n10 + n11) > 0) n11 / (n10 + n11) else 0
  pi   <- (n01 + n11) / (n00 + n01 + n10 + n11)

  pi01_c <- clip_prob(pi01)
  pi11_c <- clip_prob(pi11)
  pi_c2  <- clip_prob(pi)

  logL_null_ind <- (n00 + n10) * log(1 - pi_c2) + (n01 + n11) * log(pi_c2)
  logL_alt_ind  <- n00 * log(1 - pi01_c) + n01 * log(pi01_c) +
                   n10 * log(1 - pi11_c) + n11 * log(pi11_c)

  LR_ind <- -2 * (logL_null_ind - logL_alt_ind)
  p_ind  <- 1 - pchisq(LR_ind, df = 1)

  LR_cc <- LR_uc + LR_ind
  p_cc  <- 1 - pchisq(LR_cc, df = 2)

  list(
    alpha                   = alpha,
    expected_violation_rate = p,
    observed_violations     = x,
    total_observations      = T,
    observed_violation_rate = phat,
    transition_counts       = c(n00 = n00, n01 = n01, n10 = n10, n11 = n11),
    kupiec                  = list(statistic = LR_uc, p_value = p_uc),
    independence            = list(statistic = LR_ind, p_value = p_ind),
    conditional_coverage    = list(statistic = LR_cc, p_value = p_cc)
  )
}