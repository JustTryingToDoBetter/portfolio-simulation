# Intent: statistical validation helpers for VaR backtests

kupiec_uc_test <- function(breaches, alpha = 0.95) {
    if (!is.logical(breaches)) {
        stop("`breaches` must be a logical vector.", call. = FALSE)
    }
    if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
        stop("`alpha` must be a scalar in (0,1).", call. = FALSE)
    }

    n <- length(breaches)
    if (n == 0) {
        stop("`breaches` must have at least one observation.", call. = FALSE)
    }

    x <- sum(breaches)
    expected <- n * (1 - alpha)
    p <- x / n
    p0 <- 1 - alpha
    eps <- 1e-12

    p <- min(max(p, eps), 1 - eps)
    p0 <- min(max(p0, eps), 1 - eps)

    ll_null <- (n - x) * log(1 - p0) + x * log(p0)
    ll_alt <- (n - x) * log(1 - p) + x * log(p)
    lr_uc <- -2 * (ll_null - ll_alt)
    p_value <- 1 - stats::pchisq(lr_uc, df = 1)

    list(
        n = n,
        breaches = x,
        expected = expected,
        breach_rate = x / n,
        lr_uc = as.numeric(lr_uc),
        p_value = as.numeric(p_value)
    )
}
