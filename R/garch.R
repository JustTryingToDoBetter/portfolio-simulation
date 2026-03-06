# Intent: optional GARCH scaffold for per-series volatility modeling

fit_garch_series <- function(x) {
    if (!is.numeric(x) || length(x) < 50) {
        stop("`x` must be a numeric series with at least 50 observations.", call. = FALSE)
    }
    if (!requireNamespace("rugarch", quietly = TRUE)) {
        stop("Package `rugarch` is required for GARCH. Install it or set `enable_garch = FALSE`.", call. = FALSE)
    }

    spec <- rugarch::ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
        distribution.model = "norm"
    )

    rugarch::ugarchfit(spec = spec, data = x, solver = "hybrid")
}

simulate_garch_returns <- function(fit, n) {
    if (!is.numeric(n) || length(n) != 1 || n < 1) {
        stop("`n` must be a positive scalar.", call. = FALSE)
    }

    sim <- rugarch::ugarchpath(
        spec = rugarch::getspec(fit),
        n.sim = as.integer(n),
        m.sim = 1,
        presigma = tail(rugarch::sigma(fit), 1),
        prereturns = tail(rugarch::fitted(fit), 1),
        preresiduals = tail(rugarch::residuals(fit), 1)
    )

    as.numeric(fitted(sim))
}
