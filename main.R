source("R/data_fetch.R")
source("R/sim_mc.R")
source("R/returns.R")
source("R/risk_metrics.R")
source("R/backtest.R")
source("R/validation.R")
source("R/logging.R")
source("R/benchmark.R")
source("R/garch.R")
source("R/globals.R")

set.seed(42)

mode <- Sys.getenv("RISK_MODE", unset = "full")
if (!(mode %in% c("quick", "full"))) {
    log_warn("Unknown RISK_MODE=", mode, ". Falling back to full.")
    mode <- "full"
}

cfg <- list( 
    tickers = c("AAPL", "MSFT", "GOOGL", "AMZN"),
    weights = c(0.25, 0.25, 0.25, 0.25),
    from = "2019-01-01",
    alpha = 0.95,
    n_sims = if (identical(mode, "quick")) 10000L else 50000L,
    backtest_window = 252L,
    backtest_sims = if (identical(mode, "quick")) 20000L else 100000L,
    cache_dir = "data/cache",
    model = "bootstrap",
    vol_scale_baseline = 1.0,
    vol_scale_stress = 1.25,
    enable_garch = FALSE
)

if (!is.character(cfg$tickers) || length(cfg$tickers) < 1) {
    stop("`cfg$tickers` must be a non-empty character vector.", call. = FALSE)
}
if (!is.numeric(cfg$weights) || length(cfg$weights) != length(cfg$tickers)) {
    stop("`cfg$weights` must be numeric and align with tickers.", call. = FALSE)
}
if (abs(sum(cfg$weights) - 1) > 1e-6) {
    stop("`cfg$weights` must sum to 1.", call. = FALSE)
}
if (!is.numeric(cfg$alpha) || cfg$alpha <= 0 || cfg$alpha >= 1) {
    stop("`cfg$alpha` must be in (0,1).", call. = FALSE)
}

log_info("run_mode=", mode)
log_info("seed=42")

bench <- list()

bench[[length(bench) + 1]] <- time_it("fetch_prices", {
    fetch_prices_yahoo_cached(cfg$tickers, from = cfg$from, cache_dir = cfg$cache_dir)
})
prices <- bench[[length(bench)]]$value

bench[[length(bench) + 1]] <- time_it("compute_returns", {
    rets <- compute_log_returns(prices)
    returns_wide_matrix(rets)
})
mat <- bench[[length(bench)]]$value

if (ncol(mat) != length(cfg$weights)) {
    stop("Returns matrix columns and weights length must match.", call. = FALSE)
}

bench[[length(bench) + 1]] <- time_it("simulate_mvn", {
    simulate_portfolio_mvn(mat, cfg$weights, n_sims = cfg$n_sims)
})
sim_mvn <- bench[[length(bench)]]$value

bench[[length(bench) + 1]] <- time_it("simulate_bootstrap", {
    simulate_portfolio_bootstrap(mat, cfg$weights, n_sims = cfg$n_sims, vol_scale = cfg$vol_scale_baseline)
})
sim_boot <- bench[[length(bench)]]$value

bench[[length(bench) + 1]] <- time_it("risk_metrics", {
    list(
        mvn = var_cvar(sim_mvn, alpha = cfg$alpha),
        bootstrap = var_cvar(sim_boot, alpha = cfg$alpha)
    )
})
risk_pair <- bench[[length(bench)]]$value
risk_mvn <- risk_pair$mvn
risk_boot <- risk_pair$bootstrap

sim_boot_stress <- simulate_portfolio_bootstrap(
    mat,
    cfg$weights,
    n_sims = cfg$n_sims,
    vol_scale = cfg$vol_scale_stress
)
risk_boot_stress <- var_cvar(sim_boot_stress, alpha = cfg$alpha)

bench[[length(bench) + 1]] <- time_it("rolling_var_backtest", {
    backtest_var(
        returns_mat = mat,
        weights = cfg$weights,
        window = cfg$backtest_window,
        alpha = cfg$alpha,
        model = cfg$model,
        n_sims = cfg$backtest_sims
    )
})
bt <- bench[[length(bench)]]$value
kupiec <- kupiec_uc_test(bt$breaches, alpha = cfg$alpha)

garch_result <- NULL
if (isTRUE(cfg$enable_garch)) {
    if (!requireNamespace("rugarch", quietly = TRUE)) {
        log_warn("enable_garch=TRUE but package `rugarch` is not installed. Skipping GARCH.")
    } else {
        fits <- lapply(seq_len(ncol(mat)), function(j) fit_garch_series(mat[, j]))
        sims <- vapply(fits, function(fit) simulate_garch_returns(fit, n = cfg$n_sims), numeric(cfg$n_sims))
        if (!is.matrix(sims)) {
            sims <- matrix(sims, ncol = length(cfg$weights))
        }
        garch_port <- as.numeric(sims %*% cfg$weights)
        garch_result <- var_cvar(garch_port, alpha = cfg$alpha)
        log_info("garch VaR=", round(garch_result$VaR, 6), " CVaR=", round(garch_result$CVaR, 6))
    }
}

timings <- summarize_benchmarks(bench)
print(timings)

log_info("MVN risk VaR=", round(risk_mvn$VaR, 6), " CVaR=", round(risk_mvn$CVaR, 6))
log_info("Bootstrap risk VaR=", round(risk_boot$VaR, 6), " CVaR=", round(risk_boot$CVaR, 6))
log_info("Bootstrap stress (vol_scale=", cfg$vol_scale_stress, ") VaR=", round(risk_boot_stress$VaR, 6), " CVaR=", round(risk_boot_stress$CVaR, 6))
log_info("Observed breach rate=", round(bt$breach_rate, 6), " expected=", round(1 - cfg$alpha, 6))
log_info("Kupiec p_value=", round(kupiec$p_value, 6))

if (!dir.exists("outputs")) {
    dir.create("outputs", recursive = TRUE)
}

run_artifact <- list(
    cfg = cfg,
    risk_mvn = risk_mvn,
    risk_boot = risk_boot,
    risk_boot_stress = risk_boot_stress,
    backtest = bt,
    kupiec = kupiec,
    timings = timings,
    garch = garch_result
)

saveRDS(run_artifact, file = "outputs/latest_run.rds")
log_info("artifact=outputs/latest_run.rds")