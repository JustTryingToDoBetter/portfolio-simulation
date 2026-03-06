global# plumber API for risk metrics
# Run with: plumber::pr('api/plumber.R')$run(port = 8000)

source("R/data_fetch.R")
source("R/returns.R")
source("R/sim_mc.R")
source("R/risk_metrics.R")

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

run_risk <- function(tickers, weights, from, alpha, n_sims, model, vol_scale) {
    if (length(tickers) != length(weights)) {
        stop("`tickers` and `weights` must have same length.", call. = FALSE)
    }
    if (abs(sum(weights) - 1) > 1e-6) {
        stop("`weights` must sum to 1.", call. = FALSE)
    }

    prices <- fetch_prices_yahoo_cached(tickers = tickers, from = from, cache_dir = "data/cache")
    returns_mat <- returns_wide_matrix(compute_log_returns(prices))

    sim <- if (identical(model, "mvn")) {
        simulate_portfolio_mvn(returns_mat, weights = weights, n_sims = n_sims)
    } else {
        simulate_portfolio_bootstrap(returns_mat, weights = weights, n_sims = n_sims, vol_scale = vol_scale)
    }

    var_cvar(sim, alpha = alpha)
}

#* Health check
#* @get /health
function() {
    list(status = "ok")
}

#* Compute portfolio risk
#* @post /risk
#* @serializer json
function(req, res) {
    body <- jsonlite::fromJSON(req$postBody)

    tickers <- as.character(body$tickers %||% c("AAPL", "MSFT", "GOOGL", "AMZN"))
    weights <- as.numeric(body$weights %||% rep(1 / length(tickers), length(tickers)))
    from <- as.character(body$from %||% "2019-01-01")
    alpha <- as.numeric(body$alpha %||% 0.95)
    n_sims <- as.integer(body$n_sims %||% 50000)
    model <- as.character(body$model %||% "bootstrap")
    vol_scale <- as.numeric(body$vol_scale %||% 1.0)

    if (!(model %in% c("bootstrap", "mvn"))) {
        res$status <- 400
        return(list(error = "model must be 'bootstrap' or 'mvn'"))
    }

    tryCatch(
        {
            out <- run_risk(
                tickers = tickers,
                weights = weights,
                from = from,
                alpha = alpha,
                n_sims = n_sims,
                model = model,
                vol_scale = vol_scale
            )
            list(
                tickers = tickers,
                alpha = alpha,
                n_sims = n_sims,
                model = model,
                vol_scale = vol_scale,
                VaR = out$VaR,
                CVaR = out$CVaR
            )
        },
        error = function(e) {
            res$status <- 400
            list(error = e$message)
        }
    )
}
