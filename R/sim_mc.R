# Intent: simulate portfolio returns using MVN model (MVP baseline)

suppressPackageStartupMessages({
    library(MASS) # for mvrnorm
})

simulate_portfolio_mvn <- function(returns_mat, weights, n_sims = 20000){
    stopifnot(is.matrix(returns_mat)) ## Ensure returns are in matrix format
    stopifnot(length(weights) == ncol(returns_mat)) ## Ensure weights match number of assets
    stopifnot(abs(sum(weights) - 1) < 1e-6) ## Ensure weights sum to 1 (within tolerance)

    mu <- colMeans(returns_mat) ## Estimate mean returns from historical data
    cov <- stats::cov(returns_mat) ## Estimate covariance matrix from historical data

    sims_asset <- MASS::mvrnorm(n = n_sims, mu = mu, Sigma = cov) ## Simulate asset returns using MVN model
    sims_port <- as.numeric(sims_asset %*% weights) ## Compute portfolio returns from asset simulations

    sims_port ## Return the simulated portfolio returns
}