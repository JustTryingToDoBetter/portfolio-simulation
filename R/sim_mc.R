# Intent: simulate portfolio returns using MVN model (MVP baseline)

suppressPackageStartupMessages({
    library(MASS) # for mvrnorm
})

simulate_portfolio_mvn <- function(returns_mat, weights, n_sims = 20000){
    stopifnot(is.matrix(returns_mat)) ## Ensure returns are in matrix format
    stopifnot(length(weights) == ncol(returns_mat)) 
    stopifnot(abs(sum(weights) - 1) < 1e-6) 

    mu <- colMeans(returns_mat) ## Estimate mean returns from historical data
    cov <- stats::cov(returns_mat) ## Estimate covariance matrix from historical data

    sims_asset <- MASS::mvrnorm(n = n_sims, mu = mu, Sigma = cov) ## Simulate asset returns using MVN model
    sims_port <- as.numeric(sims_asset %*% weights) ## Compute portfolio returns from asset simulations

    sims_port ## Return the simulated portfolio returns
}

simulate_portfolio_bootstrap <- function(returns_mat, weights, n_sims = 20000) {
  stopifnot(is.matrix(returns_mat))
  stopifnot(length(weights) == ncol(returns_mat))

  n_obs <- nrow(returns_mat)

  # Resample historical days with replacement
  idx <- sample(seq_len(n_obs), size = n_sims, replace = TRUE)

  sims_asset <- returns_mat[idx, , drop = FALSE]
  sims_port  <- as.numeric(sims_asset %*% weights)

  sims_port
}