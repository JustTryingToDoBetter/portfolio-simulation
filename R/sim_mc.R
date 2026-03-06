# Intent: simulate portfolio returns using MVN model (MVP baseline)

suppressPackageStartupMessages({
    library(MASS) # for mvrnorm
})

simulate_portfolio_mvn <- function(returns_mat, weights, n_sims = 20000){
  if (!is.matrix(returns_mat) || nrow(returns_mat) < 2) {
    stop("`returns_mat` must be a numeric matrix with at least 2 rows.", call. = FALSE)
  }
  if (!is.numeric(weights) || length(weights) != ncol(returns_mat)) {
    stop("`weights` must be numeric and match number of columns in `returns_mat`.", call. = FALSE)
  }
  if (abs(sum(weights) - 1) > 1e-6) {
    stop("`weights` must sum to 1.", call. = FALSE)
  }
  if (!is.numeric(n_sims) || length(n_sims) != 1 || n_sims < 1) {
    stop("`n_sims` must be a positive scalar.", call. = FALSE)
  }

    mu <- colMeans(returns_mat) ## Estimate mean returns from historical data
    cov <- stats::cov(returns_mat) ## Estimate covariance matrix from historical data

  sims_asset <- MASS::mvrnorm(n = as.integer(n_sims), mu = mu, Sigma = cov) ## Simulate asset returns using MVN model
    sims_port <- as.numeric(sims_asset %*% weights) ## Compute portfolio returns from asset simulations

    sims_port ## Return the simulated portfolio returns
}

simulate_portfolio_bootstrap <- function(returns_mat, weights, n_sims = 20000, vol_scale = 1.0) {
  if (!is.matrix(returns_mat) || nrow(returns_mat) < 2) {
    stop("`returns_mat` must be a numeric matrix with at least 2 rows.", call. = FALSE)
  }
  if (!is.numeric(weights) || length(weights) != ncol(returns_mat)) {
    stop("`weights` must be numeric and match number of columns in `returns_mat`.", call. = FALSE)
  }
  if (abs(sum(weights) - 1) > 1e-6) {
    stop("`weights` must sum to 1.", call. = FALSE)
  }
  if (!is.numeric(n_sims) || length(n_sims) != 1 || n_sims < 1) {
    stop("`n_sims` must be a positive scalar.", call. = FALSE)
  }
  if (!is.numeric(vol_scale) || length(vol_scale) != 1 || vol_scale <= 0) {
    stop("`vol_scale` must be a positive scalar.", call. = FALSE)
  }

  n_obs <- nrow(returns_mat)
  idx <- sample.int(n_obs, size = as.integer(n_sims), replace = TRUE)
  sims_asset <- returns_mat[idx, , drop = FALSE]

  asset_mu <- matrix(colMeans(returns_mat), nrow = nrow(sims_asset), ncol = ncol(sims_asset), byrow = TRUE)
  sims_asset_stressed <- asset_mu + (sims_asset - asset_mu) * vol_scale

  as.numeric(sims_asset_stressed %*% weights)
}