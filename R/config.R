# Intent: centralized runtime configuration and deterministic seeding helpers

default_config <- function() {
  list(
    tickers = c("AAPL", "MSFT", "GOOGL", "AMZN"),
    weights = c(0.25, 0.25, 0.25, 0.25),
    from = "2019-01-01",
    n_sims = 50000,
    alpha = 0.95,
    seed = 42L,
    cache_dir = "data/cache",
    cache_max_age_hours = 24,
    var_window = 252
  )
}

validate_config <- function(cfg) {
  stopifnot(is.list(cfg))
  stopifnot(length(cfg$tickers) >= 1)
  stopifnot(length(cfg$weights) == length(cfg$tickers))
  stopifnot(abs(sum(cfg$weights) - 1) < 1e-6)
  stopifnot(is.numeric(cfg$alpha), cfg$alpha > 0, cfg$alpha < 1)
  stopifnot(is.numeric(cfg$n_sims), cfg$n_sims > 0)
  stopifnot(is.numeric(cfg$seed), length(cfg$seed) == 1)
  stopifnot(is.numeric(cfg$cache_max_age_hours), cfg$cache_max_age_hours >= 0)
  stopifnot(is.numeric(cfg$var_window), cfg$var_window >= 20)
  cfg
}

load_config <- function(overrides = list()) {
  cfg <- utils::modifyList(default_config(), overrides)
  validate_config(cfg)
}

set_deterministic_seed <- function(seed) {
  stopifnot(is.numeric(seed), length(seed) == 1)
  set.seed(as.integer(seed))
  invisible(as.integer(seed))
}