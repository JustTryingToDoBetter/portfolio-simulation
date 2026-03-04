source("R/data_fetch.R")
source("R/sim_mc.R")
source("R/returns.R")
source("R/risk_metrics.R")
source("R/logging.R")
source("R/benchmark.R")
library(dplyr)
set.seed(42) ## Set a seed for reproducibility of the simulation results

cfg <- list (
    tickers = c("AAPL", "MSFT", "GOOGL", "AMZN"),
    weights = c(0.25, 0.25, 0.25, 0.25), 
    from = "2019-01-01",
    n_sims = 50000,
    alpha = 0.95

)


t_prices <- time_it("fetch_prices", fetch_prices_yahoo(cfg$tickers, from = cfg$from))
prices <-t_prices$value
log_info('fetch prices seconds=' , round(t_prices$seconds, 2))

t_rets <- time_it("compute_log_returns", {
    rets <- compute_log_returns(prices)
    returns_wide_matrix(rets)
})
mat <- t_rets$value
log_info('compute log returns seconds=' , round(t_rets$seconds, 2), " n_rows=", nrow(mat), " n_cols=", ncol(mat))


t_mvn <- time_it("simulate_mvn", simulate_portfolio_mvn(mat, cfg$weights, n_sims = cfg$n_sims))
sim_mvn <- t_mvn$value
log_info("simulate_mvn seconds=", round(t_mvn$seconds, 3))

t_boot <- time_it("simulate_bootstrap", simulate_portfolio_bootstrap(mat, cfg$weights, n_sims = cfg$n_sims))
sim_boot <- t_boot$value
log_info("simulate_bootstrap seconds=", round(t_boot$seconds, 3))