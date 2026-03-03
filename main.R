source("R/data_fetch.R")
source("R/sim_mc.R")
source("R/returns.R")
source("R/risk_metrics.R")
library(dplyr)

tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN") ## Define the tickers for which we want to fetch data and simulate
weights <- c(0.25, 0.25, 0.25, 0.25) ## Define equal weights for the portfolio

prices <- fetch_prices_yahoo(tickers, from = "2019-01-01") ## Fetch historical adjusted close prices for the specified tickers
rets <- compute_log_returns(prices) ## Compute log returns from the fetched price data
mat <- returns_wide_matrix(rets) ## Convert the returns to a wide matrix format suitable for simulation

set.seed(42) ## Set a seed for reproducibility of the simulation results
sim_port <- simulate_portfolio_mvn(mat, weights, n_sims = 50000) ## Simulate portfolio returns using the MVN model

risk <- var_cvar(sim_port, alpha = 0.95) ## Compute VaR and CVaR from the simulated portfolio returns
print(risk)