source("R/data_fetch.R")
source("R/sim_mc.R")
source("R/returns.R")
source("R/risk_metrics.R")
source("R/backtest.R")
source("R/logging.R")
source("R/benchmark.R")
source("R/config.R")
library(dplyr)

cfg <- load_config()
set_deterministic_seed(cfg$seed)
log_info("seed=", cfg$seed)

bench <- list()

bench[[length(bench) + 1]] <- time_it("fetch_prices", fetch_prices_yahoo(
    cfg$tickers,
    from = cfg$from,
    cache_dir = cfg$cache_dir,
    cache_max_age_hours = cfg$cache_max_age_hours
))
t_prices <- bench[[length(bench)]]
prices <-t_prices$value
log_info('fetch prices seconds=' , round(t_prices$seconds, 2))

bench[[length(bench) + 1]] <- time_it("compute_log_returns", {
    rets <- compute_log_returns(prices)
    returns_wide_matrix(rets)
})
t_rets <- bench[[length(bench)]]
mat <- t_rets$value
log_info('compute log returns seconds=' , round(t_rets$seconds, 2), " n_rows=", nrow(mat), " n_cols=", ncol(mat))


bench[[length(bench) + 1]] <- time_it("simulate_mvn", simulate_portfolio_mvn(mat, cfg$weights, n_sims = cfg$n_sims))
t_mvn <- bench[[length(bench)]]
sim_mvn <- t_mvn$value
log_info("simulate_mvn seconds=", round(t_mvn$seconds, 3))

bench[[length(bench) + 1]] <- time_it("simulate_bootstrap", simulate_portfolio_bootstrap(mat, cfg$weights, n_sims = cfg$n_sims))
t_boot <- bench[[length(bench)]]
sim_boot <- t_boot$value
log_info("simulate_bootstrap seconds=", round(t_boot$seconds, 3))

bench[[length(bench) + 1]] <- time_it("risk_metrics", var_cvar(sim_mvn, alpha = cfg$alpha))
t_risk <- bench[[length(bench)]]
risk <- t_risk$value
log_info("risk metrics VaR=", round(risk$VaR, 5), " CVaR=", round(risk$CVaR, 5))

hist_port_rets <- as.numeric(mat %*% cfg$weights)
bench[[length(bench) + 1]] <- time_it("rolling_var_backtest", rolling_var_backtest(
    hist_port_rets,
    window = cfg$var_window,
    alpha = cfg$alpha
))
t_backtest <- bench[[length(bench)]]
bt <- t_backtest$value
bt_summary <- backtest_summary(bt, alpha = cfg$alpha)
log_info(
    "rolling_var_backtest seconds=", round(t_backtest$seconds, 3),
    " observed_breach_rate=", round(bt_summary$observed_breach_rate, 4),
    " expected_breach_rate=", round(bt_summary$expected_breach_rate, 4)
)

bench_tbl <- summarize_benchmarks(bench)
log_info("benchmark summary:")
print(bench_tbl)