# Portfolio Simulation

A comprehensive R-based tool for portfolio risk simulation and analysis. This project provides functionality to fetch historical stock data, compute returns, simulate portfolio performance using Monte Carlo methods, and calculate key risk metrics like Value at Risk (VaR) and Conditional VaR (CVaR).

## Features

- **Data Fetching**: Retrieve historical adjusted close prices from Yahoo Finance for multiple tickers
- **Return Calculation**: Compute logarithmic returns from price data
- **Portfolio Simulation**: Generate Monte Carlo simulations of portfolio returns using multivariate normal distribution
- **Risk Metrics**: Calculate VaR and CVaR for portfolio risk assessment
- **Rolling VaR Backtest**: Evaluate VaR forecasts with rolling historical windows and breach rate
- **Deterministic Config**: Centralized runtime config with deterministic seeding for reproducibility
- **Caching**: Yahoo fetch results cached locally with configurable TTL
- **Modular Design**: Clean, organized code structure with separate modules for each functionality

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/JustTryingToDoBetter/portfolio-simulation.git
   cd portfolio-simulation
   ```

2. Install dependencies using renv:
   ```r
   renv::restore()
   ```

## Usage

The main simulation can be run using the `main.R` script:

```r
source("R/data_fetch.R")
source("R/sim_mc.R")
source("R/returns.R")
source("R/risk_metrics.R")
source("R/backtest.R")
source("R/config.R")
library(dplyr)

# Load deterministic config
cfg <- load_config()
set_deterministic_seed(cfg$seed)

# Fetch data and compute returns
prices <- fetch_prices_yahoo(
   cfg$tickers,
   from = cfg$from,
   cache_dir = cfg$cache_dir,
   cache_max_age_hours = cfg$cache_max_age_hours
)
rets <- compute_log_returns(prices)
mat <- returns_wide_matrix(rets)

# Simulate portfolio
sim_port <- simulate_portfolio_mvn(mat, cfg$weights, n_sims = cfg$n_sims)

# Calculate risk metrics
risk <- var_cvar(sim_port, alpha = cfg$alpha)
print(risk)

# Rolling VaR backtest
hist_port_rets <- as.numeric(mat %*% cfg$weights)
bt <- rolling_var_backtest(hist_port_rets, window = cfg$var_window, alpha = cfg$alpha)
print(backtest_summary(bt, cfg$alpha))
```

## Project Structure

```
portfolio-simulation/
├── main.R                 # Main execution script
├── R/
│   ├── data_fetch.R       # Data fetching utilities
│   ├── returns.R          # Return calculation functions
│   ├── sim_mc.R           # Monte Carlo simulation
│   ├── risk_metrics.R     # Risk metric calculations
│   └── backtest.R         # Backtesting framework (placeholder)
├── api/                   # API endpoints (future development)
├── app/                   # Web application (future development)
├── data/
│   ├── cache/             # Cached data storage
│   └── outputs/           # Simulation outputs
├── tests/                 # Unit tests (future development)
├── renv/                  # Dependency management
└── README.md
```

## Key Functions

- `fetch_prices_yahoo()`: Downloads historical price data from Yahoo Finance
- `compute_log_returns()`: Calculates logarithmic returns from price series
- `returns_wide_matrix()`: Converts returns to wide matrix format for simulation
- `simulate_portfolio_mvn()`: Runs Monte Carlo simulation using multivariate normal distribution
- `var_cvar()`: Computes Value at Risk and Conditional VaR
- `rolling_var_backtest()`: Performs rolling-window VaR backtest
- `backtest_summary()`: Reports breach-rate diagnostics for the backtest

## Tests

Run basic unit tests:

```r
source("tests/run_tests.R")
```

## Dependencies

This project uses renv for dependency management. Key packages include:

- quantmod: Financial data retrieval
- dplyr/tidyr: Data manipulation
- MASS: Multivariate normal simulation
- PerformanceAnalytics: Financial analytics (available in renv)

