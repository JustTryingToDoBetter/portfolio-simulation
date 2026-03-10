# Portfolio Simulation Engine

A modular **financial risk and portfolio simulation engine in R** for estimating downside risk under multiple Monte Carlo approaches.

This project models portfolio loss using:

- **Multivariate Normal Monte Carlo**
- **Historical Bootstrap Simulation**
- **Volatility Stress Scenarios**
- **Optional GARCH-based simulation**

It also includes:

- **VaR and CVaR risk estimation**
- **Rolling backtesting**
- **Kupiec calibration testing**
- **Artifact persistence**
- **API and Shiny app scaffolding**

The goal is to bridge **quantitative finance**, **data science**, and **software engineering** by building a system that is not only analytically useful, but also modular, testable, and product-ready.

---

## Why this project matters

In financial risk management, a model is only useful if it can answer two questions:

1. **How bad can portfolio losses get?**
2. **Does the risk model hold up when tested against realized outcomes?**

This engine addresses both.

It estimates **Value at Risk (VaR)** and **Conditional Value at Risk (CVaR)** using multiple simulation techniques, then validates those estimates with a **rolling backtest** and **Kupiec unconditional coverage test**.

This makes the project more than a basic Monte Carlo script. It is a small but structured **risk engineering system**.

---

## Core Features

### Simulation models
- **MVN Monte Carlo**
        - fast baseline model using mean vector + covariance matrix
- **Historical Bootstrap**
        - resamples observed historical return days
        - preserves empirical tail behavior better than Gaussian simulation
- **Stress Bootstrap**
        - scales volatility around the historical mean to simulate stressed conditions
- **Optional GARCH**
        - models volatility clustering using per-asset conditional variance processes

### Risk metrics
- **VaR**
- **CVaR**

### Validation
- **Rolling 1-day VaR backtesting**
- **Observed vs expected breach rate**
- **Kupiec unconditional coverage test**

### Engineering features
- deterministic runs with `set.seed(42)`
- cached market data
- modular architecture
- timing / benchmark logging
- saved run artifacts
- test scaffolding
- Shiny + plumber integration points

---

## Example Findings

Using an equal-weight portfolio of:

- AAPL
- MSFT
- GOOGL
- AMZN

the engine produced results with the following pattern:

- **MVN VaR** and **Bootstrap VaR** were very similar
- **Bootstrap CVaR** was materially worse than Gaussian CVaR
- **Stress scenarios** widened tail loss significantly
- **Rolling backtest breach rate** was close to the expected 5%
- **Kupiec p-value** did not reject the bootstrap VaR model

### Interpretation
This suggests that while normal-model VaR may approximate the threshold of bad outcomes, it can understate the **severity of tail losses**, which becomes clearer under bootstrap and stress-based simulation.

That is a common and important result in practical risk modeling.

---

## Architecture

```text
Yahoo Finance Data
                                ↓
Price Cleaning + Alignment
                                ↓
Log Return Computation
                                ↓
Simulation Engine
        ├─ MVN
        ├─ Bootstrap
        ├─ Stress Bootstrap
        └─ Optional GARCH
                                ↓
Risk Metrics
        ├─ VaR
        └─ CVaR
                                ↓
Validation
        ├─ Rolling Backtest
        └─ Kupiec Test
                                ↓
Outputs / Delivery
        ├─ RDS artifacts
        ├─ CSV summaries
        ├─ plots
        ├─ plumber API
        └─ Shiny app
```

## Quickstart (GitHub Codespaces)

Restore packages and run:

```bash
R -q -e 'renv::restore()'
R -q -f main.R
```

Or from an interactive R session:

```r
source("main.R")
```

## Run modes

```bash
RISK_MODE=quick R -q -f main.R
RISK_MODE=full R -q -f main.R
```

- `quick`: lower simulation counts for faster iteration
- `full`: larger simulation counts for final evaluation and artifact generation

`full` is the default.

## VaR / CVaR Convention

This project uses a loss-based convention:

portfolio returns are converted to losses using:

```text
loss = -return
```

- VaR is the `alpha` quantile of simulated losses
- CVaR is the average loss in the tail where `loss >= VaR`

This keeps VaR and CVaR expressed as positive downside values.

## Models

### 1. MVN

`simulate_portfolio_mvn`

Baseline Monte Carlo model using:

- historical mean returns
- covariance matrix
- multivariate normal simulation

Useful as a benchmark, but it may underestimate tail severity due to the Gaussian assumption.

### 2. Historical Bootstrap

`simulate_portfolio_bootstrap`

Resamples historical return days with replacement.

Advantages:

- preserves empirical return distribution
- captures fat-tail behavior better than MVN
- simple and robust

### 3. Stress Bootstrap

`simulate_portfolio_bootstrap(..., vol_scale > 1)`

Applies volatility scaling around the historical mean to create stressed scenarios.

Useful for:

- stress testing
- scenario analysis
- comparing baseline vs elevated volatility conditions

### 4. Optional GARCH

`R/garch.R`

Extends the engine to model volatility clustering using per-asset GARCH processes.

This is useful because financial returns often exhibit:

- changing volatility over time
- clustered high-volatility periods
- stronger tail exposure during stressed regimes

If enabled, GARCH can provide a more realistic conditional-risk estimate than static models.

## Backtesting and Validation

### Rolling VaR backtest

The engine performs a rolling-window backtest:

- fit / simulate on a trailing window
- estimate 1-day VaR
- compare against realized next-day return

A breach occurs when:

```text
(-realized_return) > VaR
```

### Kupiec unconditional coverage test

The Kupiec test evaluates whether the observed number of VaR breaches is statistically consistent with the expected breach probability.

For example:

- at `alpha = 0.95`
- expected breach rate is `0.05`

A high p-value indicates that the model is not rejected on unconditional coverage grounds.

## API and Shiny

### plumber API

Minimal API scaffold:

```r
pr <- plumber::pr("api/plumber.R")
pr$run(port = 8000)
```

From terminal:

```bash
R -q -e 'pr <- plumber::pr("api/plumber.R"); pr$run(port = 8000, host = "0.0.0.0")'
```

Available endpoints:

- `GET /health`
- `POST /risk`

Expected `POST /risk` payload fields:

- `tickers`
- `weights`
- `from`
- `alpha`
- `n_sims`
- `model`
- `vol_scale`

### Shiny app

Launch locally:

```r
source("app/app.R")
```

Or from terminal:

```bash
R -q -e 'shiny::runApp("app", host = "0.0.0.0", port = 3838)'
```

The app is intended as a lightweight interface for:

- selecting tickers
- choosing simulation model
- adjusting alpha
- applying volatility stress
- viewing VaR/CVaR outputs

## Outputs

Generated outputs include:

- cached Yahoo data: `data/cache/`
- latest run artifact: `outputs/latest_run.rds`
- model comparison CSV
- summary plots

The main run artifact contains:

- `cfg`
- `risk_mvn`
- `risk_boot`
- `risk_boot_stress`
- `backtest`
- `kupiec`
- `timings`
- `garch`

This makes runs reproducible and reviewable.

## Project Structure

```text
R/
        data_fetch.R
        returns.R
        sim_mc.R
        risk_metrics.R
        backtest.R
        validation.R
        logging.R
        benchmark.R
        garch.R

api/
        plumber.R

app/
        app.R

outputs/
data/cache/
tests/
main.R
README.md
```

## Tests

Run the current test suite with:

```r
source("tests/run_tests.R")
```

## Roadmap

Planned next improvements:

- filtered historical simulation
- Christoffersen independence test
- portfolio optimization layer
- Dockerized deployment
- richer Shiny visualizations
- CI pipeline for automated test runs

## Positioning

This project is designed to demonstrate capability across:

- R for quantitative finance
- Monte Carlo simulation
- time series and volatility modeling
- risk backtesting and validation
- modular software engineering
- early-stage productization via API and UI