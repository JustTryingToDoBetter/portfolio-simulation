# Portfolio Simulation

Risk engineering toolkit for Monte Carlo portfolio VaR/CVaR, rolling backtests, and validation.

## Quickstart (Codespaces)

```bash
R -q -e 'renv::restore()'
R -q -f main.R
```

Or inside an interactive R session:

```r
source("main.R")
```

Run modes:

```bash
RISK_MODE=quick R -q -f main.R
RISK_MODE=full  R -q -f main.R
```

`full` is the default.

## VaR/CVaR Convention

- Returns are converted to losses via `loss = -return`.
- VaR is the `alpha` quantile of simulated losses (positive loss threshold).
- CVaR is the mean loss in the tail where `loss >= VaR`.

## Models

- `mvn`: multivariate normal Monte Carlo (`simulate_portfolio_mvn`).
- `bootstrap`: historical day-resampling (`simulate_portfolio_bootstrap`).
- stress scenario: bootstrap with volatility scaling around the historical mean (`vol_scale > 1`).
- optional `garch`: per-asset sGARCH(1,1) scaffold in `R/garch.R` (disabled by default).

## Backtesting + Validation

- Rolling VaR backtest: `backtest_var(..., window = 252, model = "bootstrap"|"mvn")`.
- Breach rule uses loss convention: breach if `(-realized_return) > VaR`.
- Kupiec unconditional coverage test: `kupiec_uc_test(breaches, alpha)`.
- Main run prints observed breach rate vs expected breach rate (`1 - alpha`) and Kupiec p-value.

## API + Shiny

Minimal skeletons are provided and optional.

API (plumber):

```r
pr <- plumber::pr("api/plumber.R")
pr$run(port = 8000)
```

Endpoints:
- `GET /health`
- `POST /risk` with payload fields: `tickers`, `weights`, `from`, `alpha`, `n_sims`, `model`, `vol_scale`

Shiny app:

```r
source("app/app.R")
```

## Outputs

- Cached Yahoo data: `data/cache/`
- Run artifact: `outputs/latest_run.rds`
   - contains `cfg`, `risk_mvn`, `risk_boot`, `risk_boot_stress`, `backtest`, `kupiec`, `timings`, `garch`

## Project Structure

```
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
```

## Tests

```r
source("tests/run_tests.R")
```

