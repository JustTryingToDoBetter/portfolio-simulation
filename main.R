source("R/data_fetch.R")
source("R/sim_mc.R")
source("R/returns.R")
source("R/risk_metrics.R")
source("R/backtest.R")
source("R/validation.R")
source("R/logging.R")
source("R/benchmark.R")
source("R/garch.R")

# ---------------------------------------------------------------------------
# run_pipeline: main orchestration function
#
# Arguments:
#   cfg       - named list of run parameters (validated at runtime)
#   seed      - integer RNG seed for reproducibility
#   fetch_fn  - price-fetch function injected for testability
#               signature: function(tickers, from, cache_dir) -> prices tibble
#
# Returns:
#   A named list (artifact) with risk estimates, backtest output, and metadata.
# ---------------------------------------------------------------------------
run_pipeline <- function(cfg, seed = 42L, fetch_fn = fetch_prices_yahoo_cached) {

  # -- Config validation -------------------------------------------------------
  if (!is.character(cfg$tickers) || length(cfg$tickers) < 1)
    stop("`cfg$tickers` must be a non-empty character vector.", call. = FALSE)
  if (!is.numeric(cfg$weights) || length(cfg$weights) != length(cfg$tickers))
    stop("`cfg$weights` must be numeric and match length of `cfg$tickers`.", call. = FALSE)
  if (!is.null(names(cfg$weights)) && !setequal(names(cfg$weights), cfg$tickers))
    stop("`cfg$weights` names must match `cfg$tickers` exactly.", call. = FALSE)
  if (abs(sum(cfg$weights) - 1) > 1e-6)
    stop("`cfg$weights` must sum to 1 (got ", round(sum(cfg$weights), 8), ").", call. = FALSE)
  if (!is.numeric(cfg$alpha) || length(cfg$alpha) != 1 || cfg$alpha <= 0 || cfg$alpha >= 1)
    stop("`cfg$alpha` must be a single number in (0, 1).", call. = FALSE)
  if (!is.numeric(cfg$n_sims) || length(cfg$n_sims) != 1 || cfg$n_sims < 1)
    stop("`cfg$n_sims` must be a positive scalar.", call. = FALSE)
  if (!is.numeric(cfg$backtest_window) || length(cfg$backtest_window) != 1 || cfg$backtest_window < 20)
    stop("`cfg$backtest_window` must be >= 20.", call. = FALSE)
  if (!is.numeric(cfg$backtest_sims) || length(cfg$backtest_sims) != 1 || cfg$backtest_sims < 1000)
    stop("`cfg$backtest_sims` must be >= 1000.", call. = FALSE)
  if (!is.numeric(cfg$vol_scale_stress) || length(cfg$vol_scale_stress) != 1 || cfg$vol_scale_stress <= 0)
    stop("`cfg$vol_scale_stress` must be a positive scalar.", call. = FALSE)
  if (!is.character(cfg$from) || length(cfg$from) != 1 || nchar(cfg$from) == 0)
    stop("`cfg$from` must be a non-empty date string.", call. = FALSE)
  valid_models <- c("bootstrap", "mvn")
  if (!is.character(cfg$model) || !(cfg$model %in% valid_models))
    stop("`cfg$model` must be one of: ", paste(valid_models, collapse = ", "), ".", call. = FALSE)

  set.seed(as.integer(seed))

  resolve_date_range <- function(index_values) {
    if (is.null(index_values) || length(index_values) == 0) {
      return(c(NA_character_, NA_character_))
    }

    parsed_dates <- suppressWarnings(as.Date(index_values))
    if (all(!is.na(parsed_dates))) {
      return(as.character(range(parsed_dates)))
    }

    c(as.character(index_values[[1]]), as.character(index_values[[length(index_values)]]))
  }

  run_ts     <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  git_commit <- tryCatch(
    trimws(system("git rev-parse --short HEAD", intern = TRUE)),
    error   = function(e) NA_character_,
    warning = function(e) NA_character_
  )

  log_info("run_pipeline seed=", seed, " tickers=", paste(cfg$tickers, collapse = ","))
  bench <- list()

  # -- Fetch prices ------------------------------------------------------------
  bench[[length(bench) + 1]] <- time_it("fetch_prices", {
    fetch_fn(cfg$tickers, from = cfg$from, cache_dir = cfg$cache_dir)
  })
  prices <- bench[[length(bench)]]$value

  # -- Compute log returns and build wide matrix -------------------------------
  bench[[length(bench) + 1]] <- time_it("compute_returns", {
    rets <- compute_log_returns(prices)
    returns_wide_matrix(rets)
  })
  mat <- bench[[length(bench)]]$value

  # -- Returns matrix validation -----------------------------------------------
  if (!is.matrix(mat))
    stop("Returns matrix must be a matrix.", call. = FALSE)
  missing_tickers <- setdiff(cfg$tickers, colnames(mat))
  if (length(missing_tickers) > 0)
    stop("Returns matrix is missing columns: ", paste(missing_tickers, collapse = ", "), ".", call. = FALSE)

  # Reorder columns to cfg$tickers order so positional weight alignment is safe.
  mat <- mat[, cfg$tickers, drop = FALSE]

  if (any(!is.finite(mat)))
    stop("Returns matrix contains NA/NaN/Inf values.", call. = FALSE)
  if (nrow(mat) <= cfg$backtest_window)
    stop("Not enough rows in returns matrix (", nrow(mat),
         ") for backtest window (", cfg$backtest_window, ").", call. = FALSE)

  # Resolve weights: use names when provided (subset + reorder), else positional.
  weights <- if (!is.null(names(cfg$weights))) {
    as.numeric(cfg$weights[cfg$tickers])
  } else {
    as.numeric(cfg$weights)
  }

  log_info("matrix rows=", nrow(mat), " cols=", paste(colnames(mat), collapse = ","))

  # -- MVN simulation ----------------------------------------------------------
  bench[[length(bench) + 1]] <- time_it("simulate_mvn", {
    simulate_portfolio_mvn(mat, weights, n_sims = cfg$n_sims)
  })
  sim_mvn <- bench[[length(bench)]]$value

  # -- Bootstrap simulation (baseline vol) -------------------------------------
  bench[[length(bench) + 1]] <- time_it("simulate_bootstrap", {
    simulate_portfolio_bootstrap(mat, weights, n_sims = cfg$n_sims,
                                 vol_scale = cfg$vol_scale_baseline)
  })
  sim_boot <- bench[[length(bench)]]$value

  # -- Risk metrics: MVN and bootstrap -----------------------------------------
  bench[[length(bench) + 1]] <- time_it("risk_metrics", {
    list(
      mvn       = var_cvar(sim_mvn,  alpha = cfg$alpha),
      bootstrap = var_cvar(sim_boot, alpha = cfg$alpha)
    )
  })
  risk_pair <- bench[[length(bench)]]$value
  risk_mvn  <- risk_pair$mvn
  risk_boot <- risk_pair$bootstrap

  # -- Stress simulation (scaled bootstrap) ------------------------------------
  bench[[length(bench) + 1]] <- time_it("sim_boot_stress", {
    simulate_portfolio_bootstrap(mat, weights, n_sims = cfg$n_sims,
                                 vol_scale = cfg$vol_scale_stress)
  })
  sim_boot_stress <- bench[[length(bench)]]$value

  bench[[length(bench) + 1]] <- time_it("risk_boot_stress", {
    var_cvar(sim_boot_stress, alpha = cfg$alpha)
  })
  risk_boot_stress <- bench[[length(bench)]]$value

  # -- Rolling VaR backtest ----------------------------------------------------
  bench[[length(bench) + 1]] <- time_it("rolling_var_backtest", {
    backtest_var(
      returns_mat = mat,
      weights     = weights,
      window      = cfg$backtest_window,
      alpha       = cfg$alpha,
      model       = cfg$model,
      n_sims      = cfg$backtest_sims
    )
  })
  bt     <- bench[[length(bench)]]$value
  kupiec <- kupiec_uc_test(bt$breaches, alpha = cfg$alpha)
  christoffersen <- if (length(bt$breaches) >= 2) {
    christoffersen_cc_test(bt$breaches, alpha = cfg$alpha)
  } else {
    NULL
  }

  log_info("MVN VaR=", round(risk_mvn$VaR, 6), " CVaR=", round(risk_mvn$CVaR, 6))
  log_info("Bootstrap VaR=", round(risk_boot$VaR, 6), " CVaR=", round(risk_boot$CVaR, 6))
  log_info("Stress VaR=", round(risk_boot_stress$VaR, 6), " CVaR=", round(risk_boot_stress$CVaR, 6))
  log_info("breach_rate=", round(bt$breach_rate, 6), " expected=", round(1 - cfg$alpha, 6))
  log_info("kupiec_p=", round(kupiec$p_value, 6))

  # -- Optional GARCH ----------------------------------------------------------
  garch_result <- NULL
  garch_sims <- NULL
  garch_status <- list(
    enabled = isTRUE(cfg$enable_garch),
    succeeded = FALSE,
    message = if (isTRUE(cfg$enable_garch)) NULL else "disabled"
  )

  if (isTRUE(cfg$enable_garch)) {
    if (!requireNamespace("rugarch", quietly = TRUE)) {
      garch_status$message <- "`rugarch` is not installed"
      log_warn("enable_garch=TRUE but `rugarch` is not installed. Skipping GARCH.")
    } else {
      garch_result <- tryCatch({
        fits      <- lapply(seq_len(ncol(mat)), function(j) fit_garch_series(mat[, j]))
        sims_list <- lapply(seq_along(fits), function(j) {
          s <- simulate_garch_returns(fits[[j]], n = cfg$n_sims)
          if (!is.numeric(s) || length(s) != cfg$n_sims || !all(is.finite(s)))
            stop("GARCH simulation for column ", j, " returned invalid values.")
          s
        })
        sims_mat   <- do.call(cbind, sims_list)
        garch_port <- as.numeric(sims_mat %*% weights)
        garch_sims <<- garch_port
        g_risk     <- var_cvar(garch_port, alpha = cfg$alpha)
        garch_status$succeeded <<- TRUE
        garch_status$message <<- "ok"
        log_info("garch VaR=", round(g_risk$VaR, 6), " CVaR=", round(g_risk$CVaR, 6))
        g_risk
      }, error = function(e) {
        garch_status$message <<- conditionMessage(e)
        log_warn("GARCH failed: ", conditionMessage(e), ". Skipping GARCH results.")
        NULL
      })
    }
  }

  # -- Timings -----------------------------------------------------------------
  timings <- summarize_benchmarks(bench)
  print(timings)

  # -- Build and save artifact -------------------------------------------------
  artifact <- list(
    cfg              = cfg,
    seed             = as.integer(seed),
    run_timestamp    = run_ts,
    git_commit       = git_commit,
    session_info     = sessionInfo(),
    matrix_cols      = colnames(mat),
    n_obs            = nrow(mat),
    date_range       = resolve_date_range(rownames(mat)),
    sample_metadata  = list(
      tickers = cfg$tickers,
      weights = weights,
      start_date = resolve_date_range(rownames(mat))[[1]],
      end_date = resolve_date_range(rownames(mat))[[2]],
      n_obs = nrow(mat),
      alpha = cfg$alpha,
      backtest_model = cfg$model,
      run_timestamp = run_ts,
      seed = as.integer(seed)
    ),
    model_metrics    = list(
      mvn = risk_mvn,
      bootstrap = risk_boot,
      stress_bootstrap = risk_boot_stress,
      garch = garch_result
    ),
    simulations      = list(
      mvn = sim_mvn,
      bootstrap = sim_boot,
      stress_bootstrap = sim_boot_stress,
      garch = garch_sims
    ),
    risk_mvn         = risk_mvn,
    risk_boot        = risk_boot,
    risk_boot_stress = risk_boot_stress,
    backtest         = bt,
    kupiec           = kupiec,
    christoffersen   = christoffersen,
    timings          = timings,
    garch            = garch_result,
    garch_status     = garch_status
  )

  if (!dir.exists("outputs"))
    dir.create("outputs", recursive = TRUE)

  saveRDS(artifact, file = "outputs/latest_run.rds")
  log_info("artifact=outputs/latest_run.rds")
  invisible(artifact)
}

# ---------------------------------------------------------------------------
# Script entrypoint: runs when executed directly, not when sourced by tests.
# sys.nframe() == 0L is TRUE only at the top level (Rscript or source at console).
# ---------------------------------------------------------------------------
if (sys.nframe() == 0L) {
  mode <- Sys.getenv("RISK_MODE", unset = "full")
  if (!(mode %in% c("quick", "full"))) {
    log_warn("Unknown RISK_MODE=", mode, ". Falling back to full.")
    mode <- "full"
  }
  log_info("run_mode=", mode)

  cfg <- list(
    tickers            = c("AAPL", "MSFT", "GOOGL", "AMZN"),
    weights            = c(AAPL = 0.25, MSFT = 0.25, GOOGL = 0.25, AMZN = 0.25),
    from               = "2019-01-01",
    alpha              = 0.95,
    n_sims             = if (identical(mode, "quick")) 10000L else 50000L,
    backtest_window    = 252L,
    backtest_sims      = if (identical(mode, "quick")) 20000L else 100000L,
    cache_dir          = "data/cache",
    model              = "bootstrap",
    vol_scale_baseline = 1.0,
    vol_scale_stress   = 1.25,
    enable_garch       = FALSE
  )

  run_pipeline(cfg, seed = 42L)
}