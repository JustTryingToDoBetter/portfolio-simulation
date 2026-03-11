# Intent: reporting layer for portfolio risk simulation artifacts.
#
# Functions read the canonical run artifact and produce structured summaries
# and a markdown report. All I/O is explicit; nothing writes to disk unless
# the caller requests it via an output path argument.
#
# Typical call sequence:
#   artifact <- readRDS("outputs/latest_run.rds")
#   summary  <- summarize_run_artifact(artifact)
#   write_run_summary_markdown(artifact, output_dir = "outputs/reports")

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Null-safe accessor with a scalar fallback.
.rpt_or <- function(x, default) if (!is.null(x)) x else default

# Format a numeric value for display; return "N/A" when NULL or non-finite.
.fmt_num <- function(x, digits = 4) {
  if (is.null(x) || !is.finite(x)) return("N/A")
  formatC(round(x, digits), format = "f", digits = digits)
}

# Format a p-value, flagging significance at 0.05.
.fmt_pval <- function(x) {
  if (is.null(x) || !is.finite(x)) return("N/A")
  flag <- if (x < 0.05) "  (*)" else ""
  paste0(round(x, 4), flag)
}

# Right-pad a label string for aligned two-column markdown tables.
.md_row <- function(label, value) {
  paste0("| ", label, " | ", value, " |")
}

# Bullet-list display for a named numeric vector.
.fmt_weights <- function(tickers, weights) {
  if (is.null(tickers) || is.null(weights)) return("N/A")
  parts <- mapply(function(t, w) paste0(t, ": ", round(w, 4)), tickers, weights)
  paste(parts, collapse = ", ")
}

# ---------------------------------------------------------------------------
# summarize_run_artifact
#
# Accepts a run artifact list and returns a clean structured summary list.
# All fields have safe fallback values so the function succeeds even with
# artifacts that predate specific fields.
#
# Arguments:
#   artifact - named list as returned by run_pipeline()
#
# Returns a named list with human-readable summary fields.
# ---------------------------------------------------------------------------
summarize_run_artifact <- function(artifact) {
  if (!is.list(artifact))
    stop("`artifact` must be a list (as returned by run_pipeline()).", call. = FALSE)

  meta    <- .rpt_or(artifact$sample_metadata, list())
  cfg     <- .rpt_or(artifact$cfg, list())
  bt      <- .rpt_or(artifact$backtest, list())
  kupiec  <- .rpt_or(artifact$kupiec, list())
  cc      <- artifact$christoffersen
  garch_s <- .rpt_or(artifact$garch_status, list())
  timings <- artifact$timings

  # Portfolio configuration
  tickers <- .rpt_or(meta$tickers, .rpt_or(cfg$tickers, artifact$matrix_cols))
  weights <- .rpt_or(meta$weights, cfg$weights)
  alpha   <- .rpt_or(meta$alpha, .rpt_or(cfg$alpha, 0.95))

  # Sample period
  start_date <- .rpt_or(meta$start_date,
                 .rpt_or(artifact$date_range[[1]], NA_character_))
  end_date   <- .rpt_or(meta$end_date,
                 .rpt_or(artifact$date_range[[2]], NA_character_))
  n_obs      <- .rpt_or(meta$n_obs, .rpt_or(artifact$n_obs, NA_integer_))

  # Risk metrics per model
  models_available <- c("mvn", "bootstrap", "stress_bootstrap", "garch")
  model_metrics_raw <- .rpt_or(artifact$model_metrics, list())
  risk_summary <- lapply(models_available, function(key) {
    m <- model_metrics_raw[[key]]
    if (is.null(m)) return(NULL)
    list(model = key, VaR = m$VaR, CVaR = m$CVaR)
  })
  names(risk_summary) <- models_available
  # Drop NULL entries for cleaner downstream use.
  risk_summary <- Filter(Negate(is.null), risk_summary)

  # Backtest
  breach_rate   <- .rpt_or(bt$breach_rate, NA_real_)
  breach_count  <- .rpt_or(bt$breach_count, NA_integer_)
  total_tested  <- .rpt_or(bt$total_tested, NA_integer_)
  exp_rate      <- .rpt_or(bt$expected_breach_rate, 1 - alpha)
  backtest_model <- .rpt_or(bt$model, .rpt_or(meta$backtest_model,
                             .rpt_or(cfg$model, "unknown")))

  # Kupiec
  kupiec_pval     <- .rpt_or(kupiec$p_value, NA_real_)
  kupiec_stat     <- .rpt_or(kupiec$statistic, NA_real_)
  obs_viol_rate   <- .rpt_or(kupiec$observed_violation_rate, NA_real_)

  # Christoffersen
  cc_pval     <- if (!is.null(cc)) .rpt_or(cc$p_value, NA_real_) else NA_real_
  cc_stat     <- if (!is.null(cc)) .rpt_or(cc$statistic, NA_real_) else NA_real_
  cc_ind_pval <- if (!is.null(cc)) .rpt_or(cc$ind_p_value, NA_real_) else NA_real_

  # Benchmark timings
  timing_df   <- if (is.data.frame(timings) && nrow(timings) > 0) timings else NULL
  total_secs  <- if (!is.null(timing_df)) sum(timing_df$seconds) else NA_real_

  # GARCH status
  garch_enabled   <- isTRUE(garch_s$enabled)
  garch_succeeded <- isTRUE(garch_s$succeeded)
  garch_message   <- .rpt_or(garch_s$message, if (garch_enabled) "unknown" else "disabled")

  list(
    run_timestamp    = .rpt_or(artifact$run_timestamp, NA_character_),
    git_commit       = .rpt_or(artifact$git_commit,    NA_character_),
    seed             = .rpt_or(artifact$seed,          NA_integer_),
    tickers          = tickers,
    weights          = weights,
    start_date       = start_date,
    end_date         = end_date,
    n_obs            = n_obs,
    alpha            = alpha,
    backtest_model   = backtest_model,
    risk_summary     = risk_summary,
    breach_count     = breach_count,
    total_tested     = total_tested,
    breach_rate      = breach_rate,
    expected_rate    = exp_rate,
    obs_viol_rate    = obs_viol_rate,
    kupiec_pval      = kupiec_pval,
    kupiec_stat      = kupiec_stat,
    cc_pval          = cc_pval,
    cc_stat          = cc_stat,
    cc_ind_pval      = cc_ind_pval,
    timing_df        = timing_df,
    total_runtime_s  = total_secs,
    garch_enabled    = garch_enabled,
    garch_succeeded  = garch_succeeded,
    garch_message    = garch_message
  )
}

# ---------------------------------------------------------------------------
# write_run_summary_markdown
#
# Generates a markdown report under `output_dir` and returns the file path.
# The report is structured for GitHub or project review.
#
# Arguments:
#   artifact   - run artifact list
#   output_dir - directory to write the report into (created if absent)
#   filename   - output filename (default: "latest_run_summary.md")
#
# Returns the absolute path of the written file.
# ---------------------------------------------------------------------------
write_run_summary_markdown <- function(artifact,
                                       output_dir = "outputs/reports",
                                       filename   = "latest_run_summary.md") {
  if (!is.list(artifact))
    stop("`artifact` must be a list.", call. = FALSE)
  if (!is.character(output_dir) || nchar(output_dir) == 0)
    stop("`output_dir` must be a non-empty string.", call. = FALSE)

  s <- summarize_run_artifact(artifact)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, filename)

  # Helper to emit a markdown table row.
  row <- function(label, value) paste0("| ", label, " | ", value, " |")

  lines <- character(0)
  emit  <- function(...) lines <<- c(lines, paste0(...))

  # -- Header ------------------------------------------------------------------
  emit("# Portfolio Risk Simulation Report")
  emit("")
  emit("*Generated: ", .rpt_or(s$run_timestamp, "unknown"), "*")
  if (!is.na(s$git_commit))
    emit("*Git commit: `", s$git_commit, "`*")
  emit("")

  # -- Overview ----------------------------------------------------------------
  emit("## Overview")
  emit("")
  emit("| Field | Value |")
  emit("|---|---|")
  emit(row("Run timestamp",    .rpt_or(s$run_timestamp, "N/A")))
  emit(row("Seed",             .rpt_or(as.character(s$seed), "N/A")))
  emit(row("Total runtime",    paste0(.fmt_num(s$total_runtime_s, 1), " s")))
  emit(row("GARCH enabled",    if (s$garch_enabled) "yes" else "no"))
  if (s$garch_enabled)
    emit(row("GARCH status",   s$garch_message))
  emit("")

  # -- Portfolio configuration -------------------------------------------------
  emit("## Portfolio Configuration")
  emit("")
  emit("| Field | Value |")
  emit("|---|---|")
  emit(row("Tickers",    paste(s$tickers, collapse = ", ")))
  emit(row("Weights",    .fmt_weights(s$tickers, s$weights)))
  emit(row("From",       .rpt_or(s$start_date, "N/A")))
  emit(row("To",         .rpt_or(s$end_date,   "N/A")))
  emit(row("Observations", .rpt_or(as.character(s$n_obs), "N/A")))
  emit(row("Alpha",      as.character(s$alpha)))
  emit(row("Backtest model", s$backtest_model))
  emit("")

  # -- Risk metrics ------------------------------------------------------------
  emit("## Risk Metrics by Model")
  emit("")
  emit("| Model | VaR | CVaR |")
  emit("|---|---|---|")
  for (key in names(s$risk_summary)) {
    m   <- s$risk_summary[[key]]
    lbl <- switch(key,
      mvn              = "MVN",
      bootstrap        = "Bootstrap",
      stress_bootstrap = "Stress Bootstrap",
      garch            = "GARCH",
      key
    )
    emit("| ", lbl, " | ", .fmt_num(m$VaR), " | ", .fmt_num(m$CVaR), " |")
  }
  emit("")

  # Note on interpretation
  emit("> VaR and CVaR are expressed as portfolio loss fractions ")
  emit("> (positive = loss). Alpha = ", s$alpha, ".")
  emit("")

  # -- Stress scenario ---------------------------------------------------------
  stress <- s$risk_summary[["stress_bootstrap"]]
  base   <- s$risk_summary[["bootstrap"]]
  if (!is.null(stress) && !is.null(base)) {
    emit("## Stress Scenario")
    emit("")
    vcfg <- artifact$cfg
    scale <- .rpt_or(vcfg$vol_scale_stress, NA_real_)
    emit("| Field | Value |")
    emit("|---|---|")
    emit(row("Vol scale factor",
             if (is.na(scale)) "N/A" else paste0(scale, "x")))
    emit(row("Stress VaR",  .fmt_num(stress$VaR)))
    emit(row("Stress CVaR", .fmt_num(stress$CVaR)))
    var_lift <- if (base$VaR > 0) (stress$VaR - base$VaR) / base$VaR else NA_real_
    emit(row("VaR lift vs baseline",
             if (is.finite(var_lift)) paste0(round(var_lift * 100, 1), "%") else "N/A"))
    emit("")
  }

  # -- Backtest validation -----------------------------------------------------
  emit("## Backtest Validation")
  emit("")
  emit("| Field | Value |")
  emit("|---|---|")
  emit(row("Observations tested",  .rpt_or(as.character(s$total_tested), "N/A")))
  emit(row("Breach count",         .rpt_or(as.character(s$breach_count), "N/A")))
  emit(row("Observed breach rate", paste0(round(.rpt_or(s$breach_rate, NA_real_) * 100, 2), "%")))
  emit(row("Expected breach rate", paste0(round(s$expected_rate * 100, 2), "%")))
  emit("")

  emit("### Kupiec Unconditional Coverage (H0: correct rate)")
  emit("")
  emit("| Statistic | Value |")
  emit("|---|---|")
  emit(row("LR statistic", .fmt_num(s$kupiec_stat, 4)))
  emit(row("p-value",      .fmt_pval(s$kupiec_pval)))
  emit("")
  if (!is.na(s$kupiec_pval) && s$kupiec_pval < 0.05) {
    emit("> (*) p < 0.05: evidence against correct unconditional coverage.")
  } else {
    emit("> p >= 0.05: no strong evidence against correct unconditional coverage.")
  }
  emit("")

  if (!is.na(s$cc_pval)) {
    emit("### Christoffersen Conditional Coverage (H0: iid correct rate)")
    emit("")
    emit("| Statistic | Value |")
    emit("|---|---|")
    emit(row("CC LR statistic",    .fmt_num(s$cc_stat, 4)))
    emit(row("CC p-value",         .fmt_pval(s$cc_pval)))
    emit(row("Independence p-value", .fmt_pval(s$cc_ind_pval)))
    emit("")
  }

  # -- Benchmark summary -------------------------------------------------------
  if (!is.null(s$timing_df)) {
    emit("## Benchmark Summary")
    emit("")
    emit("| Stage | Elapsed (s) |")
    emit("|---|---|")
    for (i in seq_len(nrow(s$timing_df))) {
      emit(row(s$timing_df$step[[i]],
               .fmt_num(s$timing_df$seconds[[i]], 2)))
    }
    emit(row("**Total**",
             paste0("**", .fmt_num(s$total_runtime_s, 2), "**")))
    emit("")
  }

  # -- Key observations --------------------------------------------------------
  emit("## Key Observations")
  emit("")

  # Stress lift
  if (!is.null(stress) && !is.null(base) && is.finite(var_lift)) {
    direction <- if (var_lift > 0) "higher" else "lower"
    emit("- Stress VaR is **", round(abs(var_lift) * 100, 1),
         "%** ", direction, " than baseline bootstrap VaR, ",
         "consistent with the applied volatility scaling.")
  }

  # Model ordering
  all_var <- sapply(names(s$risk_summary), function(k) s$risk_summary[[k]]$VaR)
  if (length(all_var) >= 2) {
    highest_model <- names(which.max(all_var))
    emit("- Highest VaR comes from the **",
         switch(highest_model,
           mvn = "MVN", bootstrap = "Bootstrap",
           stress_bootstrap = "Stress Bootstrap", garch = "GARCH",
           highest_model),
         "** model (", .fmt_num(all_var[[highest_model]]), ").")
  }

  # Breach rate interpretation
  if (!is.na(s$breach_rate) && !is.na(s$expected_rate)) {
    ratio <- s$breach_rate / max(s$expected_rate, 1e-9)
    if (ratio > 1.5)
      emit("- Observed breach rate is **", round(ratio, 2),
           "x** the expected rate: model may be under-estimating risk.")
    else if (ratio < 0.5)
      emit("- Observed breach rate is much lower than expected: ",
           "model may be over-estimating risk.")
    else
      emit("- Breach rate is broadly in line with the expected rate of ",
           round(s$expected_rate * 100, 2), "%.")
  }

  # Kupiec verdict
  if (!is.na(s$kupiec_pval)) {
    verdict <- if (s$kupiec_pval >= 0.05) "passes" else "fails"
    emit("- Kupiec unconditional coverage test **", verdict,
         "** at the 5% level (p = ", round(s$kupiec_pval, 4), ").")
  }

  # Slowest stage
  if (!is.null(s$timing_df) && nrow(s$timing_df) > 0) {
    idx_slow <- which.max(s$timing_df$seconds)
    emit("- Slowest pipeline stage: **", s$timing_df$step[[idx_slow]],
         "** (", round(s$timing_df$seconds[[idx_slow]], 1), " s).")
  }
  emit("")

  # -- Footer ------------------------------------------------------------------
  emit("---")
  emit("*Report generated by portfolio-simulation reporting module.*")

  writeLines(lines, out_path)
  message("Report written: ", out_path)
  invisible(out_path)
}

# ---------------------------------------------------------------------------
# render_run_report
#
# Orchestrates artifact loading, all five plots, and the markdown report.
# Returns the summary list invisibly so callers can inspect it.
#
# Arguments:
#   artifact_path  - path to the RDS artifact (default: "outputs/latest_run.rds")
#   plots_dir      - directory for output PNGs
#   reports_dir    - directory for the markdown report
#   save_plots     - logical; set FALSE to skip writing PNGs (useful in tests)
#
# Returns the summarize_run_artifact() list invisibly.
# ---------------------------------------------------------------------------
render_run_report <- function(artifact_path = "outputs/latest_run.rds",
                              plots_dir     = "outputs/plots",
                              reports_dir   = "outputs/reports",
                              save_plots    = TRUE) {
  if (!file.exists(artifact_path))
    stop("Artifact not found: ", artifact_path, call. = FALSE)

  artifact <- readRDS(artifact_path)

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required. Install it with install.packages('ggplot2').",
         call. = FALSE)

  # Plotting source is expected to already be sourced by the caller,
  # but guard defensively by checking for the function existence.
  if (!exists("plot_loss_distribution", mode = "function"))
    stop("plots.R must be sourced before calling render_run_report().",
         call. = FALSE)

  # -- Plots -------------------------------------------------------------------
  message("Generating plots...")

  # Loss distribution for each available simulation model.
  sims <- artifact$simulations
  for (key in c("mvn", "bootstrap", "stress_bootstrap", "garch")) {
    if (is.null(sims[[key]])) next
    path <- if (save_plots)
      file.path(plots_dir, paste0("loss_distribution_", key, ".png"))
    else
      NULL
    tryCatch(
      plot_loss_distribution(artifact, model = key, save_to = path),
      error = function(e)
        message("Skipping loss distribution plot for '", key, "': ", e$message)
    )
  }

  # Backtest plot.
  tryCatch(
    plot_backtest_var(artifact,
      save_to = if (save_plots) file.path(plots_dir, "backtest_var.png") else NULL),
    error = function(e)
      message("Skipping backtest_var plot: ", e$message)
  )

  # Breach timeline.
  tryCatch(
    plot_breach_timeline(artifact,
      save_to = if (save_plots) file.path(plots_dir, "breach_timeline.png") else NULL),
    error = function(e)
      message("Skipping breach_timeline plot: ", e$message)
  )

  # Model comparison.
  tryCatch(
    plot_model_risk_comparison(artifact,
      save_to = if (save_plots) file.path(plots_dir, "model_risk_comparison.png") else NULL),
    error = function(e)
      message("Skipping model_risk_comparison plot: ", e$message)
  )

  # Benchmark timings.
  tryCatch(
    plot_benchmark_timings(artifact,
      save_to = if (save_plots) file.path(plots_dir, "benchmark_timings.png") else NULL),
    error = function(e)
      message("Skipping benchmark_timings plot: ", e$message)
  )

  # -- Report ------------------------------------------------------------------
  message("Writing markdown report...")
  write_run_summary_markdown(artifact, output_dir = reports_dir)

  invisible(summarize_run_artifact(artifact))
}
