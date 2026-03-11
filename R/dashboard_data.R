# Intent: load, validate, and normalize run artifacts for dashboard rendering.

# ---------------------------------------------------------------------------
# Artifact loading
# ---------------------------------------------------------------------------

load_run_artifact <- function(path = "outputs/latest_run.rds") {
  if (!is.character(path) || length(path) != 1 || nchar(path) == 0) {
    stop("`path` must be a non-empty string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(
      "Artifact file not found at '", path, "'. ",
      "Run the pipeline first, then refresh the dashboard.",
      call. = FALSE
    )
  }
  readRDS(path)
}

# ---------------------------------------------------------------------------
# Artifact validation
# ---------------------------------------------------------------------------

validate_run_artifact <- function(artifact) {
  if (!is.list(artifact)) {
    stop("Artifact must be a list object loaded from RDS.", call. = FALSE)
  }

  required_fields <- c(
    "cfg", "sample_metadata", "model_metrics", "backtest", "kupiec"
  )
  missing_required <- required_fields[vapply(required_fields, function(nm) {
    is.null(artifact[[nm]])
  }, logical(1))]

  if (length(missing_required) > 0) {
    stop(
      "Artifact is missing required field(s): ",
      paste(missing_required, collapse = ", "),
      ". Re-run the pipeline to produce a complete artifact.",
      call. = FALSE
    )
  }

  warnings <- character(0)

  if (is.null(artifact$timings)) {
    warnings <- c(warnings, "Benchmark timings are missing.")
  }
  if (is.null(artifact$christoffersen)) {
    warnings <- c(warnings, "Christoffersen validation is not available.")
  }
  if (is.null(artifact$model_metrics$garch)) {
    warnings <- c(warnings, "GARCH metrics are not available for this run.")
  }
  if (is.null(artifact$simulations$garch)) {
    warnings <- c(warnings, "GARCH simulations are not available for this run.")
  }

  list(ok = TRUE, warnings = warnings)
}

# ---------------------------------------------------------------------------
# View model builder
# ---------------------------------------------------------------------------

build_dashboard_view_model <- function(artifact) {
  validation <- validate_run_artifact(artifact)
  summary <- summarize_run_artifact(artifact)

  mm <- artifact$model_metrics
  model_order <- c("mvn", "bootstrap", "stress_bootstrap", "garch")
  risk_rows <- lapply(model_order, function(k) {
    m <- mm[[k]]
    if (is.null(m)) return(NULL)
    data.frame(
      model_key = k,
      model = model_title(k),
      VaR = as.numeric(m$VaR),
      CVaR = as.numeric(m$CVaR),
      stringsAsFactors = FALSE
    )
  })
  risk_table <- do.call(rbind, Filter(Negate(is.null), risk_rows))
  if (is.null(risk_table)) {
    risk_table <- data.frame(
      model_key = character(0),
      model = character(0),
      VaR = numeric(0),
      CVaR = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  available_models <- risk_table$model_key

  bt <- artifact$backtest
  backtest <- list(
    breach_rate = as.numeric(bt$breach_rate),
    expected_rate = as.numeric(if (!is.null(bt$expected_breach_rate)) bt$expected_breach_rate else 1 - summary$alpha),
    breach_count = as.integer(bt$breach_count),
    total_tested = as.integer(bt$total_tested),
    model = if (!is.null(bt$model)) bt$model else summary$backtest_model
  )

  cc <- artifact$christoffersen
  transition <- if (!is.null(cc$transition_counts)) cc$transition_counts else NULL

  validation_tbl <- data.frame(
    Metric = c(
      "Observed breach rate",
      "Expected breach rate",
      "Breach count",
      "Observations tested",
      "Kupiec p-value",
      "Christoffersen independence p-value",
      "Christoffersen conditional coverage p-value"
    ),
    Value = c(
      fmt_pct(backtest$breach_rate),
      fmt_pct(backtest$expected_rate),
      as.character(backtest$breach_count),
      as.character(backtest$total_tested),
      fmt_pval(summary$kupiec_pval),
      fmt_pval(summary$cc_ind_pval),
      fmt_pval(summary$cc_pval)
    ),
    stringsAsFactors = FALSE
  )

  health_checks <- data.frame(
    section = c(
      "Core configuration",
      "Risk metrics",
      "Simulations",
      "Backtest",
      "Kupiec validation",
      "Christoffersen validation",
      "Benchmark timings"
    ),
    status = c(
      if (!is.null(artifact$cfg) && !is.null(artifact$sample_metadata)) "ok" else "missing",
      if (!is.null(artifact$model_metrics)) "ok" else "missing",
      if (!is.null(artifact$simulations)) "ok" else "missing",
      if (!is.null(artifact$backtest)) "ok" else "missing",
      if (!is.null(artifact$kupiec)) "ok" else "missing",
      if (!is.null(artifact$christoffersen)) "optional-missing" else "ok",
      if (!is.null(artifact$timings)) "ok" else "optional-missing"
    ),
    stringsAsFactors = FALSE
  )

  list(
    artifact = artifact,
    validation = validation,
    summary = summary,
    risk_table = risk_table,
    available_models = available_models,
    backtest = backtest,
    validation_table = validation_tbl,
    transition_counts = transition,
    timings = artifact$timings,
    health_checks = health_checks,
    interpretation_lines = build_interpretation_lines(list(
      risk_table = risk_table,
      backtest = backtest,
      validation = list(kupiec_p_value = summary$kupiec_pval)
    ))
  )
}
