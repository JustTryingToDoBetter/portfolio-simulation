# Intent: production-ready visualizations for portfolio risk simulation artifacts.
#
# All functions accept an artifact list (as returned by run_pipeline()) and an
# optional `save_to` filepath. They return a ggplot2 object invisibly, or stop
# with a clear message when required data is absent.
#
# Convention: losses are positive numbers (VaR/CVaR are positive thresholds).
# Simulation vectors store returns (negative = loss); we negate them for plots.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Verify ggplot2 is installed before any plot function runs.
.require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop(
      "ggplot2 is required for plotting. ",
      "Install it with install.packages('ggplot2').",
      call. = FALSE
    )
}

# Save a ggplot to disk; create parent directories automatically.
.save_plot <- function(p, path, width = 8, height = 5, dpi = 150) {
  if (is.null(path)) return(invisible(p))
  dir.create(dirname(normalizePath(path, mustWork = FALSE)),
             recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  dpi = dpi, bg = "white")
  message("Plot saved: ", path)
  invisible(p)
}

# Stop with a tidy message when a required artifact field is NULL.
.require_field <- function(x, field_name) {
  if (is.null(x))
    stop(
      "Artifact is missing required field: `", field_name, "`. ",
      "Re-run the pipeline to generate an up-to-date artifact.",
      call. = FALSE
    )
  x
}

# Null-safe accessor: returns `default` when `x` is NULL.
.or_default <- function(x, default) if (!is.null(x)) x else default

# Human-readable label for a model key.
.model_label <- function(key) {
  labels <- c(
    mvn              = "MVN",
    bootstrap        = "Bootstrap",
    stress_bootstrap = "Stress Bootstrap",
    garch            = "GARCH"
  )
  if (key %in% names(labels)) labels[[key]] else key
}

# Resolve the alpha level from the artifact, with a safe fallback.
.resolve_alpha <- function(artifact) {
  a <- artifact$cfg$alpha
  if (is.null(a)) a <- artifact$sample_metadata$alpha
  if (is.null(a)) a <- 0.95
  a
}

# ---------------------------------------------------------------------------
# plot_loss_distribution
#
# Density of simulated portfolio losses for a chosen model, overlaid with
# VaR and CVaR vertical lines. Models: "mvn", "bootstrap", "stress_bootstrap",
# or "garch" (only when the GARCH path was enabled and succeeded).
#
# Arguments:
#   artifact  - run artifact from run_pipeline()
#   model     - simulation model key (character)
#   save_to   - file path for PNG output, or NULL to skip saving
#
# Returns the ggplot2 object invisibly.
# ---------------------------------------------------------------------------
plot_loss_distribution <- function(artifact, model = "bootstrap", show_cvar = TRUE, save_to = NULL) {
  .require_ggplot2()
  stopifnot(is.list(artifact))

  valid_models <- c("mvn", "bootstrap", "stress_bootstrap", "garch")
  if (!is.character(model) || length(model) != 1L || !(model %in% valid_models))
    stop("`model` must be one of: ", paste(valid_models, collapse = ", "),
         ".", call. = FALSE)

  sims_list <- .require_field(artifact$simulations, "simulations")
  sims      <- sims_list[[model]]
  if (is.null(sims))
    stop("No simulation samples stored for model '", model, "'. ",
         "Ensure the model was enabled during the pipeline run.", call. = FALSE)
  if (!is.numeric(sims) || length(sims) < 10)
    stop("Simulation samples for model '", model,
         "' must be a numeric vector with at least 10 values.", call. = FALSE)

  metrics_list <- .require_field(artifact$model_metrics, "model_metrics")
  metrics      <- metrics_list[[model]]
  if (is.null(metrics))
    stop("No risk metrics stored for model '", model, "'.", call. = FALSE)

  alpha    <- .resolve_alpha(artifact)
  label    <- .model_label(model)
  losses   <- -sims
  var_val  <- metrics$VaR
  cvar_val <- metrics$CVaR

  # Clip display range to remove extreme outliers while keeping shape visible.
  x_lo <- as.numeric(stats::quantile(losses, 0.001))
  x_hi <- as.numeric(stats::quantile(losses, 0.999))

  df <- data.frame(loss = losses)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = loss)) +
    ggplot2::geom_density(
      fill = "#4e79a7", alpha = 0.40,
      colour = "#2e5f8a", linewidth = 0.6
    ) +
    ggplot2::geom_vline(
      xintercept = var_val,
      colour = "#e15759", linewidth = 0.9, linetype = "dashed"
    ) +
    ggplot2::annotate(
      "text", x = var_val, y = Inf,
      label  = paste0("VaR = ", round(var_val, 4)),
      hjust  = -0.08, vjust = 1.6,
      colour = "#e15759", size = 3.2
    ) +
    ggplot2::coord_cartesian(xlim = c(x_lo, x_hi)) +
    ggplot2::labs(
      title    = paste0("Simulated Loss Distribution (", label, ")"),
      subtitle = paste0(
        "alpha = ", alpha,
        "  |  VaR = ",  round(var_val,  4),
        "  |  CVaR = ", round(cvar_val, 4)
      ),
      x = "Portfolio Loss  (negative return)",
      y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  if (isTRUE(show_cvar)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = cvar_val,
        colour = "#f28e2b", linewidth = 0.9, linetype = "dotdash"
      ) +
      ggplot2::annotate(
        "text", x = cvar_val, y = Inf,
        label  = paste0("CVaR = ", round(cvar_val, 4)),
        hjust  = -0.08, vjust = 3.2,
        colour = "#f28e2b", size = 3.2
      )
  }

  .save_plot(p, save_to)
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_backtest_var
#
# Realized portfolio returns over time with the rolling VaR threshold
# overlaid as a dashed line. Breach observations are highlighted with
# cross markers. Use this to assess whether exceedances cluster in time.
#
# The VaR threshold is rendered as its negative (a loss boundary in return
# space) so it sits below zero alongside the realized returns.
# ---------------------------------------------------------------------------
plot_backtest_var <- function(artifact, save_to = NULL) {
  .require_ggplot2()
  stopifnot(is.list(artifact))

  bt       <- .require_field(artifact$backtest, "backtest")
  port_ret <- .require_field(bt$port_ret, "backtest$port_ret")
  vars     <- .require_field(bt$vars,     "backtest$vars")
  breaches <- .require_field(bt$breaches, "backtest$breaches")

  if (length(port_ret) != length(vars) || length(port_ret) != length(breaches))
    stop("backtest fields `port_ret`, `vars`, and `breaches` must have equal length.",
         call. = FALSE)

  # Use stored dates when available; fall back to integer index.
  x_vals <- if (!is.null(bt$dates) && length(bt$dates) == length(port_ret)) {
    bt$dates
  } else {
    seq_along(port_ret)
  }

  df <- data.frame(
    x        = x_vals,
    ret      = port_ret,
    var_line = -vars,   # rendered as a negative return to align with ret axis
    breach   = as.logical(breaches)
  )
  df_breach <- df[df$breach, , drop = FALSE]

  model_key <- .or_default(bt$model, .or_default(artifact$cfg$model, "unknown"))
  n_breach  <- sum(breaches)
  n_total   <- length(breaches)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_line(
      ggplot2::aes(y = ret),
      colour = "#76b7b2", linewidth = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = var_line),
      colour = "#e15759", linewidth = 0.7, linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = df_breach,
      ggplot2::aes(x = x, y = ret),
      colour = "#e15759", size = 2.2, shape = 4, stroke = 1.1
    ) +
    ggplot2::labs(
      title    = "Rolling VaR Backtest",
      subtitle = paste0(
        "Model: ", .model_label(model_key),
        "  |  Breaches: ", n_breach, " / ", n_total,
        "  (", round(100 * n_breach / max(n_total, 1), 2), "%)"
      ),
      x = NULL,
      y = "Portfolio Return  /  -VaR Threshold"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  .save_plot(p, save_to)
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_breach_timeline
#
# Binary breach indicator plotted as a column series plus a rug/point layer
# for each breach. Clustering is immediately visible because consecutive
# breach bars appear adjacent. Complements Kupiec and Christoffersen tests.
# ---------------------------------------------------------------------------
plot_breach_timeline <- function(artifact, save_to = NULL) {
  .require_ggplot2()
  stopifnot(is.list(artifact))

  bt       <- .require_field(artifact$backtest, "backtest")
  breaches <- .require_field(bt$breaches, "backtest$breaches")

  x_vals <- if (!is.null(bt$dates) && length(bt$dates) == length(breaches)) {
    bt$dates
  } else {
    seq_along(breaches)
  }

  df        <- data.frame(x = x_vals, breach = as.integer(breaches))
  df_breach <- df[df$breach == 1L, , drop = FALSE]

  exp_rate <- .or_default(bt$expected_breach_rate,
                          1 - .resolve_alpha(artifact))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = breach)) +
    ggplot2::geom_col(
      fill = "#bab0ac", width = 0.7, alpha = 0.45
    ) +
    ggplot2::geom_rug(
      data    = df_breach,
      mapping = ggplot2::aes(x = x),
      sides   = "b", colour = "#e15759", linewidth = 0.6, alpha = 0.85
    ) +
    ggplot2::geom_point(
      data    = df_breach,
      mapping = ggplot2::aes(x = x, y = breach),
      colour  = "#e15759", size = 2, shape = 17
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0L, 1L),
      labels = c("No breach", "Breach")
    ) +
    ggplot2::labs(
      title    = "VaR Breach Timeline",
      subtitle = paste0(
        sum(breaches), " breaches  |  ",
        "observed = ", round(100 * mean(breaches), 2), "%",
        "  |  expected = ", round(100 * exp_rate, 2), "%"
      ),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  .save_plot(p, save_to)
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_model_risk_comparison
#
# Side-by-side bar chart of VaR and CVaR across all available models.
# GARCH bars are shown only when the GARCH path ran successfully.
# ---------------------------------------------------------------------------
plot_model_risk_comparison <- function(artifact, save_to = NULL) {
  .require_ggplot2()
  stopifnot(is.list(artifact))

  metrics_list <- .require_field(artifact$model_metrics, "model_metrics")

  rows <- list()
  for (key in c("mvn", "bootstrap", "stress_bootstrap", "garch")) {
    m <- metrics_list[[key]]
    if (is.null(m)) next
    rows[[length(rows) + 1L]] <- data.frame(
      model  = .model_label(key),
      metric = c("VaR", "CVaR"),
      value  = c(m$VaR, m$CVaR),
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L)
    stop(
      "No model metrics found in artifact. ",
      "Cannot create comparison plot.", call. = FALSE
    )

  df        <- do.call(rbind, rows)
  df$model  <- factor(df$model, levels = unique(df$model))
  df$metric <- factor(df$metric, levels = c("VaR", "CVaR"))

  alpha <- .resolve_alpha(artifact)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = model, y = value, fill = metric)) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(0.72), width = 0.62
    ) +
    ggplot2::scale_fill_manual(
      values = c(VaR = "#4e79a7", CVaR = "#f28e2b")
    ) +
    ggplot2::labs(
      title    = "Risk Model Comparison: VaR and CVaR",
      subtitle = paste0("alpha = ", alpha),
      x        = "Model",
      y        = "Loss  (portfolio return units)",
      fill     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  .save_plot(p, save_to)
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_benchmark_timings
#
# Horizontal bar chart of per-stage runtimes from the timing summary.
# Stages are ordered from fastest to slowest (longest bar at top) to make
# bottlenecks obvious. Uses the `timings` data.frame from the artifact.
# ---------------------------------------------------------------------------
plot_benchmark_timings <- function(artifact, save_to = NULL) {
  .require_ggplot2()
  stopifnot(is.list(artifact))

  timings <- .require_field(artifact$timings, "timings")

  if (!is.data.frame(timings) || !all(c("step", "seconds") %in% names(timings)))
    stop(
      "`artifact$timings` must be a data.frame with columns `step` and `seconds`.",
      call. = FALSE
    )
  if (nrow(timings) == 0L)
    stop("`artifact$timings` contains no rows.", call. = FALSE)

  # Order ascending so the slowest stage lands at the top of the horizontal chart.
  df      <- timings[order(timings$seconds), ]
  df$step <- factor(df$step, levels = df$step)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = seconds, y = step)) +
    ggplot2::geom_col(fill = "#59a14f", alpha = 0.85, width = 0.62) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(seconds, 2)),
      hjust = -0.12, size = 3.2, colour = "grey30"
    ) +
    ggplot2::expand_limits(x = max(df$seconds) * 1.18) +
    ggplot2::labs(
      title = "Pipeline Stage Runtimes",
      x     = "Elapsed seconds",
      y     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  .save_plot(p, save_to)
  invisible(p)
}
