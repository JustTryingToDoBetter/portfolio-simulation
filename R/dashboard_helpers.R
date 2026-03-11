# Intent: helper utilities for the Shiny dashboard UI and interpretation text.

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

fmt_num <- function(x, digits = 4, na = "N/A") {
  if (is.null(x) || length(x) == 0 || !is.finite(x)) return(na)
  formatC(round(as.numeric(x), digits), format = "f", digits = digits)
}

fmt_pct <- function(x, digits = 2, na = "N/A") {
  if (is.null(x) || length(x) == 0 || !is.finite(x)) return(na)
  paste0(formatC(round(100 * as.numeric(x), digits), format = "f", digits = digits), "%")
}

fmt_pval <- function(x, digits = 4, na = "N/A") {
  if (is.null(x) || length(x) == 0 || !is.finite(x)) return(na)
  formatC(round(as.numeric(x), digits), format = "f", digits = digits)
}

model_title <- function(key) {
  titles <- c(
    mvn = "MVN",
    bootstrap = "Bootstrap",
    stress_bootstrap = "Stress Bootstrap",
    garch = "GARCH"
  )
  if (is.null(key) || !(key %in% names(titles))) return(as.character(key))
  titles[[key]]
}

# ---------------------------------------------------------------------------
# Small UI building blocks
# ---------------------------------------------------------------------------

metric_card <- function(title, value, subtitle = NULL) {
  shiny::tags$div(
    class = "metric-card",
    shiny::tags$div(class = "metric-title", title),
    shiny::tags$div(class = "metric-value", value),
    shiny::tags$div(class = "metric-subtitle", subtitle %||% "")
  )
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---------------------------------------------------------------------------
# Interpretation helper
# ---------------------------------------------------------------------------

build_interpretation_lines <- function(vm) {
  lines <- character(0)

  risk_tbl <- vm$risk_table
  if (is.data.frame(risk_tbl) && nrow(risk_tbl) > 0) {
    v_boot <- risk_tbl$VaR[risk_tbl$model_key == "bootstrap"]
    v_mvn  <- risk_tbl$VaR[risk_tbl$model_key == "mvn"]
    if (length(v_boot) == 1 && length(v_mvn) == 1 && is.finite(v_boot) && is.finite(v_mvn)) {
      if (v_boot > v_mvn) {
        lines <- c(lines, "Bootstrap VaR is higher than MVN VaR, suggesting heavier effective tails under resampling.")
      } else if (v_boot < v_mvn) {
        lines <- c(lines, "MVN VaR is higher than bootstrap VaR in this run, suggesting stronger parametric tail pressure.")
      } else {
        lines <- c(lines, "Bootstrap and MVN VaR are very close in this run.")
      }
    }

    v_stress <- risk_tbl$VaR[risk_tbl$model_key == "stress_bootstrap"]
    if (length(v_boot) == 1 && length(v_stress) == 1 && is.finite(v_boot) && is.finite(v_stress)) {
      lift <- (v_stress - v_boot) / max(v_boot, 1e-12)
      lines <- c(lines, paste0("Stress VaR is ", fmt_pct(lift, digits = 1), " relative to baseline bootstrap VaR."))
    }
  }

  bt <- vm$backtest
  if (!is.null(bt$breach_rate) && !is.null(bt$expected_rate) &&
      is.finite(bt$breach_rate) && is.finite(bt$expected_rate)) {
    ratio <- bt$breach_rate / max(bt$expected_rate, 1e-12)
    if (ratio > 1.5) {
      lines <- c(lines, "Observed breach frequency is materially above expectation, indicating potential risk underestimation.")
    } else if (ratio < 0.5) {
      lines <- c(lines, "Observed breach frequency is materially below expectation, indicating potential conservatism.")
    } else {
      lines <- c(lines, "Observed breach frequency is close to expectation.")
    }
  }

  kup <- vm$validation$kupiec_p_value
  if (is.finite(kup)) {
    if (kup < 0.05) {
      lines <- c(lines, "Kupiec test rejects at the 5% level, so unconditional coverage is questionable.")
    } else {
      lines <- c(lines, "Kupiec test does not reject at the 5% level.")
    }
  }

  unique(lines)
}
