source("R/logging.R")
source("R/benchmark.R")
source("R/globals.R")
source("R/returns.R")
source("R/risk_metrics.R")
source("R/sim_mc.R")
source("R/validation.R")
source("R/plots.R")
source("R/reporting.R")
source("R/dashboard_helpers.R")
source("R/dashboard_data.R")

library(shiny)

artifact_path_default <- "outputs/latest_run.rds"

app_css <- "
body {
  background: radial-gradient(circle at top left, #1d2433 0%, #121722 35%, #0f131b 100%);
  color: #e6ebf4;
}
.navbar-default {
  background-color: rgba(17, 21, 31, 0.95);
  border-color: #242a38;
}
.navbar-default .navbar-brand,
.navbar-default .navbar-nav > li > a {
  color: #d9e0ec !important;
}
.panel {
  background-color: rgba(20, 25, 37, 0.92);
  border: 1px solid #2a3244;
  border-radius: 10px;
}
.panel-heading {
  background-color: rgba(26, 32, 47, 0.95) !important;
  border-bottom: 1px solid #313a50 !important;
  color: #d9e0ec !important;
}
.well {
  background-color: rgba(20, 25, 37, 0.92);
  border: 1px solid #2a3244;
}
.table {
  color: #d9e0ec;
}
.metric-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
  gap: 12px;
}
.metric-card {
  background: linear-gradient(180deg, rgba(37, 45, 64, 0.95) 0%, rgba(25, 31, 45, 0.95) 100%);
  border: 1px solid #39435a;
  border-radius: 10px;
  padding: 12px 14px;
  min-height: 96px;
}
.metric-title {
  font-size: 12px;
  text-transform: uppercase;
  color: #a9b5cc;
  letter-spacing: 0.04em;
}
.metric-value {
  font-size: 24px;
  font-weight: 700;
  margin-top: 4px;
  color: #f0f4fb;
}
.metric-subtitle {
  font-size: 11px;
  color: #98a6bf;
  margin-top: 6px;
}
.interpretation-box {
  border-left: 3px solid #4e79a7;
  padding: 10px 12px;
  background-color: rgba(27, 34, 48, 0.85);
  border-radius: 6px;
  margin-top: 10px;
}
.status-ok { color: #7fd18f; font-weight: 600; }
.status-optional-missing { color: #f1c66a; font-weight: 600; }
.status-missing { color: #f07e7e; font-weight: 600; }
.help-note { color: #a9b5cc; font-size: 12px; }
"

ui <- navbarPage(
  title = "Risk Analytics Dashboard",
  header = tags$head(tags$style(HTML(app_css))),

  tabPanel(
    title = "Overview",
    fluidRow(
      column(
        9,
        tags$h3("Run Summary"),
        uiOutput("overview_cards"),
        tags$div(class = "help-note", "VaR and CVaR are shown as positive loss thresholds under the project loss convention."),
        tags$h4("Interpretation"),
        uiOutput("interpretation_block")
      ),
      column(
        3,
        tags$h3("Artifact"),
        actionButton("reload_artifact", "Reload latest artifact", class = "btn-primary btn-sm"),
        br(), br(),
        tableOutput("overview_meta")
      )
    )
  ),

  tabPanel(
    title = "Risk Models",
    fluidRow(
      column(
        3,
        wellPanel(
          uiOutput("risk_model_ui"),
          checkboxInput("show_cvar", "Show CVaR marker", value = TRUE)
        ),
        tableOutput("risk_table")
      ),
      column(
        9,
        plotOutput("risk_compare_plot", height = "330px"),
        plotOutput("loss_dist_plot", height = "330px")
      )
    )
  ),

  tabPanel(
    title = "Backtesting",
    fluidRow(
      column(
        8,
        plotOutput("backtest_plot", height = "330px"),
        plotOutput("breach_plot", height = "250px")
      ),
      column(
        4,
        tableOutput("validation_table"),
        tags$h4("Transition Counts"),
        tableOutput("transition_table")
      )
    )
  ),

  tabPanel(
    title = "Diagnostics",
    fluidRow(
      column(
        8,
        plotOutput("timings_plot", height = "330px"),
        tableOutput("timings_table")
      ),
      column(
        4,
        tags$h4("Run Metadata"),
        tableOutput("diagnostics_meta"),
        tags$h4("Artifact Health"),
        uiOutput("health_table")
      )
    )
  )
)

server <- function(input, output, session) {
  artifact_rv <- reactiveVal(NULL)
  vm_rv <- reactiveVal(NULL)
  load_error <- reactiveVal(NULL)

  load_artifact <- function(path = artifact_path_default) {
    tryCatch({
      artifact <- load_run_artifact(path)
      vm <- build_dashboard_view_model(artifact)
      artifact_rv(artifact)
      vm_rv(vm)
      load_error(NULL)
    }, error = function(e) {
      artifact_rv(NULL)
      vm_rv(NULL)
      load_error(conditionMessage(e))
    })
  }

  load_artifact()

  observeEvent(input$reload_artifact, {
    load_artifact()
  })

  ensure_vm <- reactive({
    if (!is.null(load_error())) {
      validate(need(FALSE, paste0(
        "Unable to load artifact. ", load_error(),
        " Try running main.R, then click Reload latest artifact."
      )))
    }
    req(vm_rv())
    vm_rv()
  })

  output$overview_cards <- renderUI({
    vm <- ensure_vm()
    s <- vm$summary
    rt <- vm$risk_table

    mvn_var <- rt$VaR[rt$model_key == "mvn"]
    boot_var <- rt$VaR[rt$model_key == "bootstrap"]
    stress_var <- rt$VaR[rt$model_key == "stress_bootstrap"]
    boot_cvar <- rt$CVaR[rt$model_key == "bootstrap"]

    cards <- list(
      metric_card("MVN VaR", fmt_num(mvn_var), paste0("alpha = ", s$alpha)),
      metric_card("Bootstrap VaR", fmt_num(boot_var), paste0("alpha = ", s$alpha)),
      metric_card("Stress VaR", fmt_num(stress_var), "Scaled bootstrap stress"),
      metric_card("Bootstrap CVaR", fmt_num(boot_cvar), "Tail average loss"),
      metric_card("Breach Rate", fmt_pct(vm$backtest$breach_rate), paste0("expected ", fmt_pct(vm$backtest$expected_rate))),
      metric_card("Kupiec p-value", fmt_pval(s$kupiec_pval), "Unconditional coverage"),
      metric_card("Christoffersen p-value", fmt_pval(s$cc_pval), if (is.na(s$cc_pval)) "Not available" else "Conditional coverage")
    )

    tags$div(class = "metric-grid", cards)
  })

  output$interpretation_block <- renderUI({
    vm <- ensure_vm()
    lines <- vm$interpretation_lines
    if (length(lines) == 0) {
      return(tags$div(class = "interpretation-box", "Interpretation is not available for the current artifact."))
    }
    tags$div(
      class = "interpretation-box",
      tags$ul(lapply(lines, tags$li))
    )
  })

  output$overview_meta <- renderTable({
    vm <- ensure_vm()
    s <- vm$summary
    data.frame(
      Field = c("Tickers", "Weights", "Alpha", "Sample start", "Sample end", "Observations", "Backtest model", "Run timestamp", "Seed"),
      Value = c(
        paste(s$tickers, collapse = ", "),
        paste(paste0(s$tickers, ": ", fmt_num(s$weights, 3)), collapse = ", "),
        s$alpha,
        s$start_date,
        s$end_date,
        s$n_obs,
        model_title(s$backtest_model),
        s$run_timestamp,
        s$seed
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, spacing = "xs")

  output$risk_model_ui <- renderUI({
    vm <- ensure_vm()
    choices <- stats::setNames(vm$available_models, vapply(vm$available_models, model_title, character(1)))
    if (length(choices) == 0) {
      return(tags$div("No model data available."))
    }
    selectInput("selected_model", "Loss distribution model", choices = choices, selected = choices[[1]])
  })

  output$risk_table <- renderTable({
    vm <- ensure_vm()
    if (nrow(vm$risk_table) == 0) return(data.frame(Note = "No risk metrics available."))
    out <- vm$risk_table
    out$model_key <- NULL
    out$VaR <- fmt_num(out$VaR)
    out$CVaR <- fmt_num(out$CVaR)
    out
  }, striped = TRUE, spacing = "xs")

  output$risk_compare_plot <- renderPlot({
    vm <- ensure_vm()
    tryCatch(
      plot_model_risk_comparison(vm$artifact),
      error = function(e) {
        validate(need(FALSE, paste("Model comparison plot unavailable:", conditionMessage(e))))
      }
    )
  })

  output$loss_dist_plot <- renderPlot({
    vm <- ensure_vm()
    req(input$selected_model)
    tryCatch(
      plot_loss_distribution(vm$artifact, model = input$selected_model, show_cvar = isTRUE(input$show_cvar)),
      error = function(e) {
        validate(need(FALSE, paste("Loss distribution unavailable:", conditionMessage(e))))
      }
    )
  })

  output$backtest_plot <- renderPlot({
    vm <- ensure_vm()
    tryCatch(
      plot_backtest_var(vm$artifact),
      error = function(e) {
        validate(need(FALSE, paste("Backtest plot unavailable:", conditionMessage(e))))
      }
    )
  })

  output$breach_plot <- renderPlot({
    vm <- ensure_vm()
    tryCatch(
      plot_breach_timeline(vm$artifact),
      error = function(e) {
        validate(need(FALSE, paste("Breach timeline unavailable:", conditionMessage(e))))
      }
    )
  })

  output$validation_table <- renderTable({
    vm <- ensure_vm()
    vm$validation_table
  }, striped = TRUE, spacing = "xs")

  output$transition_table <- renderTable({
    vm <- ensure_vm()
    tc <- vm$transition_counts
    if (is.null(tc)) {
      return(data.frame(Note = "Christoffersen transition counts not available."))
    }
    data.frame(
      Transition = c("n00", "n01", "n10", "n11"),
      Count = c(tc$n00, tc$n01, tc$n10, tc$n11),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, spacing = "xs")

  output$timings_plot <- renderPlot({
    vm <- ensure_vm()
    if (is.null(vm$timings)) {
      validate(need(FALSE, "Benchmark timings are not available for this artifact."))
    }
    tryCatch(
      plot_benchmark_timings(vm$artifact),
      error = function(e) {
        validate(need(FALSE, paste("Timing plot unavailable:", conditionMessage(e))))
      }
    )
  })

  output$timings_table <- renderTable({
    vm <- ensure_vm()
    if (is.null(vm$timings)) {
      return(data.frame(Note = "Benchmark timings are not available."))
    }
    out <- vm$timings
    out$seconds <- fmt_num(out$seconds, digits = 3)
    out
  }, striped = TRUE, spacing = "xs")

  output$diagnostics_meta <- renderTable({
    vm <- ensure_vm()
    s <- vm$summary
    data.frame(
      Field = c("Run timestamp", "Seed", "Git commit", "Backtest model", "Total runtime"),
      Value = c(s$run_timestamp, s$seed, s$git_commit, model_title(s$backtest_model), paste0(fmt_num(s$total_runtime_s, 2), " s")),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, spacing = "xs")

  output$health_table <- renderUI({
    vm <- ensure_vm()
    rows <- lapply(seq_len(nrow(vm$health_checks)), function(i) {
      status <- vm$health_checks$status[[i]]
      klass <- switch(
        status,
        ok = "status-ok",
        `optional-missing` = "status-optional-missing",
        "status-missing"
      )
      tags$tr(
        tags$td(vm$health_checks$section[[i]]),
        tags$td(class = klass, status)
      )
    })

    warning_block <- NULL
    if (length(vm$validation$warnings) > 0) {
      warning_block <- tags$div(
        class = "interpretation-box",
        tags$strong("Warnings"),
        tags$ul(lapply(vm$validation$warnings, tags$li))
      )
    }

    tagList(
      tags$table(class = "table table-condensed",
                 tags$thead(tags$tr(tags$th("Section"), tags$th("Status"))),
                 tags$tbody(rows)),
      warning_block
    )
  })
}

shinyApp(ui = ui, server = server)
