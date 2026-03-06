source("R/data_fetch.R")
source("R/returns.R")
source("R/sim_mc.R")
source("R/risk_metrics.R")

library(shiny)

ui <- fluidPage(
    titlePanel("Portfolio Risk (MVP)"),
    fluidRow(
        column(4, textInput("tickers", "Tickers (comma separated)", "AAPL,MSFT,GOOGL,AMZN")),
        column(4, textInput("weights", "Weights (comma separated)", "0.25,0.25,0.25,0.25")),
        column(2, numericInput("alpha", "Alpha", value = 0.95, min = 0.5, max = 0.999, step = 0.01)),
        column(2, selectInput("model", "Model", choices = c("bootstrap", "mvn"), selected = "bootstrap"))
    ),
    fluidRow(
        column(2, numericInput("vol_scale", "Vol Scale", value = 1.0, min = 0.1, max = 5.0, step = 0.05)),
        column(2, actionButton("run", "Run"))
    ),
    tableOutput("metrics")
)

server <- function(input, output, session) {
    result <- eventReactive(input$run, {
        tickers <- trimws(unlist(strsplit(input$tickers, ",")))
        weights <- as.numeric(trimws(unlist(strsplit(input$weights, ","))))

        validate(need(length(tickers) > 0, "Provide at least one ticker."))
        validate(need(length(tickers) == length(weights), "Tickers/weights length mismatch."))
        validate(need(abs(sum(weights) - 1) < 1e-6, "Weights must sum to 1."))

        prices <- fetch_prices_yahoo_cached(tickers = tickers, from = "2019-01-01", cache_dir = "data/cache")
        mat <- returns_wide_matrix(compute_log_returns(prices))

        sim <- if (identical(input$model, "mvn")) {
            simulate_portfolio_mvn(mat, weights = weights, n_sims = 20000)
        } else {
            simulate_portfolio_bootstrap(mat, weights = weights, n_sims = 20000, vol_scale = input$vol_scale)
        }

        var_cvar(sim, alpha = input$alpha)
    })

    output$metrics <- renderTable({
        req(result())
        data.frame(
            Metric = c("VaR", "CVaR"),
            Value = c(result()$VaR, result()$CVaR)
        )
    }, digits = 6)
}

shinyApp(ui = ui, server = server)
