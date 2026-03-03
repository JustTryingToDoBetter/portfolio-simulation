# Intent: fetch adjusted close prices from Yahoo Finance and return a clean tibble

suppressPackageStartupMessages({
    library(quantmod)
    library(dplyr)
    library(tidyr)
    library(tibble)
})

## 
fetch_prices_yahoo <- function(tickers, from = "2018-01-01") {
    stopifnot(length(tickers) >= 1)

    env <- new.env(parent = emptyenv())

    getSymbols(
        Symbols = tickers, 
        src = "yahoo",
        from= from,
        env = env,
        auto.assign = TRUE, 
        warnings = FALSE
    )

# Extract Adjusted close for each ticker, align by date
price_list <- lapply(tickers, function(sym) {
    xt <- env[[sym]]
    adj <- Ad(xt)
    tibble(
        date = as.Date(index(adj)),
        ticker = sym,
        price = as.numeric(adj)
    )
})

bind_rows(price_list) %>%
    arrange(ticker,date) %>%
    filter(!is.na(price))
}