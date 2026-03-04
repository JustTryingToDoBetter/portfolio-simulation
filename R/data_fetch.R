# Intent: fetch adjusted close prices from Yahoo Finance and return a clean tibble

suppressPackageStartupMessages({
    library(quantmod)
    library(dplyr)
    library(tidyr)
    library(tibble)
})

cache_file_is_fresh <- function(cache_file, max_age_hours) {
    if (!file.exists(cache_file)) {
        return(FALSE)
    }

    file_age_hours <- as.numeric(difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours"))
    is.finite(file_age_hours) && file_age_hours <= max_age_hours
}

fetch_prices_yahoo <- function(
    tickers,
    from = "2018-01-01",
    cache_dir = "data/cache",
    cache_max_age_hours = 24,
    refresh_cache = FALSE
) {
    stopifnot(length(tickers) >= 1)
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE) ## Ensure cache directory exists
    
    key <- paste(sort(tickers), collapse = "_") ## Create a unique key for the combination of tickers
    cache_file <- file.path(cache_dir, sprintf("prices_%s_from_%s.rds", key, from))
    ## Return cache if available and still fresh
    if (!refresh_cache && cache_file_is_fresh(cache_file, cache_max_age_hours)) {
        prices <- readRDS(cache_file)
        return(prices)
    }
    
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
            date = as.Date(xts::index(adj)),
            ticker = sym,
            price = as.numeric(adj)
        )
    })

    prices <- bind_rows(price_list) %>%
        arrange(.data$ticker, .data$date) %>%
        filter(!is.na(.data$price))

    saveRDS(prices, cache_file)
    prices
}