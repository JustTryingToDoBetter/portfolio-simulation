# Intent: compute log returns per ticker and return a wide matrix ready for simulation

suppressPackageStartupMessages({
    library(dplyr) # for data manipulation
    library(tidyr) # for pivoting data
})

## 
compute_log_returns <- function(prices_tbl) {
    prices_tbl %>% #
        group_by(ticker) %>% ## 
        arrange(date, .by_group = TRUE) %>%
        mutate(ret = log(price / dplyr::lag(price))) %>%
        ungroup() %>% 
        filter(!is.na(ret)) ## Remove the first row per ticker which will have NA return
  
}

returns_wide_matrix <- function(returns_tbl) { 
    wide <- returns_tbl %>%
        select(date, ticker, ret) %>%
        tidyr::pivot_wider(names_from = ticker, values_from = ret) %>%
        arrange(date)  ## Ensure rows are ordered by date for time series analysis
    
    mat <- as.matrix(wide %>% select(-date)) ## Convert to matrix format for simulation
    rownames(mat) <- as.character(wide$date) ## Set row names to dates for reference

      # Guardrails: remove any rows with missing returns (misaligned tickers)
    mat <- mat[stats::complete.cases(mat), , drop = FALSE]
    mat ## Return the wide matrix of log returns with dates as row names
}

