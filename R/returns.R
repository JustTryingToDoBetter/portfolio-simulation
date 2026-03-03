# Intent: compute log returns and reshape into a clean wide matrix for simulation
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

compute_log_returns <- function(prices_tbl) {
  prices_tbl %>%
    dplyr::group_by(ticker) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(ret = log(price / dplyr::lag(price))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(ret))
}

returns_wide_matrix <- function(returns_tbl) {
  wide <- returns_tbl %>%
    dplyr::select(date, ticker, ret) %>%
    tidyr::pivot_wider(names_from = ticker, values_from = ret) %>%
    dplyr::arrange(date)

  mat <- as.matrix(wide %>% dplyr::select(-date))
  rownames(mat) <- as.character(wide$date)

  # Guardrail: drop misaligned rows (e.g., missing returns for a ticker on a given date)
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  mat
}