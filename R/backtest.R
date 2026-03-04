# Intent: rolling VaR backtest using historical window and breach-rate output

rolling_var_backtest <- function(returns, window = 252, alpha = 0.95) {
	stopifnot(is.numeric(returns), length(returns) > window)
	stopifnot(is.numeric(window), window >= 20)
	stopifnot(is.numeric(alpha), alpha > 0, alpha < 1)

	n <- length(returns)
	eval_idx <- seq.int(window + 1, n)

	var_forecast <- vapply(eval_idx, function(i) {
		hist_window <- returns[(i - window):(i - 1)]
		as.numeric(stats::quantile(-hist_window, probs = alpha, type = 7, names = FALSE))
	}, numeric(1))

	realized_returns <- returns[eval_idx]
	realized_losses <- -realized_returns
	breach <- realized_losses > var_forecast

	data.frame(
		index = eval_idx,
		realized_return = realized_returns,
		realized_loss = realized_losses,
		VaR = var_forecast,
		breach = breach
	)
}

breach_rate <- function(backtest_tbl) {
	stopifnot(is.data.frame(backtest_tbl), "breach" %in% names(backtest_tbl))
	mean(backtest_tbl$breach)
}

backtest_summary <- function(backtest_tbl, alpha) {
	rate <- breach_rate(backtest_tbl)
	expected <- 1 - alpha

	list(
		observations = nrow(backtest_tbl),
		alpha = alpha,
		expected_breach_rate = expected,
		observed_breach_rate = rate,
		breaches = sum(backtest_tbl$breach)
	)
}
