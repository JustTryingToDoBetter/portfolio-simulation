# Intent: rolling VaR backtesting for portfolio models with compatibility wrappers

source("R/sim_mc.R")

backtest_var <- function(returns_mat, weights, window = 252, alpha = 0.95, model = "bootstrap", n_sims = 100000) {
	if (!is.matrix(returns_mat) || nrow(returns_mat) <= window) {
		stop("`returns_mat` must be a matrix with more rows than `window`.", call. = FALSE)
	}
	if (!is.numeric(weights) || length(weights) != ncol(returns_mat)) {
		stop("`weights` must be numeric and match number of return columns.", call. = FALSE)
	}
	if (abs(sum(weights) - 1) > 1e-6) {
		stop("`weights` must sum to 1.", call. = FALSE)
	}
	if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
		stop("`alpha` must be in (0,1).", call. = FALSE)
	}
	if (!is.numeric(window) || window < 20) {
		stop("`window` must be >= 20.", call. = FALSE)
	}
	if (!is.numeric(n_sims) || n_sims < 1000) {
		stop("`n_sims` must be >= 1000 for stable rolling VaR.", call. = FALSE)
	}

	model <- match.arg(model, choices = c("bootstrap", "mvn"))
	clean_mat <- returns_mat[stats::complete.cases(returns_mat), , drop = FALSE]
	if (nrow(clean_mat) <= window) {
		stop("Not enough complete rows after NA filtering for rolling backtest.", call. = FALSE)
	}

	n <- nrow(clean_mat)
	date_index <- rownames(clean_mat)
	eval_idx <- seq.int(window + 1, n)
	vars <- numeric(length(eval_idx))
	port_ret <- numeric(length(eval_idx))

	for (k in seq_along(eval_idx)) {
		i <- eval_idx[k]
		window_mat <- clean_mat[(i - window):(i - 1), , drop = FALSE]

		sim_port <- if (identical(model, "bootstrap")) {
			simulate_portfolio_bootstrap(window_mat, weights, n_sims = n_sims, vol_scale = 1.0)
		} else {
			simulate_portfolio_mvn(window_mat, weights, n_sims = n_sims)
		}

		vars[k] <- as.numeric(stats::quantile(-sim_port, probs = alpha, type = 7, names = FALSE))
		port_ret[k] <- sum(clean_mat[i, ] * weights)
	}

	breaches <- (-port_ret) > vars
	eval_dates <- NULL
	if (!is.null(date_index)) {
		eval_dates <- date_index[eval_idx]
		parsed_dates <- suppressWarnings(as.Date(eval_dates))
		if (all(!is.na(parsed_dates))) {
			eval_dates <- parsed_dates
		}
	}

	list(
		breach_rate = mean(breaches),
		breach_count = sum(breaches),
		total_tested = length(breaches),
		vars = vars,
		var_series = vars,
		port_ret = port_ret,
		realized_return = port_ret,
		realized_loss = -port_ret,
		breaches = breaches,
		breach_indicator = as.integer(breaches),
		evaluation_index = eval_idx,
		dates = eval_dates,
		model = model,
		alpha = alpha,
		window = as.integer(window),
		n_sims = as.integer(n_sims),
		expected_breach_rate = 1 - alpha
	)
}

rolling_var_backtest <- function(returns, window = 252, alpha = 0.95) {
	if (!is.numeric(returns) || length(returns) <= window) {
		stop("`returns` must be a numeric vector with length > window.", call. = FALSE)
	}
	mat <- matrix(returns, ncol = 1)
	bt <- backtest_var(
		returns_mat = mat,
		weights = 1,
		window = window,
		alpha = alpha,
		model = "bootstrap",
		n_sims = 20000
	)

	eval_idx <- seq.int(window + 1, length(returns))
	data.frame(
		index = eval_idx,
		realized_return = bt$port_ret,
		realized_loss = -bt$port_ret,
		VaR = bt$vars,
		breach = bt$breaches
	)
}

breach_rate <- function(backtest_tbl) {
	if (!is.data.frame(backtest_tbl) || !("breach" %in% names(backtest_tbl))) {
		stop("`backtest_tbl` must be a data.frame containing a `breach` column.", call. = FALSE)
	}
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
