# Intent: compute VaR and CVaR for a vector of PnL (or returns)

var_cvar <- function(pnl, alpha = 0.95) {
    stopifnot(is.numeric(pnl), length(pnl) > 10, alpha > 0, alpha < 1) ## Basic checks on inputs

    # Convention: VaR is a loss threshold (positive number). Use -PnL if PnL is profits.

    losses <- -pnl ## Convert PnL to losses for VaR/CVaR calculation
    var <- as.numeric(stats::quantile(losses, probs = alpha,type = 7, names = FALSE)) ## VaR at the alpha level
    cvar <- mean(losses[losses >= var]) ## CVaR is the average loss in the tail beyond VaR

    list(VaR = var, CVar = cvar) ## Return a list with both metrics
}