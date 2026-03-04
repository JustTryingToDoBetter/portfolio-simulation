# Intent: time blocks and return both value + elapsed seconds

time_it <- function(label, expr) {

    t0 <- proc.time()[["elapsed"]]
    value <- force(expr)
    t1 <- proc.time()[["elapsed"]]
    elapsed <- t1 - t0
    list(label = label, value = value, seconds = elapsed)
}