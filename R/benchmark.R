# Intent: time blocks and return both value + elapsed seconds

time_it <- function(label, expr) {

    t0 <- proc.time()[["elapsed"]]
    value <- force(expr)
    t1 <- proc.time()[["elapsed"]]
    elapsed <- t1 - t0
    list(label = label, value = value, seconds = elapsed)
}

summarize_benchmarks <- function(bench_results) {
    if (length(bench_results) == 0) {
        return(data.frame(step = character(0), seconds = numeric(0)))
    }

    data.frame(
        step = vapply(bench_results, function(x) x$label, character(1)),
        seconds = vapply(bench_results, function(x) as.numeric(x$seconds), numeric(1))
    )
}