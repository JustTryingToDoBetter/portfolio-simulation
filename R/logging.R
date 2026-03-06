# Intent: minimal structured logging for CLI runs

log_message <- function(level, ..., .sep = "") {
    msg <- paste0(...)
    ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    cat(sprintf("ts=%s level=%s msg=\"%s\"\n", ts, level, msg), sep = .sep)
}

log_info <- function(..., .sep = "") {
    log_message("INFO", ..., .sep = .sep)
}

log_warn <- function(..., .sep = "") {
    log_message("WARN", ..., .sep = .sep)
}

log_error <- function(..., .sep = "") {
    log_message("ERROR", ..., .sep = .sep)
}