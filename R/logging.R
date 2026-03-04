# Intent: minimal structured logging for CLI runs

log_message <- function(level, ..., .sep = "") {
    msg <- paste0(...)
    cat(sprintf("[%s] %s %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg), sep = .sep)
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