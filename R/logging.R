# Intent: minimal structured logging for CLI runs

log_info <- function(..., .sep= ""){
    msg <- paste0(...)
    cat(sprintf("[%s] INFO %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg), sep = .sep)
}

log_warn <- function(..., .sep = ""){
    msg <- paste0(...)
    cat(sprintf("[%s] WARN %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg), sep = .sep)
}