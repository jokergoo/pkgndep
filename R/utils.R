stop_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}

warning_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    warning(x, call. = FALSE)
}

message_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x)
}
