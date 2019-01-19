safequery <- function(expr, prefix = "") {
  if (is.null(expr)) {
    return("")
  } else {
    return(paste0(prefix, expr))
  }
}
