safe_query <- function(expr, prefix = "", collapse = "|") {
  if (is.null(expr)) {
    return("")
  } else {
    return(paste0(prefix, glue::glue_collapse(expr, sep = collapse)))
  }
}
