safe_query <- function(expr, prefix = "", collapse = "|") {
  if (is.null(expr)) {
    return("")
  } else {
    return(paste0(prefix, glue::glue_collapse(expr, sep = collapse)))
  }
}

check_connect_version <- function(connect) {
  settings <- connect$get_server_settings()
  using_version <- settings[["version"]]
  
  comp <- compareVersion(tested_version, using_version)
  
  msg <- switch(
    as.character(comp),
    "0" = NULL,
    "1" = warning(glue::glue(
      "You are using an older version of RStudio Connect",
      "({using_version}) than was tested ({tested_version}).",
      "Some APIs may not function as expected."
    )),
    "-1" = warning(glue::glue(
      "You are using a newer version of RStudio Connect",
      "({using_version}) than was tested ({tested_version}).",
      "Some APIs may not function as expected."
    ))
  )
  invisible()
}

tested_version <- "1.7.0-11"
