# this function helps creating query parameters
safe_query <- function(expr, prefix = "", collapse = "|") {
  if (is.null(expr)) {
    return("")
  } else if (identical(expr, TRUE)) {
    return(paste0(prefix, "true"))
  } else if (identical(expr, FALSE)) {
    return(paste0(prefix, "false"))
  } else {
    return(paste0(prefix, glue::glue_collapse(expr, sep = collapse)))
  }
}


# experimental functions
tested_connect_version <- function() {
  current_connect_version
}

check_connect_version <- function(using_version, tested_version = tested_connect_version()) {
  
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
      "Most APIs should function as expected."
    ))
  )
  invisible()
}

connect_input <- function(connect) {
  if (R6::is.R6(connect)) {
    # is an R6 object... we presume the right type
    return(connect)
  } else if (is.list(connect) && c("host","api_key") %in% names(connect)) {
    return(Connect$new(host = connect[["host"]], api_key = connect[["api_key"]]))
  } else {
    stop("Input 'connect' is not an R6 object or a named list")
  }
}
