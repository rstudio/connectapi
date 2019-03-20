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

validate_R6_class <- function(class, instance) {
  obj <- rlang::enquo(instance)
  if (!R6::is.R6(instance) | !inherits(instance, class)) {
    stop(paste(rlang::quo_text(obj), "must be an R6", class, "object"))
  }
  invisible(TRUE)
}

warn_experimental <- function(name) {
  warn_once(
    msg = glue::glue("The {name} function is experimental and subject to change in a future release"),
    id = paste0(name, "-experimental")
  )
}

warn_once <- function(msg, id = msg) {
  if (rlang::env_has(warn_env, id)) {
    return(invisible(NULL))
  }
  
  has_color <- function() rlang::is_installed("crayon") && crayon::has_color()
  silver <- function(x) if (has_color()) crayon::silver(x) else x
  
  msg <- paste0(
    msg,
    "\n",
    silver("This warning is displayed once per session.")
  )
  
  rlang::env_poke(warn_env, id, TRUE)
  
  warning(msg)
}
warn_env <- new.env(parent = emptyenv())

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
