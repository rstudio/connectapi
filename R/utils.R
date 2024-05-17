# Helpers to make it easier to identify where we're calling public APIs and not
v1_url <- function(...) {
  paste("v1", ..., sep = "/")
}

unversioned_url <- function(...) {
  paste(..., sep = "/")
}

generate_R6_print_output <- function() {
  con <- Connect$new(server = "http://test_host", api_key = "test_key")
  bnd <- Bundle$new(path = "/test/path")

  ex_content <- list(guid = "content-guid", title = "content-title", url = "http://content-url")
  cnt1 <- Content$new(connect = con, ex_content)

  ex_task <- list(task_id = "task-id")
  tsk1 <- ContentTask$new(connect = con, content = ex_content, task = ex_task)

  ex_vanity <- list(path_prefix = "vanity-prefix")
  van1 <- Vanity$new(connect = con, content = ex_content, vanity = ex_vanity)

  obj_list <- list(con, bnd, cnt1, tsk1, van1)

  unlist(mapply(
    function(.x, .y) {
      c(
        "----------------------------",
        .y,
        "----------------------------",
        capture.output(print(.x))
      )
    },
    .x = obj_list,
    .y = lapply(obj_list, function(x) {
      class(x)[[1]]
    }),
    SIMPLIFY = FALSE
  ))
}

is_R6_class <- function(instance, class) {
  return(R6::is.R6(instance) && inherits(instance, class))
}

validate_R6_class <- function(instance, class) {
  obj <- rlang::enquo(instance)
  if (!R6::is.R6(instance) | !inherits(instance, class)) {
    stop(paste(rlang::quo_text(obj), "must be an R6", glue::glue_collapse(class, sep = " or "), "object"))
  }
  invisible(TRUE)
}

# super useful examples
# https://github.com/tidyverse/tibble/blob/master/R/compat-lifecycle.R
warn_experimental <- function(name) {
  if (rlang::is_true(rlang::peek_option("connectapi_disable_experimental_warnings"))) {
    return(invisible(NULL))
  }
  warn_once(
    msg = glue::glue("The `{name}` function is experimental and subject to change without warning in a future release"),
    id = paste0(name, "-experimental")
  )
}

scoped_experimental_silence <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    connectapi_disable_experimental_warnings = TRUE
  )
}

warn_dire <- function(name) {
  if (rlang::is_true(rlang::peek_option("connectapi_disable_dire_warnings"))) {
    return(invisible(NULL))
  }
  warn_once(
    msg = glue::glue("DO NOT USE IN PRODUCTION - The {name} function is for internal testing purposes only"),
    id = paste0(name, "-dire")
  )
}

scoped_dire_silence <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    connectapi_disable_dire_warnings = TRUE
  )
}

warn_clear <- function(id) {
  rm(list = id, envir = warn_env)
}

warn_once <- function(msg, id = msg) {
  if (rlang::is_true(rlang::peek_option("connectapi_disable_warnings"))) {
    return(invisible(NULL))
  }

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

  rlang::signal(msg, .subclass = "warning")
}
warn_env <- new.env(parent = emptyenv())

check_connect_license <- function(url) {
  if (is_R6_class(url, "Connect")) {
    res <- url$GET_RESULT_URL(url$server)
  } else {
    res <- httr::GET(glue::glue("{url}/__ping__"))
  }
  if (res$status_code == 402) {
    stop(glue::glue("ERROR: The Connect server's license is expired ({url})"))
  }
  invisible(res)
}

simplify_version <- function(version) {
  sub("([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", version)
}

safe_server_settings <- function(client) {
  srv <- tryCatch(
    {
      client$server_settings()
    },
    error = function(e) {
      message(
        glue::glue("Problem talking to Posit Connect at {client$server}/__api__/server_settings")
      )
      stop(e)
    }
  )
  return(srv)
}

safe_server_version <- function(client) {
  version <- safe_server_settings(client)$version
  if (is.null(version) || nchar(version) == 0) {
    message("Version information is not exposed by this Posit Connect instance.")
    # Return 0 so this will always show up as "too old"
    version <- "0"
  }
  version
}

error_if_less_than <- function(client, tested_version) {
  server_version <- safe_server_version(client)
  comp <- compare_connect_version(server_version, tested_version)
  if (comp < 0) {
    msg <- paste0(
      "ERROR: This API requires Posit Connect version ", tested_version,
      " but you are using", server_version, ". Please use a previous version",
      " of the `connectapi` package, upgrade Posit Connect, or review the API ",
      "documentation corresponding to your version."
    )
    stop(msg)
  }
  invisible()
}

compare_connect_version <- function(using_version, tested_version) {
  compareVersion(simplify_version(using_version), simplify_version(tested_version))
}

check_connect_version <- function(using_version, minimum_tested_version = "1.8.8.2") {
  comp <- compare_connect_version(using_version, minimum_tested_version)
  if (comp < 0) {
    warn_once(glue::glue(
      "You are using an older version of Posit Connect ",
      "({using_version}) than is tested ({minimum_tested_version}). ",
      "Some APIs may not function as expected."
    ), id = "old-connect")
  }
  invisible()
}
