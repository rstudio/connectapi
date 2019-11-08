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

# because format(NULL, "%Y-%m") == "NULL"
safe_format <- function(expr, ...) {
  if (is.null(expr)) {
    return(NULL)
  } else {
    return(format(expr, ...))
  }
}

make_timestamp <- function(input) {
  safe_format(input, '%Y-%m-%dT%H:%M:%SZ')
}

generate_R6_print_output <- function() {
  con <- Connect$new(host = "test_host", api_key = "test_key")
  bnd <- Bundle$new(path = "/test/path")
  
  ex_content <- list(guid = "content-guid", title = "content-title", url = "http://content-url")
  cnt1 <- Content$new(connect = con, ex_content)
  
  ex_task <- list(task_id = "task-id")
  tsk1 <- Task$new(connect = con, content = ex_content, task = ex_task)
  
  ex_vanity <- list(path_prefix = "vanity-prefix")
  van1 <- Vanity$new(connect = con, content = ex_content, vanity = ex_vanity)
  
  obj_list <- list(con, bnd, cnt1, tsk1, van1)
  
  unlist(mapply(
    function(.x, .y) {c(
      "----------------------------",
      .y,
      "----------------------------",
      capture.output(print(.x))
      )},
    .x = obj_list,
    .y = lapply(obj_list, function(x){class(x)[[1]]}),
    SIMPLIFY = FALSE
  ))
}

validate_R6_class <- function(class, instance) {
  obj <- rlang::enquo(instance)
  if (!R6::is.R6(instance) | !inherits(instance, class)) {
    stop(paste(rlang::quo_text(obj), "must be an R6", class, "object"))
  }
  invisible(TRUE)
}

# set up test servers...
bootstrap_test_env <- function(clean = FALSE) {
  # find compose
  # this is b/c specifying an env requires an absolute path
  wh <- processx::process$new("which", "docker-compose", stdout = "|", stderr = "|")
  while (wh$is_alive()) Sys.sleep(0.05)
  stopifnot(wh$get_exit_status() == 0)
  compose_path <- wh$read_output_lines()
  
  # stop compose
  if (clean) {
    compose_down <- processx::process$new(
      compose_path,
      c("-f", system.file("test-connect.yml", package = "connectapi"), "down"),
      stdout = "|",
      stderr = "|"
    )
    while (compose_down$is_alive()) Sys.sleep(0.05)
    stopifnot(compose_down$get_exit_status() == 0)
  }
  
  # start compose
  compose <- processx::process$new(
    compose_path, 
    c("-f", system.file("test-connect.yml", package = "connectapi"), "up", "-d"),
    stdout = "|",
    stderr = "|",
    env = c(CONNECT_VERSION=current_connect_version)
    )
  while (compose$is_alive()) Sys.sleep(0.05)
  stopifnot(compose$get_exit_status() == 0)
  
  # get docker containers
  docker_ps <- processx::process$new("docker", "ps", stdout = "|", stderr = "|")
  while(docker_ps$is_alive()) Sys.sleep(0.05)
  stopifnot(docker_ps$get_exit_status() == 0)
  docker_ps_output <- docker_ps$read_output_lines()
  
  c1 <- docker_ps_output[grep("connectapi_connect_1", docker_ps_output)]
  c2 <- docker_ps_output[grep("connectapi_connect_2", docker_ps_output)]
  
  p1 <- substr(c1, regexpr("0\\.0\\.0\\.0:", c1)+8, regexpr("->3939", c1)-1)
  p2 <- substr(c2, regexpr("0\\.0\\.0\\.0:", c2)+8, regexpr("->3939", c2)-1)
  
  a1 <- create_first_admin(
    glue::glue("http://localhost:{p1}"),
    "admin", "admin0", "admin@example.com"
    )
  a2 <- create_first_admin(
    glue::glue("http://localhost:{p2}"),
    "admin", "admin0", "admin@example.com"
    )
  
  return(
    list(
      connect1 = a1,
      connect2 = a2
    )
  )
}

# set up the first admin
create_first_admin <- function(
  url, 
  user, password, email, 
  keyname = "first-key",
  provider = "password"
  ) {
  check_connect_license(url)
  
  client <- HackyConnect$new(host = url, api_key = NULL)
  
  if (provider == "password") {
    tryCatch({
      first_admin <- client$POST(
        body = list(
          username = user,
          password = password,
          email = email
        ),
        path = "v1/users"
      )
    },
    error = function(e) {
      message(glue::glue("Error creating first admin: {e}"))
    }
    )
  }
  
  login <- client$login(
    user = user,
    password = password
  )
  
  user_info <- client$me()
  
  api_key <- client$POST(
    path = "keys",
    body = list(name = keyname)
  )
  
  return(
    connect(url, api_key$key)
  )
}

HackyConnect <- R6::R6Class(
  "HackyConnect",
  inherit = Connect,
  public = list(
    xsrf = NULL,
    login = function(user, password) {
      res <- httr::POST(
        glue::glue("{self$host}/__login__"),
        body = list(username = user, password = password),
        encode = "json"
      )
      
      res_cookies <- httr::cookies(res)
      self$xsrf <- res_cookies[res_cookies$name == "RSC-XSRF", "value"]
      
      httr::content(res, as = 'parsed')
    },
    
    add_auth = function() {
      httr::add_headers('X-RSC-XSRF'=self$xsrf)
    }
  )
)

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
  rlang::scoped_options(
    .frame = frame,
    connectapi_disable_experimental_warnings = TRUE
    )
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

tested_connect_version <- function() {
  current_connect_version
}

check_connect_license <- function(url) {
  res <- httr::GET(glue::glue("{url}/__ping__"))
  if (res$status_code == 402) {
    stop(glue::glue("ERROR: The Connect server's license is expired ({url})"))
  }
  invisible(res)
}

check_connect_version <- function(using_version, tested_version = tested_connect_version()) {
  
  comp <- compareVersion(tested_version, using_version)
  
  msg <- switch(
    as.character(comp),
    "0" = NULL,
    "1" = warn_once(glue::glue(
      "You are using an older version of RStudio Connect",
      "({using_version}) than was tested ({tested_version}).",
      "Some APIs may not function as expected."
    ), id = "old-connect"),
    "-1" = warn_once(glue::glue(
      "You are using a newer version of RStudio Connect",
      "({using_version}) than was tested ({tested_version}).",
      "Most APIs should function as expected."
    ), id = "new-connect")
  )
  invisible()
}
