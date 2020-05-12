# TODO: A nicer way to execute these system commands...
# - debug output... better error handling... etc.

# set up test servers...
find_compose <- function() {
  wh <- processx::process$new("which", "docker-compose", stdout = "|", stderr = "|")
  while (wh$is_alive()) Sys.sleep(0.05)
  stopifnot(wh$get_exit_status() == 0)
  wh$read_output_lines()
}

clean_test_env <- function(compose_file_path = system.file("ci/test-connect.yml", package = "connectapi")) {
  compose_path <- find_compose()
  cat_line("docker-compose: cleaning...")
  compose_down <- processx::process$new(
    compose_path,
    c("-f", compose_file_path, "down"),
    stdout = "|",
    stderr = "|"
  )
  while (compose_down$is_alive()) Sys.sleep(0.05)
  stopifnot(compose_down$get_exit_status() == 0)
  cat_line("docker-compose: clean!")
  invisible()
}

determine_license_env <- function(license) {
  if (fs::file_exists(license) && fs::path_ext(license) == "lic") {
    cat_line("determine_license: looks like a license file")
    return(list(
      type = "file",
      cmd_params = c("-v", paste0(license, ":/etc/rstudio-connect/license.lic")),
      env_params = c(RSC_LICENSE_FILE = license)
    ))
  } else {
    cat_line("determine_license: looks like a license key")
    return(list(
      type = "key",
      cmd_params = c(),
      env_params = c(
        RSC_LICENSE = license
      )
    ))
  }
}

compose_start <- function(connect_license = Sys.getenv("RSC_LICENSE"), clean = TRUE) {
  warn_dire("compose_Start")
  scoped_dire_silence()

  stopifnot(nchar(connect_license) > 0)

  # find compose
  # this is b/c specifying an env requires an absolute path
  compose_path <- find_compose()

  license_details <- determine_license_env(connect_license)
  compose_file <- switch(
    license_details$type,
    "file" = "ci/test-connect-lic.yml",
    "ci/test-connect.yml"
  )

  compose_file_path <- system.file(compose_file, package = "connectapi")

  # stop compose
  if (clean) {
    clean_test_env(compose_file_path)
  }

  # start compose
  cat_line("docker-compose: starting...")
  compose <- processx::process$new(
    compose_path,
    c("-f", compose_file_path, "up", "-d"),
    stdout = "|",
    stderr = "|",
    env = c(
      RSC_VERSION = current_connect_version,
      license_details$env_params
    )
  )
  while (compose$is_alive()) Sys.sleep(0.05)
  if (compose$get_exit_status() != 0) {
    stop(compose$read_all_error_lines())
  }
  cat_line("docker-compose: started!")
  invisible()
}

compose_find_hosts <- function(prefix) {
  warn_dire("compose_find")
  scoped_dire_silence()

  # get docker containers
  cat_line("docker: getting list of containers...")
  docker_ps <- processx::process$new("docker", "ps", stdout = "|", stderr = "|")
  while (docker_ps$is_alive()) Sys.sleep(0.05)
  stopifnot(docker_ps$get_exit_status() == 0)
  docker_ps_output <- docker_ps$read_output_lines()
  cat_line("docker: got containers")

  c1 <- docker_ps_output[grep(glue::glue("{prefix}_1"), docker_ps_output)]
  c2 <- docker_ps_output[grep(glue::glue("{prefix}_2"), docker_ps_output)]

  p1 <- substr(c1, regexpr("0\\.0\\.0\\.0:", c1) + 8, regexpr("->3939", c1) - 1)
  p2 <- substr(c2, regexpr("0\\.0\\.0\\.0:", c2) + 8, regexpr("->3939", c2) - 1)
  cat_line(glue::glue("docker: got ports {p1} and {p2}"))

  # TODO: make this silly sleep more savvy
  cat_line("connect: sleeping - waiting for connect to start")
  Sys.sleep(10)

  return(list(
    glue::glue("http://localhost:{p1}"),
    glue::glue("http://localhost:{p2}")
  ))
}


update_renviron_creds <- function(host, api_key, prefix, .file = ".Renviron") {
  cat_line(glue::glue("connect: writing values for {prefix} to {.file}"))
  curr_environ <- tryCatch(readLines(.file), error = function(e) {
    print(e)
    return(character())
  })

  curr_environ <- curr_environ[!grepl(glue::glue("^{prefix}_SERVER="), curr_environ)]
  curr_environ <- curr_environ[!grepl(glue::glue("^{prefix}_API_KEY="), curr_environ)]
  output_environ <- glue::glue(
    paste(curr_environ, collapse = "\n"),
    "{prefix}_SERVER={host}",
    "{prefix}_API_KEY={api_key}",
    .sep = "\n"
  )
  if (!fs::file_exists(.file)) fs::file_touch(.file)
  writeLines(output_environ, .file)
  invisible()
}

build_test_env <- function(
                           connect_license = Sys.getenv("RSC_LICENSE"),
                           clean = TRUE,
                           username = "admin",
                           password = "admin0") {
  warn_dire("build_test_env")
  scoped_dire_silence()

  compose_start(connect_license = connect_license, clean = clean)

  hosts <- compose_find_hosts(prefix = "ci_connect")

  cat_line("connect: creating first admin...")
  a1 <- create_first_admin(
    hosts[[1]],
    "admin", "admin0", "admin@example.com"
  )
  a2 <- create_first_admin(
    hosts[[2]],
    "admin", "admin0", "admin@example.com"
  )

  update_renviron_creds(a1$host, a1$api_key, "TEST_1")
  update_renviron_creds(a2$host, a2$api_key, "TEST_2")

  cat_line("connect: done")

  cat_line()
  cat_line()
  cat_line("NOTE: be sure to reload .Renviron before running tests!")
  cat_line('  readRenviron(".Renviron")')
  cat_line("  devtools::test()")
  cat_line()
  cat_line()

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
  warn_dire("create_first_admin")
  check_connect_license(url)

  client <- HackyConnect$new(host = url, api_key = NULL)
  
  if (provider == "password") {
    tryCatch(
      {
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
  } else if (provider %in% c("ldap", "pam")) {
    # can just log in using user / password
  } else {
    stop("Unsupported authentication provider")
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
      warn_dire("HackyConnect")
      res <- httr::POST(
        glue::glue("{self$host}/__login__"),
        body = list(username = user, password = password),
        encode = "json"
      )

      res_cookies <- httr::cookies(res)
      self$xsrf <- res_cookies[res_cookies$name == "RSC-XSRF", "value"]

      httr::content(res, as = "parsed")
    },

    add_auth = function() {
      httr::add_headers("X-RSC-XSRF" = self$xsrf)
    }
  )
)
