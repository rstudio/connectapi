#' Content
#'
#' An R6 class that represents content
#'
#' @family R6 classes
#' @export
Content <- R6::R6Class(
  "Content",
  public = list(
    #' @field connect An R6 Connect object
    connect = NULL,
    #' @field content The content details from RStudio Connect
    content = NULL,

    initialize = function(connect, content) {
      validate_R6_class(connect, "Connect")
      self$connect <- connect
      # TODO: need to check that content has
      # at least guid, url, title to be functional
      self$content <- content
    },
    get_connect = function() {
      self$connect
    },
    get_content = function() {
      self$content
    },
    get_content_remote = function() {
      new_content_details <- self$get_connect()$content(self$get_content()$guid)
      self$content <- new_content_details
      self$get_content()
    },
    get_bundles = function(page_number = 1) {
      url <- glue::glue("v1/experimental/content/{self$get_content()$guid}/bundles?page_number={page_number}")
      self$get_connect()$GET(url)
    },
    internal_content = function() {
      url <- glue::glue("applications/{self$get_content()$guid}")
      self$get_connect()$GET(url)
    },
    update = function(...) {
      con <- self$get_connect()
      error_if_less_than(con, "1.8.6")
      params <- rlang::list2(...)
      url <- glue::glue("v1/content/{self$get_content()$guid}")
      res <- con$PATCH(
        url,
        params
      )
      return(self)
    },
    runas = function(run_as, run_as_current_user = FALSE) {
      warn_experimental("content_runas")
      params <- list(
        run_as = run_as,
        run_as_current_user = run_as_current_user
      )
      url <- glue::glue("applications/{self$get_content()$guid}/runas")
      res <- self$get_connect()$POST(
        url,
        params
      )
      return(res)
    },
    get_url = function() {
      self$get_content()$url
    },
    get_dashboard_url = function(pane = "") {
      dashboard_url_chr(self$connect$server, self$content$guid, pane = pane)
    },
    get_jobs = function() {
      lifecycle::deprecate_warn("0.1.0.9005", what = "get_jobs()", with = "jobs()")
      self$jobs()
    },
    get_job = function(key) {
      lifecycle::deprecate_warn("0.1.0.9005", "get_job()", "job()")
      self$job(key)
    },
    jobs = function() {
      warn_experimental("jobs")
      url <- glue::glue("applications/{self$get_content()$guid}/jobs")
      res <- self$get_connect()$GET(url)
    },
    job = function(key) {
      warn_experimental("job")
      url <- glue::glue("applications/{self$get_content()$guid}/job/{key}")
      res <- self$get_connect()$GET(url)

      content_guid <- self$get_content()$guid
      purrr::map(
        list(res),
        ~ purrr::list_modify(.x, app_guid = content_guid)
      )[[1]]
    },
    variants = function() {
      warn_experimental("variants")
      url <- glue::glue("applications/{self$get_content()$guid}/variants")
      self$get_connect()$GET(url)
    },
    tag_set = function(id) {
      warn_experimental("tag_set")
      url <- glue::glue("applications/{self$get_content()$guid}/tags")
      self$get_connect()$POST(
        url,
        body = list(
          id = id
        )
      )
    },
    tag_delete = function(id) {
      # note that deleting the parent tag deletes all children
      warn_experimental("tag_delete")
      url <- glue::glue("applications/{self$get_content()$guid}/tags/{id}")
      invisible(self$get_connect()$DELETE(url))
    },
    tags = function() {
      warn_experimental("tags")
      url <- glue::glue("applications/{self$get_content()$guid}/tags")
      self$get_connect()$GET(url)
    },
    environment = function() {
      warn_experimental("environment")
      url <- glue::glue("applications/{self$get_content()$guid}/environment")
      res <- self$get_connect()$GET(url)
      # update values to be NA, which is how we preserve them
      res$values <- purrr::map(
        res$values,
        ~ NA_character_
      )
      return(res)
    },
    environment_set = function(..., .version = 0) {
      warn_experimental("environment_set")
      url <- glue::glue("applications/{self$get_content()$guid}/environment")
      # post with
      # key = NA to retain
      # post without a variable/key to remove
      # bump version number each time
      vals <- rlang::list2(...)

      # TODO: evaluate whether we should be coercing to character or erroring
      vals <- purrr::map(vals, as.character)
      body <- list(
        values = vals,
        version = .version,
        app_guid = self$get_content()$guid
      )
      self$get_connect()$POST(
        path = url,
        body = body
      )
      invisible()
    },
    deploy = function() {
      self$get_connect()$POST(
        glue::glue("v1/experimental/content/{self$get_content()$guid}/deploy"),
        body = "{}"
      )
    },
    repo_enable = function(enabled = TRUE) {
      warn_experimental("repo_enable")
      self$get_connect()$PUT(
        glue::glue("applications/{self$get_content()$guid}/repo"),
        body = list(
          enabled = enabled
        )
      )
    },
    repo_set = function(repository, branch, subdirectory) {
      warn_experimental("repo_set")
      self$get_connect()$POST(
        glue::glue("applications/{self$get_content()$guid}/repo"),
        body = list(
          repository = repository,
          branch = branch,
          subdirectory = subdirectory
        )
      )
    },
    print = function(...) {
      cat("RStudio Connect Content: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  Content URL: ", dashboard_url_chr(self$get_connect()$server, self$get_content()$guid), "\n", sep = "")
      cat("  Content Title: ", self$get_content()$title, "\n", sep = "")
      cat("\n")
      cat('content_item(client, guid = "', self$get_content()$guid, '")', "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

#' Environment
#'
#' An R6 class that represents a Content's Environment Variables
#'
Environment <- R6::R6Class(
  "Environment",
  inherit = Content,
  public = list(
    env_version = NULL,
    env_raw = NULL,
    env_vars = NULL,
    initialize = function(connect, content) {
      super$initialize(connect = connect, content = content)
      self$env_refresh()
    },
    env_refresh = function() {
      # mutates the existing instance, so future
      # references have the right version
      self$env_raw <- self$environment()
      self$env_version <- self$env_raw$version
      self$env_vars <- self$env_raw$values
      return(self)
    },
    print = function(...) {
      super$print(...)
      cat("Environment Variables:\n")
      cat("  vctrs::vec_c(\n")
      purrr::imap(self$env_vars, ~ cat(paste0("    ", .y, " = ", .x, ",\n")))
      cat("  )\n")
      cat("\n")
      invisible(self)
    }
  )
)

# does it make more sense to automatically "get the latest"
# or to force the user to do that?
#' Manage Environment Variables
#'
#' \lifecycle{experimental} Manage Environment Variables for a piece of content.
#'
#' `get_environment()` returns an Environment object for use with "setter" methods
#'
#' `set_environment_new()` sets new environment values (either creating new
#' values or updating existing)
#'
#' `set_environment_remove()` removes existing environment variables
#'
#' @param content An R6 Content object as returned by `content_item()`
#' @param env An R6 Environment object as returned by `get_environment()`
#' @param ... name = value pairs of environment variable names and values
#'
#' @family content functions
#' @export
#'
#' @rdname environment
get_environment <- function(content) {
  warn_experimental("get_environment")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  content_data <- content$get_content_remote()
  connect_client <- content$get_connect()
  return(Environment$new(connect_client, content_data))
}

#' @rdname environment
#' @export
set_environment_new <- function(env, ...) {
  warn_experimental("set_environment")
  scoped_experimental_silence()
  validate_R6_class(env, "Environment")

  # update existing env vars with new ones
  new_env_vars <- purrr::list_modify(env$env_vars, ...)

  env$environment_set(!!!new_env_vars, .version = env$env_version)

  env$env_refresh()
}

#' @rdname environment
#' @export
set_environment_remove <- function(env, ...) {
  warn_experimental("set_environment")
  scoped_experimental_silence()
  validate_R6_class(env, "Environment")

  to_remove <- rlang::enexprs(...)
  existing_vars <- env$env_vars
  new_env_vars <- existing_vars[!names(existing_vars) %in% c(names(to_remove), as.character(to_remove))]

  env$environment_set(!!!new_env_vars, .version = env$env_version)

  env$env_refresh()
}

#' Get Content Item
#'
#' Returns a single content item based on guid
#'
#' @param connect A Connect object
#' @param guid The GUID for the content item to be retrieved
#'
#' @return A Content object for use with other content endpoints
#'
#' @family content functions
#' @export
content_item <- function(connect, guid) {
  # TODO : think about how to handle if GUID does not exist
  validate_R6_class(connect, "Connect")

  res <- connect$get_connect()$content(guid)

  Content$new(connect = connect, content = res)
}

#' Get Content Title
#'
#' Return content title for a piece of content. If the content
#' is missing (deleted) or not visible, then returns the `default`
#'
#' @param connect A Connect object
#' @param guid The GUID for the content item to be retrieved
#' @param default The default value returned for missing or not visible content
#'
#' @return character. The title of the requested content
#'
#' @family content functions
#' @export
content_title <- function(connect, guid, default = "Unknown Content") {
  validate_R6_class(connect, "Connect")

  content_title <- tryCatch({
      res <- suppressMessages(connect$get_connect()$content(guid))
      # TODO: What about length 0?
      if (is.null(res$title)) {
        return(default)
      }
      res$title
    },
    error = function(e) {
      return(default)
    }
  )

  return(content_title)
}

content_ensure <- function(connect, name = uuid::UUIDgenerate(), title = name, guid = NULL, ..., .permitted = c("new", "existing")) {
  if (!is.null(guid)) {
    # guid-based deployment
    # just in case we get a 404 back...
    content <- tryCatch(
      suppressMessages(connect$content(guid = guid)),
      error = function(e) {
        return(NULL)
      })
    if (is.null(content)) {
      if (!"new" %in% .permitted) {
        stop(glue::glue("guid {guid} was not found on {connect$server}"))
      }
      warning(glue::glue(
        "guid {guid} was not found on {connect$server}.",
        "Creating new content with name {name}"
      ))
      content <- connect$content_create(
        name = name,
        title = title,
        ...
      )
    }
  } else {
    # name-based deployment
    content <- connect$get_apps(list(name = name))
    if (length(content) > 1) {
      stop(glue::glue(
        "Found {length(to_content)} content items ",
        "matching {name} on {connect$server}",
        ", content must have a unique name."
      ))
    } else if (length(content) == 0) {
      if (!"new" %in% .permitted) {
        stop(glue::glue("Content with name {name} was not found on {connect$server}"))
      }
      message(glue::glue(
        "Creating NEW content {content$guid} ",
        "with name {name} on {connect$server}"
      ))
      # create app
      content <- connect$content_create(
        name = name,
        title = title,
        ...
      )
    } else {
      content <- content[[1]]
      if (!"existing" %in% .permitted) {
        stop(glue::glue("Content with name {name} already exists at {dashboard_url_chr(connect$server, content$guid)}"))
      }
      message(glue::glue(
        "Found EXISTING content {content$guid} with ",
        "name {name} on {connect$server}"
      ))
      # TODO: update values...? need a PUT endpoint
    }
  }
  return(content)
}

#' Get Jobs
#'
#' \lifecycle{experimental} Retrieve details about jobs associated with a `content_item`.
#' "Jobs" in RStudio Connect are content executions
#'
#' @param content A Content object, as returned by `content_item()`
#' @param key The key for a job
#'
#' @rdname jobs
#' @family content functions
#' @export
get_jobs <- function(content) {
  warn_experimental("get_jobs")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")

  jobs <- content$jobs()
  parse_connectapi_typed(jobs, !!!connectapi_ptypes$jobs)
}

# TODO: Need to test `logged_error` on a real error
#' @rdname jobs
#' @export
get_job <- function(content, key) {
  warn_experimental("get_job")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")

  job <- content$job(key = key)
  # protect against becoming a list...
  job$stdout <- strsplit(job$stdout, "\n")[[1]]
  job$stderr <- strsplit(job$stderr, "\n")[[1]]
  # a bit of an abuse
  # since stdout / stderr / logged_error are here now...
  parse_connectapi_typed(list(job), !!!connectapi_ptypes$job)
}

#' Set RunAs User
#'
#' \lifecycle{experimental} Set the `RunAs` user for a piece of content.
#' The `run_as_current_user` flag only does anything if:
#'
#' - PAM is the authentication method
#' - `Applications.RunAsCurrentUser` is enabled on the server
#'
#' Also worth noting that the `run_as` user must exist on the RStudio Connect
#' server and have appropriate group memberships, or you will get a `400: Bad Request`.
#' Set to `NULL` to use the default RunAs user / unset any current configuration.
#'
#' To "read" the current RunAs user, use the `Content` object or `get_content()` function.
#'
#' @param content an R6 Content item
#' @param run_as The RunAs user to use for this content
#' @param run_as_current_user Whether to run this content as the viewer of the application
#'
#' @return a Content object, updated with new details
#'
#' @seealso connectapi::get_content
#'
#' @family content functions
#' @export
set_run_as <- function(content, run_as, run_as_current_user = FALSE) {
  warn_experimental("set_run_as")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")

  content$runas(run_as = run_as, run_as_current_user = run_as_current_user)

  invisible(content$get_content_remote())

  return(content)
}


delete_content <- function(content) {
  validate_R6_class(content, "Content")
  # TODO
}


#' Verify Content Name
#'
#' Ensures that a content name fits the specifications / requirements of RStudio
#' Connect. Throws an error if content name is invalid. Content names (as of the
#' time of writing) must be between 3 and 64 alphanumeric characters, dashes,
#' and underscores
#'
#' @param name The proposed content name
#'
#' @return The name (or an error if invalid)
#'
#' @seealso connectapi::create_random_name
#' @family content functions
#' @export
verify_content_name <- function(name) {
  if (grepl("[^\\-\\_a-zA-Z0-9]", name, perl = TRUE) || nchar(name) < 3 || nchar(name) > 64 ) {
    stop(glue::glue("ERROR: content name '{name}' must be between 3 and 64 alphanumeric characters, dashes, and underscores"))
  }
  return(name)
}

#' Create Random Name
#'
#' Creates a random name from the LETTERS dataset
#'
#' @param length Optional. The length of the random name. Defaults to 25
#'
#' @return The randomly generated name
#'
#' @seealso connectapi::verify_content_name
#' @family content functions
#' @export
create_random_name <- function(length = 25) {
  tolower(paste(sample(LETTERS, length, replace = TRUE), collapse = ""))
}

#' Get Bundles
#'
#' Lists bundles for a content item
#'
#' @param content A R6 Content item, as returned by `content_item()`
#' @param limit Optional. Limit on number of bundles to return. Default Infinity.
#' @param connect A R6 Connect item, as returned by `connect()`
#' @param bundle_id A specific bundle ID for a content item
#'
#' @rdname get_bundles
#' @family content functions
#' @export
get_bundles <- function(content, limit = Inf) {
  validate_R6_class(content, "Content")
  bundles <- page_offset(content$get_connect(), content$get_bundles(), limit = limit)

  parse_connectapi_typed(bundles, !!!connectapi_ptypes$bundles)
}

#' @rdname get_bundles
#' @family content functions
#' @export
delete_bundle <- function(connect, bundle_id) {
  validate_R6_class(connect, "Connect")
  message(glue::glue("Deleting bundle {bundle_id}"))
  connect$bundle_delete(bundle_id)
  return(connect)
}
