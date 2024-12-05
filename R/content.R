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
    #' @field content The content details from Posit Connect
    content = NULL,

    #' @description Initialize this content.
    #' @param connect The `Connect` instance.
    #' @param content The content data.
    initialize = function(connect, content) {
      validate_R6_class(connect, "Connect")
      self$connect <- connect
      # TODO: need to check that content has
      # at least guid, url, title to be functional
      self$content <- content
    },
    #' @description Returns the `Connect` instance.
    get_connect = function() {
      self$connect
    },
    #' @description Returns the underlying content data.
    get_content = function() {
      self$content
    },
    #' @description Obtain the content data from the Connect server.
    get_content_remote = function() {
      new_content_details <- self$get_connect()$content(self$get_content()$guid)
      self$content <- new_content_details
      self$get_content()
    },
    #' @description Return the set of content bundles.
    get_bundles = function() {
      url <- v1_url("content", self$get_content()$guid, "bundles")
      self$get_connect()$GET(url)
    },
    #' @description Download the source archive for a content bundle.
    #' @param bundle_id The bundle identifer.
    #' @param filename Where to write the result.
    #' @param overwrite Overwrite an existing filename.
    bundle_download = function(
      bundle_id,
      filename = tempfile(pattern = "bundle", fileext = ".tar.gz"),
      overwrite = FALSE
    ) {
      url <- v1_url("content", self$get_content()$guid, "bundles", bundle_id, "download")
      self$get_connect()$GET(url, httr::write_disk(filename, overwrite = overwrite), parser = "raw")
      return(filename)
    },
    #' @description Delete a content bundle.
    #' @param bundle_id The bundle identifer.
    bundle_delete = function(bundle_id) {
      url <- v1_url("content", self$get_content()$guid, "bundles", bundle_id)
      self$get_connect()$DELETE(url)
    },
    #' @description Get this (remote) content item.
    internal_content = function() {
      url <- unversioned_url("applications", self$get_content()$guid)
      self$get_connect()$GET(url)
    },
    #' @description Update this content item.
    #' @param ... Content fields.
    update = function(...) {
      con <- self$get_connect()
      error_if_less_than(con$version, "1.8.6")
      url <- v1_url("content", self$get_content()$guid)
      body <- rlang::list2(...)
      if (length(body)) {
        # Only need to make a request if there are changes
        con$PATCH(url, body = body)
      }
      self
    },
    #' @description Delete this content item.
    danger_delete = function() {
      con <- self$get_connect()
      url <- v1_url("content", self$get_content()$guid)
      con$DELETE(url)
    },
    #' @description Return the URL for this content.
    get_url = function() {
      self$get_content()$content_url
    },
    #' @description Return the URL for this content in the Posit Connect dashboard.
    #' @param pane The pane in the dashboard to link to.
    get_dashboard_url = function(pane = "") {
      dashboard_url_chr(self$connect$server, self$content$guid, pane = pane)
    },
    #' @description Return the jobs for this content
    jobs = function() {
      res <- self$connect$GET(v1_url("content", self$content$guid, "jobs"), parser = NULL)
      use_unversioned <- endpoint_does_not_exist(res)
      if (use_unversioned) {
        res <- self$connect$GET(unversioned_url("applications", self$content$guid, "jobs"), parser = NULL)
      }
      self$connect$raise_error(res)
      parsed <- httr::content(res, as = "parsed")
      if (use_unversioned) {
        # The unversioned endpoint does not contain a `status` field. Its field
        # `finalized` is `FALSE` corresponds to active jobs. The `finalized`
        # field is dropped during parsing.
        parsed <- purrr::modify_if(parsed, ~ isFALSE(.x$finalized), function(x) {
          x$status <- 0
          x
        })
      }
      parsed
    },
    #' @description Return a single job for this content.
    #' @param key The job key.
    job = function(key) {
      warn_experimental("job")
      url <- unversioned_url("applications", self$get_content()$guid, "job", key)
      res <- self$get_connect()$GET(url)

      content_guid <- self$get_content()$guid
      purrr::map(
        list(res),
        ~ purrr::list_modify(.x, app_guid = content_guid)
      )[[1]]
    },
    #' @description Terminate a single job for this content item.
    #' @param key The job key.
    register_job_kill_order = function(key) {
      con <- self$connect
      url <- v1_url("content", self$content$guid, "jobs", key)
      res <- self$connect$DELETE(url)
      if (endpoint_does_not_exist(res)) {
        con$raise_error(res)
      }
      res
    },
    #' @description Return the variants for this content.
    variants = function() {
      warn_experimental("variants")
      url <- unversioned_url("applications", self$get_content()$guid, "variants")
      self$get_connect()$GET(url)
    },
    #' @description Set a tag for this content.
    #' @param tag_id The tag identifier.
    tag_set = function(tag_id) {
      self$get_connect()$set_content_tag(self$get_content()$guid, tag_id = tag_id)
    },
    #' @description Remove a tag for this content.
    #' @param tag_id The tag identifier.
    tag_delete = function(tag_id) {
      # note that deleting the parent tag deletes all children
      self$get_connect()$remove_content_tag(self$get_content()$guid, tag_id = tag_id)
    },
    #' @description The tags for this content.
    tags = function() {
      url <- v1_url("content", self$get_content()$guid, "tags")
      self$get_connect()$GET(url)
    },
    #' @description Add a principal to the ACL for this content.
    #' @param principal_guid GUID for the target user or group.
    #' @param principal_type Acting on user or group.
    #' @param role The kind of content access.
    permissions_add = function(principal_guid, principal_type, role) {
      url <- v1_url("content", self$get_content()$guid, "permissions")
      self$get_connect()$POST(url, body = list(
        principal_guid = principal_guid,
        principal_type = principal_type,
        role = role
      ))
    },
    #' @description Alter a principal in the ACL for this content.
    #' @param id The target identifier.
    #' @param principal_guid GUID for the target user or group.
    #' @param principal_type Acting on user or group.
    #' @param role The kind of content access.
    permissions_update = function(id, principal_guid, principal_type, role) {
      url <- v1_url("content", self$get_content()$guid, "permissions", id)
      self$get_connect()$PUT(url, body = list(
        principal_guid = principal_guid,
        principal_type = principal_type,
        role = role
      ))
    },
    #' @description Remove an entry from the ACL for this content.
    #' @param id The target identifier.
    permissions_delete = function(id) {
      url <- v1_url("content", self$get_content()$guid, "permissions", id)
      self$get_connect()$DELETE(url)
    },
    #' @description Obtain some or all of the ACL for this content.
    #' @param id The target identifier.
    #' @param add_owner Include the content owner in the result set.
    permissions = function(id = NULL, add_owner = FALSE) {
      guid <- self$get_content()$guid
      if (is.null(id)) {
        url <- v1_url("content", self$get_content()$guid, "permissions")
      } else {
        url <- v1_url("content", self$get_content()$guid, "permissions", id)
      }
      res <- self$get_connect()$GET(url)
      # NOTE: the default for the low-level functions is to map to the API
      # as close as possible. This differs from the "cleaner UX" functions
      if (add_owner) {
        owner_entry <- list(
          id = NA_character_,
          content_guid = guid,
          # TODO: what if groups can own content?
          principal_guid = self$get_content()$owner_guid,
          principal_type = "user",
          role = "owner"
        )
        res <- c(res, list(owner_entry))
      }
      res
    },
    #' @description Return the environment variables set for this content.
    environment = function() {
      url <- v1_url("content", self$get_content()$guid, "environment")
      self$get_connect()$GET(url)
    },
    #' @description Adjust the environment variables set for this content.
    #' @param ... Environment variable names and values. Use `NA` as the value
    #' to unset variables.
    environment_set = function(...) {
      url <- v1_url("content", self$get_content()$guid, "environment")
      # post with
      # key = NA to remove
      vals <- rlang::list2(...)
      body <- purrr::imap(vals, function(.x, .y) {
        # TODO: evaluate whether we should be coercing to character or erroring
        return(list(name = .y, value = as.character(.x)))
      })
      names(body) <- NULL

      self$get_connect()$PATCH(path = url, body = body)
    },
    #' @description Overwrite the environment variables set for this content.
    #' @param ... Environment variable names and values.
    environment_all = function(...) {
      url <- v1_url("content", self$get_content()$guid, "environment")

      vals <- rlang::list2(...)
      if (length(vals) == 0) {
        # Make sure we send an empty array and not an empty list
        body <- "[]"
      } else {
        body <- purrr::imap(vals, function(.x, .y) {
          # TODO: evaluate whether we should be coercing to character or erroring
          return(list(name = .y, value = as.character(.x)))
        })
        names(body) <- NULL
      }

      self$get_connect()$PUT(path = url, body = body)
    },
    #' @description Deploy this content
    #' @param bundle_id Target bundle identifier.
    deploy = function(bundle_id = NULL) {
      body <- list(bundle_id = bundle_id)
      self$get_connect()$POST(
        v1_url("content", self$get_content()$guid, "deploy"),
        body = body
      )
    },
    #' @description Adjust Git polling.
    #' @param enabled Polling enabled.
    repo_enable = function(enabled = TRUE) {
      warn_experimental("repo_enable")
      self$get_connect()$PUT(
        unversioned_url("applications", self$get_content()$guid, "repo"),
        body = list(
          enabled = enabled
        )
      )
    },
    #' @description Adjust Git repository.
    #' @param repository Git repository URL
    #' @param branch Git repository branch
    #' @param subdirectory Git repository directory
    repo_set = function(repository, branch, subdirectory) {
      warn_experimental("repo_set")
      self$get_connect()$POST(
        unversioned_url("applications", self$get_content()$guid, "repo"),
        body = list(
          repository = repository,
          branch = branch,
          subdirectory = subdirectory
        )
      )
    },
    #' @description Print this object.
    #' @param ... Unused.
    print = function(...) {
      cat("Posit Connect Content: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  Content URL: ", dashboard_url_chr(self$get_connect()$server, self$get_content()$guid), "\n", sep = "")
      cat("  Content Title: ", self$get_content()$title, "\n", sep = "")
      cat("\n")
      cat('content_item(client, guid = "', self$get_content()$guid, '")', "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  ),
  active = list(
    #' @field default_variant The default variant for this object.
    default_variant = function() {
      get_variant(self, "default")
    },

    #' @field is_rendered TRUE if this is a rendered content type, otherwise FALSE.
    is_rendered = function() {
      self$content$app_mode %in% c("rmd-static", "jupyter-static", "quarto-static")
    },

    #' @field is_interactive TRUE if this is a rendered content type, otherwise FALSE.
    is_interactive = function() {
      interactive_app_modes <- c(
        "shiny",
        "rmd-shiny",
        "jupyter-voila",
        "python-api",
        "python-dash",
        "python-streamlit",
        "python-bokeh",
        "python-fastapi",
        "python-shiny",
        "quarto-shiny",
        "tensorflow-saved-model",
        "api"
      )
      self$content$app_mode %in% interactive_app_modes
    }
  )
)

#' Environment
#'
#' An R6 class that represents a Content's Environment Variables
#'
#' @rdname EnvironmentR6
#'
#' @family R6 classes
#' @export
Environment <- R6::R6Class(
  "Environment",
  inherit = Content,
  public = list(
    #' @field env_raw The (raw) set of environment variables.
    env_raw = NULL,
    #' @field env_vars The set of environment variables.
    env_vars = NULL,

    #' @description Initialize this set of environment variables.
    #' @param connect The `Connect` instance.
    #' @param content The `Content` instance.
    initialize = function(connect, content) {
      super$initialize(connect = connect, content = content)
      self$env_refresh()
    },
    #' @description Fetch the set of environment variables.
    environment = function() {
      res <- super$environment()
      env_raw <- res
      env_vars <- res
      return(res)
    },
    #' @description Update the set of environment variables.
    #' @param ... Environment variable names and values.
    environment_set = function(...) {
      res <- super$environment_set(...)
      env_raw <- res
      env_vars <- res
      return(res)
    },
    #' @description Overwrite the set of environment variables.
    #' @param ... Environment variable names and values.
    environment_all = function(...) {
      res <- super$environment_all(...)
      env_raw <- res
      env_vars <- res
      return(res)
    },
    #' @description Fetch the set o environment variables.
    env_refresh = function() {
      # mutates the existing instance, so future
      # references have the right version
      self$env_raw <- self$environment()
      self$env_vars <- self$env_raw
      return(self)
    },
    #' @description Print this object.
    #' @param ... Unused.
    print = function(...) {
      super$print(...)
      cat("Environment Variables:\n")
      cat("  vctrs::vec_c(\n")
      purrr::map(self$env_vars, ~ cat(paste0('    "', .x, '",\n')))
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
#' Manage Environment Variables for a piece of content.
#'
#' `get_environment()` returns an Environment object for use with "setter" methods
#'
#' `set_environment_new()` updates environment values (either creating new
#' values or updating existing). Set `NA` as the value to remove a variable.
#'
#' `set_environment_remove()` is a wrapper on `set_environment_new()` that
#' allows removing named / listed variables quickly
#'
#' `set_environment_all()` sets _all_ environment variable values (will remove
#' variables not specified)
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
  validate_R6_class(content, "Content")
  content_data <- content$get_content_remote()
  connect_client <- content$get_connect()
  return(Environment$new(connect_client, content_data))
}

#' @rdname environment
#' @export
set_environment_new <- function(env, ...) {
  validate_R6_class(env, "Content")

  if (!inherits(env, "Environment")) {
    env <- get_environment(env)
  }

  # update existing env vars with new ones
  new_env_vars <- rlang::dots_list(...)

  env$environment_set(!!!new_env_vars)

  env$env_refresh()
}

#' @rdname environment
#' @export
set_environment_remove <- function(env, ...) {
  to_remove <- rlang::enexprs(...)
  to_remove_names <- c(names(to_remove), as.character(unlist(to_remove, as.character)))
  to_remove_names <- to_remove_names[nchar(to_remove_names) > 0]
  to_remove_final <- rlang::set_names(rep(NA, length(to_remove_names)), to_remove_names)

  set_environment_new(env, !!!to_remove_final)
}

#' @rdname environment
#' @export
set_environment_all <- function(env, ...) {
  validate_R6_class(env, "Content")

  if (!inherits(env, "Environment")) {
    env <- get_environment(env)
  }

  # set all environment variables
  new_env_vars <- rlang::dots_list(...)

  env$environment_all(!!!new_env_vars)

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
#' @examples
#' \dontrun{
#' connect() %>%
#'   content_item("some-guid") %>%
#'   content_update_access_type("all")
#' }
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

  content_title <- tryCatch(
    {
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

#' @importFrom uuid UUIDgenerate
content_ensure <- function(
  connect,
  name = uuid::UUIDgenerate(),
  title = name,
  guid = NULL,
  ...,
  .permitted = c("new", "existing")
) {
  if (!is.null(guid)) {
    # guid-based deployment
    # just in case we get a 404 back...
    content <- tryCatch(
      suppressMessages(connect$content(guid = guid)),
      error = function(e) {
        return(NULL)
      }
    )
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
        "Found {length(content)} content items ",
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
#' Retrieve details about server processes associated with a `content_item`,
#' such as a FastAPI app or a Quarto render.
#'
#' Note that Connect versions below 2022.10.0 use a legacy endpoint, and will
#' not return the complete set of information provided by newer versions.
#'
#' @param content A Content object, as returned by `content_item()`
#'
#' @return A data frame with a row for each job, with the following columns:
#'
#' - `id`: The job identifier.
#' - `ppid`: The job's parent process identifier (see Note 1).
#' - `pid`: The job's process identifier.
#' - `key`: The job's unique key identifier.
#' - `remote_id`: The job's identifier for off-host execution configurations
#' (see Note 1).
#' - `app_id`: The job's parent content identifier
#' - `variant_id`: The identifier of the variant owning this job.
#' - `bundle_id`: The identifier of a content bundle linked to this job.
#' - `start_time`: The timestamp (RFC3339) indicating when this job started.
#' - `end_time`: The timestamp (RFC3339) indicating when this job finished.
#' - `last_heartbeat_time`: The timestamp (RFC3339) indicating the last time
#' this job was observed to be running (see Note 1).
#' - `queued_time`: The timestamp (RFC3339) indicating when this job was added
#' to the queue to be processed. Only scheduled reports will present a value
#' for this field (see Note 1).
#' - `queue_name`: The name of the queue which processes the job. Only
#' scheduled reports will present a value for this field (see Note 1).
#' - `tag`: A tag to identify the nature of the job.
#' - `exit_code`: The job's exit code. Present only when job is finished.
#' - `status`: The current status of the job. On Connect 2022.10.0 and newer,
#' one of Active: 0, Finished: 1, Finalized: 2; on earlier versions, Active:
#' 0, otherwise `NA`.
#' - `hostname`: The name of the node which processes the job.
#' - `cluster`: The location where this content runs. Content running on the
#' same server as Connect will have either a null value or the string Local.
#' Gives the name of the cluster when run external to the Connect host
#' (see Note 1).
#' - `image`: The location where this content runs. Content running on
#' the same server as Connect will have either a null value or the string
#' Local. References the name of the target image when content runs in
#' a clustered environment such as Kubernetes (see Note 1).
#' - `run_as`: The UNIX user that executed this job.
#'
#' @note
#' 1. On Connect instances earlier than 2022.10.0, these columns will contain `NA` values.
#'
#' @family job functions
#' @family content functions
#' @export
get_jobs <- function(content) {
  validate_R6_class(content, "Content")

  jobs <- content$jobs()
  parse_connectapi_typed(jobs, connectapi_ptypes$jobs, order_columns = TRUE)
}

# TODO: Need to test `logged_error` on a real error
#'
#' Retrieve details about a server process
#' associated with a `content_item`, such as a FastAPI app or a Quarto render.
#'
#' @param content A Content object, as returned by `content_item()`
#' @param key The key for a job
#'
#' @family job functions
#' @family content functions
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
  parse_connectapi_typed(list(job), connectapi_ptypes$job)
}

#' Terminate Jobs
#'
#' Register a job kill order for one or more jobs associated with a content
#' item. Requires Connect 2022.10.0 or newer.
#'
#' @param content A Content object, as returned by `content_item()`
#' @param keys Optional. One or more job keys, which can be obtained using
#' `get_jobs(content)`. If no keys are provided, will terminate all active
#' jobs for the provided content item.

#' @return A data frame with a row for each requested termination.
#'
#' - `app_id`: The content item's identifier.
#' - `app_guid`: The content item's GUID.
#' - `job_key`: The job key.
#' - `job_id`: The job's identifier.
#' - `result`: The result string returned by Connect.
#' - `code`: An error code, `NA` if the request was successful.
#' - `error`: An error message, `NA` if the result was successful.
#'
#' Note that `app_id`, `app_guid`, `job_id`, and `result` are `NA` if if the
#' request returned an error.
#'
#' @family job functions
#' @family content functions
#' @export
terminate_jobs <- function(content, keys = NULL) {
  validate_R6_class(content, "Content")

  if (is.null(keys)) {
    all_jobs <- get_jobs(content)
    keys <- all_jobs[all_jobs$status == 0, ]$key
    if (length(keys) == 0) {
      message("No active jobs found.")
      return(vctrs::vec_ptype(connectapi_ptypes$job_termination))
    }
  }


  res <- purrr::map(keys, content$register_job_kill_order)
  res_content <- purrr::map(res, httr::content)
  res_df <- tibble::tibble(
    parse_connectapi_typed(
      res_content,
      connectapi_ptypes$job_termination,
      order_columns = TRUE)
  )
  # Errors will not have the job_key.
  res_df$job_key <- keys
  res_df
}

#' Set RunAs User
#'
#' Set the `RunAs` user for a piece of content.
#' The `run_as_current_user` flag only does anything if:
#'
#' - PAM is the authentication method
#' - `Applications.RunAsCurrentUser` is enabled on the server
#'
#' Also worth noting that the `run_as` user must exist on the Posit Connect
#' server (as a linux user) and have appropriate group memberships, or you will
#' get a `400: Bad Request`. Set to `NULL` to use the default RunAs user / unset
#' any current configuration.
#'
#' To "read" the current RunAs user, use the `Content` object or `get_content()` function.
#'
#' @param content an R6 Content item
#' @param run_as The RunAs user to use for this content
#' @param run_as_current_user Whether to run this content as the viewer of the application
#'
#' @return a Content object, updated with new details
#'
#' @seealso connectapi::content_update
#'
#' @family content functions
#' @export
set_run_as <- function(content, run_as, run_as_current_user = FALSE) {
  validate_R6_class(content, "Content")

  content$update(run_as = run_as, run_as_current_user = run_as_current_user)

  invisible(content$get_content_remote())

  return(content)
}


#' Delete Content
#'
#' Delete a content item. WARNING: This action deletes all history, configuration,
#' logs, and resources about a content item. It _cannot_ be undone.
#'
#' @param content an R6 content item
#' @param force Optional. A boolean that determines whether we should prompt in interactive sessions
#'
#' @return The R6 Content item. The item is deleted, but information about it is cached locally
#'
#' @family content functions
#' @export
content_delete <- function(content, force = FALSE) {
  validate_R6_class(content, "Content")

  cn <- content$get_content_remote()
  if (!force) {
    if (interactive()) {
      cat(glue::glue("WARNING: Are you sure you want to delete '{cn$title}' ({cn$guid})?"))
      if (utils::menu(c("Yes", "No")) == 2) {
        stop("'No' selected. Aborting content delete")
      }
    }
  }

  message(glue::glue("Deleting content '{cn$title}' ({cn$guid})"))
  res <- content$danger_delete()
  content$get_connect()$raise_error(res)

  return(content)
}

#' Update Content
#'
#' Update settings for a content item. For a list of all settings, see the
#' [latest
#' documentation](https://docs.posit.co/connect/api/#patch-/v1/content/{guid})
#' or the documentation for your server via `connectapi::browse_api_docs()`.
#'
#' Popular selections are `content_update(access_type="all")`,
#' `content_update(access_type="logged_in")` or
#' `content_update(access_type="acl")`, process settings, title, description,
#' etc.
#'
#' - `content_update_access_type()` is a helper to make it easier to change access_type
#' - `content_update_owner()` is a helper to make it easier to change owner
#'
#' @param content An R6 content item
#' @param ... Settings up update that are passed along to Posit Connect
#' @param access_type One of "all", "logged_in", or "acl"
#' @param owner_guid The GUID of a user who is a publisher, so that they can
#'   become the new owner of the content
#'
#' @return An R6 content item
#'
#' @family content functions
#' @export
content_update <- function(content, ...) {
  validate_R6_class(content, "Content")

  content$update(...)

  content$get_content_remote()

  return(content)
}

#' @rdname content_update
#' @export
content_update_access_type <- function(content, access_type = c("all", "logged_in", "acl")) {
  if (length(access_type) > 1 || !access_type %in% c("all", "logged_in", "acl")) {
    stop("Please select one of 'all', 'logged_in', or 'acl'.")
  }
  content_update(content = content, access_type = access_type)
}

#' @rdname content_update
#' @export
content_update_owner <- function(content, owner_guid) {
  content_update(content = content, owner_guid = owner_guid)
}


#' Verify Content Name
#'
#' Ensures that a content name fits the specifications / requirements of Posit
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
  if (grepl("[^\\-\\_a-zA-Z0-9]", name, perl = TRUE) || nchar(name) < 3 || nchar(name) > 64) {
    stop(glue::glue(
      "ERROR: content name '{name}' must be between 3 and 64 alphanumeric characters, ",
      "dashes, and underscores"
    ))
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
#'
#' @rdname get_bundles
#' @param bundle_id A specific bundle ID for a content item
#' @family content functions
#' @export
get_bundles <- function(content) {
  validate_R6_class(content, "Content")
  bundles <- content$get_bundles()

  parse_connectapi_typed(bundles, connectapi_ptypes$bundles)
}

#' @rdname get_bundles
#' @family content functions
#' @export
delete_bundle <- function(content, bundle_id) {
  validate_R6_class(content, "Content")
  cn <- content$get_content_remote()
  message(glue::glue("Deleting bundle {bundle_id} for content '{cn$title}' ({cn$guid})"))
  res <- content$bundle_delete(bundle_id)
  content$get_connect()$raise_error(res)
  return(content)
}


#' Content permissions
#'
#' Get or set content permissions for a content item
#'
#' Permission modification:
#' - `content_add_*` adds a permission to the content
#' - `content_delete_*` removes a permission from the content
#'
#' Permission retrieval:
#' - `get_content_permissions()` lists permissions
#' - `get_my_permission()` gets the permission associated with the caller.
#' - `get_user_permission()` gets the permissions associated with a given user.
#'   It does not evaluate group memberships
#' - `get_group_permission()` gets the permissions associated with a given
#'   group.
#'
#' NOTE: by default, the owner is injected with an "NA_character_" permission id.
#' This makes it easier to find / isolate this record.
#'
#' @param content An R6 content object
#' @param guid The guid associated with either a user (for `content_add_user`) or group (for `content_add_group`)
#' @param role The role to assign to a user. Either "viewer" or "owner." Defaults to "viewer"
#' @param add_owner Optional. Whether to include the owner in returned
#'   permission sets. Default is TRUE. The owner will have an NA_character_
#'   permission "id"
#'
#' @name permissions
#' @rdname permissions
#' @family content functions
#' @export
content_add_user <- function(content, guid, role = c("viewer", "owner")) {
  validate_R6_class(content, "Content")
  role <- .define_role(role)

  purrr::map(guid, ~ .content_add_permission_impl(content, "user", .x, role))

  return(content)
}

#' @rdname permissions
#' @export
content_add_group <- function(content, guid, role = c("viewer", "owner")) {
  validate_R6_class(content, "Content")
  role <- .define_role(role)

  purrr::map(guid, ~ .content_add_permission_impl(content = content, type = "group", guid = .x, role = role))

  return(content)
}

.content_delete_permission_impl <- function(content, type, guid) {
  res <- .get_permission(content, type, guid)
  if (length(res) > 0) {
    message(glue::glue("Removing {type} permission for '{guid}'"))
    remove_permission <- content$permissions_delete(res[[1]]$id)
    return(remove_permission)
  } else {
    message(glue::glue("{type} '{guid}' already does not have access. No permission being removed"))
    return(NULL)
  }
}

.content_add_permission_impl <- function(content, type, guid, role) {
  existing <- .get_permission(content, type, guid)
  if (length(existing) > 0) {
    message(glue::glue("Updating permission for {type} '{guid}' with role '{role}'"))
    res <- content$permissions_update(
      id = existing[[1]]$id,
      principal_guid = guid,
      principal_type = type,
      role = role
    )
  } else {
    message(glue::glue("Adding permission for {type} '{guid}' with role '{role}'"))
    res <- content$permissions_add(
      principal_guid = guid,
      principal_type = type,
      role = role
    )
  }
  return(res)
}

#' @rdname permissions
#' @export
content_delete_user <- function(content, guid) {
  validate_R6_class(content, "Content")
  purrr::map(guid, ~ .content_delete_permission_impl(content = content, type = "user", guid = .x))
  return(content)
}

#' @rdname permissions
#' @export
content_delete_group <- function(content, guid) {
  validate_R6_class(content, "Content")
  purrr::map(guid, ~ .content_delete_permission_impl(content = content, type = "group", guid = .x))
  return(content)
}

.define_role <- function(role) {
  if (length(role) > 1) {
    # use default
    return("viewer")
  } else {
    if (role %in% c("viewer", "owner")) {
      return(role)
    } else {
      stop(glue::glue("ERROR: invalid role. Expected 'viewer' or 'owner', instead got {{ role }}"))
    }
  }
}

.get_permission <- function(content, type, guid, add_owner = TRUE) {
  res <- content$permissions(add_owner = add_owner)
  purrr::keep(res, ~ identical(.x$principal_type, type) && identical(.x$principal_guid, guid))
}

#' @rdname permissions
#' @export
get_user_permission <- function(content, guid, add_owner = TRUE) {
  validate_R6_class(content, "Content")
  res <- .get_permission(content, "user", guid, add_owner = add_owner)
  if (length(res) > 0) {
    return(res[[1]])
  } else {
    return(NULL)
  }
}

#' @rdname permissions
#' @export
get_my_permission <- function(content, add_owner = TRUE) {
  my_guid <- content$get_connect()$GET("me")$guid
  get_user_permission(content, my_guid, add_owner = add_owner)
}

#' @rdname permissions
#' @export
get_group_permission <- function(content, guid) {
  validate_R6_class(content, "Content")
  # do not add_owner, because groups cannot own content
  res <- .get_permission(content, "group", guid, add_owner = FALSE)
  if (length(res) > 0) {
    return(res[[1]])
  } else {
    return(NULL)
  }
}


#' @rdname permissions
#' @export
get_content_permissions <- function(content, add_owner = TRUE) {
  validate_R6_class(content, "Content")
  res <- content$permissions(add_owner = add_owner)
  parse_connectapi_typed(res, connectapi_ptypes$permissions)
}

#' Render a content item.
#'
#' @description Submit a request to render a content item. Once submitted, the
#' server runs an asynchronous process to render the content. This might be
#' useful if content needs to be updated after its source data has changed,
#' especially if this doesn't happen on a regular schedule.
#'
#' Only valid for rendered content (e.g., most Quarto documents, Jupyter
#' notebooks, R Markdown reports).
#'
#' @param content The content item you wish to render.
#' @param variant_key If a variant key is provided, render that variant. Otherwise, render the default variant.
#' @return A [VariantTask] object that can be used to track completion of the render.
#'
#' @examples
#' \dontrun{
#' client <- connect()
#' item <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
#' task <- content_render(item)
#' poll_task(task)
#' }
#'
#' @export
content_render <- function(content, variant_key = NULL) {
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  if (!content$is_rendered) {
    stop(glue::glue(
      "Render not supported for application mode: {content$content$app_mode}. ",
      "Did you mean content_restart()?"
    ))
  }
  if (is.null(variant_key)) {
    target_variant <- get_variant(content, "default")
  } else {
    target_variant <- get_variant(content, variant_key)
  }
  render_task <- target_variant$render()

  VariantTask$new(
    connect = content$connect,
    content = content$content,
    key = target_variant$key,
    task = render_task
  )
}

#' Restart a content item.
#'
#' @description Submit a request to restart a content item. Once submitted, the
#' server performs an asynchronous request to kill all processes associated with
#' the content item, starting new processes as needed. This might be useful if
#' the application relies on data that is loaded at startup, or if its memory
#' usage has grown over time.
#'
#' Note that users interacting with certain types of applications may have their
#' workflows interrupted.
#'
#' Only valid for interactive content (e.g., applications, APIs).
#'
#' @param content The content item you wish to restart.
#'
#' @examples
#' \dontrun{
#' client <- connect()
#' item <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
#' content_restart(item)
#' }
#'
#' @importFrom rlang :=
#' @export
content_restart <- function(content) {
  validate_R6_class(content, "Content")
  if (!content$is_interactive) {
    stop(glue::glue(
      "Restart not supported for application mode: {content$content$app_mode}. ",
      "Did you mean content_render()?"
    ))
  }
  unix_epoch_in_seconds <- as.integer(Sys.time())
  # nolint start: object_usage_linter, object_name_linter
  # https://rlang.r-lib.org/reference/glue-operators.html#using-glue-syntax-in-packages
  env_var_name <- glue::glue("_CONNECT_RESTART_{unix_epoch_in_seconds}")
  content$environment_set("{env_var_name}" := unix_epoch_in_seconds)
  content$environment_set("{env_var_name}" := NA)
  # nolint end
  invisible(NULL)
}
