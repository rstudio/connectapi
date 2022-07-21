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
    get_bundles = function() {
      url <- glue::glue("v1/content/{self$get_content()$guid}/bundles")
      self$get_connect()$GET(url)
    },
    bundle_download = function(bundle_id, filename = tempfile(pattern = "bundle", fileext=".tar.gz"), overwrite = FALSE) {
      url <- glue::glue("/v1/content/{self$get_content()$guid}/bundles/{bundle_id}/download")
      self$get_connect()$GET(url, httr::write_disk(filename, overwrite = overwrite), "raw")
      return(filename)
    },
    bundle_delete = function(bundle_id) {
      url <- glue::glue("/v1/content/{self$get_content()$guid}/bundles/{bundle_id}")
      self$get_connect()$DELETE(url)
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
    danger_delete = function() {
      con <- self$get_connect()
      url <- glue::glue("v1/content/{self$get_content()$guid}")
      res <- con$DELETE(url)
      return(res)
    },
    runas = function(run_as, run_as_current_user = FALSE) {
      lifecycle::deprecate_soft("0.1.1", "Content$runas()", "content$update()")

      self$update(run_as = run_as, run_as_current_user = run_as_current_user)
    },
    get_url = function() {
      self$get_content()$content_url
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
    tag_set = function(tag_id) {
      self$get_connect()$set_content_tag(self$get_content()$guid, tag_id = tag_id)
    },
    tag_delete = function(id) {
      # note that deleting the parent tag deletes all children
      self$get_connect()$tag_delete(id)
    },
    tags = function() {
      url <- glue::glue("v1/content/{self$get_content()$guid}/tags")
      self$get_connect()$GET(url)
    },
    permissions_add = function(principal_guid, principal_type, role) {
      url <- glue::glue("v1/content/{self$get_content()$guid}/permissions")
      self$get_connect()$POST(url, body = list(
        principal_guid = principal_guid,
        principal_type = principal_type,
        role = role
      ))
    },
    permissions_update = function(id, principal_guid, principal_type, role) {
      url <- glue::glue("v1/content/{self$get_content()$guid}/permissions/{id}")
      self$get_connect()$PUT(url, body = list(
        principal_guid = principal_guid,
        principal_type = principal_type,
        role = role
      ))
    },
    permissions_delete = function(id) {
      url <- glue::glue("v1/content/{self$get_content()$guid}/permissions/{id}")
      self$get_connect()$DELETE(url)
    },
    permissions = function(id = NULL, add_owner=FALSE) {
      guid <- self$get_content()$guid
      url <- glue::glue("v1/content/{guid}/permissions")
      if (!is.null(id)) {
        url <- glue::glue("{url}/{id}")
      }
      res <- self$get_connect()$GET(url)
      # NOTE: the default for the low-level functions is to map to the API
      # as close as possible. This differs from the "cleaner UX" functions
      if (add_owner) {
        owner_entry <- list(
          id = NA_character_,
          content_guid = guid,
          # TODO: what if groups can own content?
          principal_guid = self$get_content()$owner,
          principal_type = "user",
          role = "owner"
          )
        return(c(res, list(owner_entry)))
      }
      return(res)
    },
    environment = function() {
      url <- glue::glue("v1/content/{self$get_content()$guid}/environment")
      res <- self$get_connect()$GET(url)
      return(res)
    },
    environment_set = function(...) {
      url <- glue::glue("v1/content/{self$get_content()$guid}/environment")
      # post with
      # key = NA to remove
      vals <- rlang::list2(...)
      body <- purrr::imap(vals, function(.x, .y) {
      # TODO: evaluate whether we should be coercing to character or erroring
        return(list(name = .y, value = as.character(.x)))
      })
      names(body) <- NULL

      res <- self$get_connect()$PATCH(
        path = url,
        body = body
      )
      res
    },
    environment_all = function(...) {
      url <- glue::glue("v1/content/{self$get_content()$guid}/environment")

      vals <- rlang::list2(...)
      body <- purrr::imap(vals, function(.x, .y) {
      # TODO: evaluate whether we should be coercing to character or erroring
        return(list(name = .y, value = as.character(.x)))
      })
      names(body) <- NULL

      res <- self$get_connect()$PUT(
        path = url,
        body = body,
        .empty_object = FALSE
      )
      res
    },
    deploy = function(bundle_id = NULL) {
      body <- list(bundle_id = bundle_id)
      self$get_connect()$POST(
        glue::glue("v1/content/{self$get_content()$guid}/deploy"),
        body = body
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
#' @rdname EnvironmentR6
#'
#' @family R6 classes
#' @export
Environment <- R6::R6Class(
  "Environment",
  inherit = Content,
  public = list(
    env_raw = NULL,
    env_vars = NULL,
    initialize = function(connect, content) {
      super$initialize(connect = connect, content = content)
      self$env_refresh()
    },
    environment = function() {
      res <- super$environment()
      env_raw <- res
      env_vars <- res
      return(res)
    },
    environment_set = function(...) {
      res <- super$environment_set(...)
      env_raw <- res
      env_vars <- res
      return(res)
    },
    environment_all = function(...) {
      res <- super$environment_all(...)
      env_raw <- res
      env_vars <- res
      return(res)
    },
    env_refresh = function() {
      # mutates the existing instance, so future
      # references have the right version
      self$env_raw <- self$environment()
      self$env_vars <- self$env_raw
      return(self)
    },
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
#'   connect() %>%
#'     content_item("some-guid") %>%
#'     content_update_access_type("all")
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

#' @importFrom uuid UUIDgenerate
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
#' Set the `RunAs` user for a piece of content.
#' The `run_as_current_user` flag only does anything if:
#'
#' - PAM is the authentication method
#' - `Applications.RunAsCurrentUser` is enabled on the server
#'
#' Also worth noting that the `run_as` user must exist on the RStudio Connect
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
content_delete <- function(content, force=FALSE) {
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
#' documentation](https://docs.rstudio.com/connect/api/#patch-/v1/content/{guid})
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
#' @param ... Settings up update that are passed along to RStudio Connect
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

  res <- content$update(...)

  content$get_content_remote()

  return(content)
}

#' @rdname content_update
#' @export
content_update_access_type <- function(content, access_type=c("all", "logged_in", "acl")) {
  if (length(access_type) > 1 || !access_type %in% c("all", "logged_in", "acl")) {
    stop("Please select one of 'all', 'logged_in', or 'acl'.")
  }
  content_update(content = content, access_type=access_type)
}

#' @rdname content_update
#' @export
content_update_owner <- function(content, owner_guid) {
  content_update(content = content, owner_guid = owner_guid)
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
#' @param bundle_id A specific bundle ID for a content item
#'
#' @rdname get_bundles
#' @family content functions
#' @export
get_bundles <- function(content, limit = Inf) {
  if (limit != Inf) {
    # deprecate_warn cannot tell if the arg was the default or not
    lifecycle::deprecate_warn("0.1.0.9029", "get_bundles(limit)")
  }
  validate_R6_class(content, "Content")
  bundles <- content$get_bundles()

  parse_connectapi_typed(bundles, !!!connectapi_ptypes$bundles)
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

  res <- purrr::map(guid, ~ .content_add_permission_impl(content, "user", .x, role))

  return(content)
}

#' @rdname permissions
#' @export
content_add_group <- function(content, guid, role = c("viewer", "owner")) {
  validate_R6_class(content, "Content")
  existing <- .get_permission(content, "group", guid)
  role <- .define_role(role)

  res <- purrr::map(guid, ~ .content_add_permission_impl(content = content, type = "group", guid = .x, role = role))

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
  res <- purrr::map(guid, ~ .content_delete_permission_impl(content = content, type = "user", guid = .x))
  return(content)
}

#' @rdname permissions
#' @export
content_delete_group <- function(content, guid) {
  validate_R6_class(content, "Content")
  res <- purrr::map(guid, ~ .content_delete_permission_impl(content = content, type = "group", guid = .x))
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
  purrr::keep(res, ~ .x$principal_type == type && .x$principal_guid == guid)
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
  parse_connectapi_typed(res, !!!connectapi_ptypes$permissions)
}



