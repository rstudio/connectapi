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
    update = function(...) {
      params <- rlang::list2(...)
      url <- glue::glue("v1/experimental/content/{self$get_content()$guid}")
      res <- self$get_connect()$POST(
        url,
        params
      )
      return(self)
    },
    get_dashboard_url = function(pane = "") {
      dashboard_url_chr(self$connect$host, self$content$guid, pane = pane)
    },
    get_jobs = function() {
      warn_experimental("get_jobs")
      url <- glue::glue("applications/{self$get_content()$guid}/jobs")
      self$get_connect()$GET(url)
    },
    get_job = function(key) {
      warn_experimental("get_job")
      url <- glue::glue("applications/{self$get_content()$guid}/job/{key}")
      self$get_connect()$GET(url)
    },
    print = function(...) {
      cat("RStudio Connect Content: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  Content URL: ", self$get_content()$url, "\n", sep = "")
      cat("  Content Title: ", self$get_content()$title, "\n", sep = "")
      cat("\n")
      cat('content_item(client, guid = "', self$get_content()$guid, '")', "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

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

#' Get ACL Details
#'
#' \lifecycle{experimental} Retrieve the Access Controls associated with a given
#' piece of content.
#'
#' NOTE: ACLs can still be stored, even when access_type for content is "all" or
#' "logged_in" users. In these cases, granting or removing "viewer" privileges
#' have no effect.
#'
#' @param content [Content] An R6 Content item
#'
#' @return A list of users who have access to the content
#'
#' @family content functions
#' @export
get_acl_user <- function(content) {
  warn_experimental("get_acl")

  content_info <- content$get_content_remote()
  prep <- get_acl_user_impl(content)
  out <- parse_connectapi_typed(prep, !!!connectapi_ptypes$acl)
  out$content_guid <- content_info$guid
  out$content_access_type <- content_info$access_type

  return(out)
}

#' @rdname get_acl_user
#' @export
get_acl <- function(content) {
  lifecycle::deprecate_warn(
    "0.1.0.9007", "get_acl()", "get_acl_user()"
    )
  get_acl_user(content)
}

get_acl_impl <- function(content) {
  validate_R6_class(content, "Content")
  client <- content$get_connect()
  res <- client$GET(glue::glue("applications/{content$get_content()$guid}"))
  
  content_info <- content$get_content_remote()

  if (content_info$access_type != "acl") {
    # we warn once per content item
    warn_once(
      glue::glue("Content (guid: {content_info$guid}) has access type {content_info$access_type}: ACLs for viewers have no effect"),
      glue::glue("get_acl_not_acl_{content_info$guid}")
    )
  }
  
  return(res)
}

get_acl_user_impl <- function(content) {
  res <- get_acl_impl(content)
  client <- content$get_connect()
  owner <- client$user(res$owner_guid)
  owner$app_role <- "owner"
  # because collaborators are hard to differentiate
  owner$is_owner <- TRUE

  content_acls <- res[["users"]]
  content_acls <- purrr::map(content_acls, function(.x) {
    .x$is_owner <- FALSE
    return(.x)
  })

  return(c(list(owner), content_acls))
}

get_acl_group_impl <- function(content) {
  res <- get_acl_impl(content)

  content_acls <- res[["groups"]]
  content_acls <- purrr::map(content_acls, function(.x) {
    return(.x)
  })

  return(c(content_acls))
}

content_ensure <- function(connect, name = uuid::UUIDgenerate(), title = name, guid = NULL, ...) {
  if (!is.null(guid)) {
    # guid-based deployment
    # just in case we get a 404 back...
    content <- tryCatch(connect$content(guid = guid), error = function(e) {
      return(NULL)
    })
    if (is.null(content)) {
      warning(glue::glue(
        "guid {guid} was not found on {connect$host}.",
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
        "matching {name} on {connect$host}",
        ", content must have a unique name."
      ))
    } else if (length(content) == 0) {
      # create app
      content <- connect$content_create(
        name = name,
        title = title,
        ...
      )
      message(glue::glue(
        "Creating NEW content {content$guid} ",
        "with name {name} on {connect$host}"
      ))
    } else {
      content <- content[[1]]
      message(glue::glue(
        "Found EXISTING content {content$guid} with ",
        "name {name} on {connect$host}"
      ))
      # update values...? need a PUT endpoint
    }
  }
  return(content)
}
