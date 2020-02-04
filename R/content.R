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
    get_connect = function(){self$connect},
    get_content = function(){self$content},
    get_content_remote = function(){
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
    get_dashboard_url = function(pane = ""){
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
    jobs = function() {
      warn_experimental("jobs")
      url <- glue::glue("applications/{self$get_content()$guid}/jobs")
      self$get_connect()$GET(url)
    },
    job = function(key) {
      warn_experimental("job")
      url <- glue::glue("applications/{self$get_content()$guid}/job/{key}")
      self$get_connect()$GET(url)
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
      self$get_connect()$GET(url)
    },
    environment_set = function(..., .version = 0) {
      warn_experimental("environment_set")
      url <- glue::glue("applications/{self$get_content()$guid}/environment")
      # post with 
      # key = NA to retain
      # post without a variable/key to remove
      # bump version number each time
      vals <- rlang::list2(...)
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

#' Variant
#' 
#' An R6 class that represents a Variant
#' 
Variant <- R6::R6Class(
  "Variant",
  inherit = Content,
  public = list(
    variant = NULL,
    get_variant = function() {self$variant},
    initialize = function(connect, content, variant) {
      # TODO
    },
    send_mail = function(to = c("me", "collaborators", "collaborators_viewers")) {
      warn_experimental("send_mail")
      if (length(to) > 1) to <- "me"
      url <- glue::glue("variants/{self$get_variant()$id}/sender")
      self$get_connect()$POST(
        path = url, 
        body = list(
          email = to
        ))
    },
    render = function() {
      warn_experimental("render")
      url <- glue::glue("variants/{self$get_variant()$id}/render")
      self$get_connect()$POST(
        path = url,
        body = list(
          email = "none",
          activate = TRUE
        )
      )
    },
    renderings = function() {
      warn_experimental("renderings")
      url <- glue::glue("variants/{self$get_variant()$id}/renderings")
      self$get_connect()$GET(
        path = url
      )
    },
    navigate_rev = function() {
      glue::glue("content_url/variant_hash/_rev{rev_id}")
    }
  )
)

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
get_acl <- function(content) {
  warn_experimental("get_acl")

  content_info <- content$get_content_remote()
  prep <- get_acl_impl(content)
  out <- parse_connectapi_typed(prep, !!!connectapi_ptypes$acl)
  out$content_guid <- content_info$guid
  out$content_access_type <- content_info$access_type
  
  return(out)
}

get_acl_impl <- function(content) {
  validate_R6_class(content, "Content")
  client <- content$get_connect()
  res <- client$GET(glue::glue("applications/{content$get_content()$guid}"))
  
  owner <- client$user(res$owner_guid)
  owner$app_role <- "owner"
  # because collaborators are hard to differentiate
  owner$is_owner <- TRUE
  
  content_info <- content$get_content_remote()
  
  if (content_info$access_type != "acl") {
    # we warn once per content item
    warn_once(
      glue::glue("Content (guid: {content_info$guid}) has access type {content_info$access_type}: ACLs for viewers have no effect"),
      glue::glue("get_acl_not_acl_{content_info$guid}")
    )
  }
  
  content_acls <- res[["users"]]
  content_acls <- purrr::map(content_acls, function(.x){.x$is_owner <- FALSE; return(.x)})
  
  return(c(list(owner), content_acls))
}
