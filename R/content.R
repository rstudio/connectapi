#' Content
#' 
#' An R6 class that represents content
#' 
#' @family deployR6 functions
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

#' Get ACL Details
#' 
#' \lifecycle{experimental} Retrieve the Access Controls
#' associated with a given piece of content.
#' 
#' @param content [Content] An R6 Content item
#' 
#' @return A list of users who have access to the content
#' @export
get_acl <- function(content) {
  warn_experimental("get_acl")
  validate_R6_class(content, "Content")
  client <- content$get_connect()
  res <- client$GET(glue::glue("applications/{content$get_content()$guid}"))
  
  owner <- client$user(res$owner_guid)
  owner$app_role <- "owner"
  # because collaborators are hard to differentiate
  owner$is_owner <- TRUE
  
  content_info <- content$get_content_remote()
  
  if (content_info$access_type != "acl") {
    # we should do this more often than once...
    warn_once(
      glue::glue("Content (guid: {content_info$guid}) has access type {content_info$access_type}: ACLs for viewers have no effect"),
      "get_acl_not_acl"
      )
  }
  
  content_acls <- res[["users"]]
  content_acls <- purrr::map(content_acls, function(.x){.x$is_owner <- FALSE; return(.x)})
  
  prep <- c(list(owner), content_acls)
  out <- parse_connectapi_typed(prep, !!!connectapi_ptypes$acl)
  out$content_guid <- content_info$guid
  out$content_access_type <- content_info$access_type
  
  return(out)
}
