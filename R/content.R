#' Get ACL Details
#' 
#' \lifecycle{experimental} Retrieve the Access Controls
#' associated with a given piece of content.
#' 
#' @param content [Content] An R6 Content item
#' 
#' @return A list of users who have access to the content
#' @export
# TODO: How should we behave if this content is open to the server / world?
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
