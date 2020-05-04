# deploy the current_bundle
deploy_current <- function(content) {
  res <- content$get_connect()$POST(glue::glue("v1/experimental/content/{content$get_content()$guid}/deploy"), body = list(bundle_id = content$get_content()$bundle_id))
  return(Task$new(connect = content$get_connect(), content = content$get_content(), task = res$task_id))
}

# ACLs ----------------------------------------------------

#' ACL Add Group
#' 
#' Add a group_guid to the content as an owner or viewer
#' 
#' @param content The R6 Content object
#' @param group_guid The group's GUID
#' @param role One of "owner" or "viewer"
#' 
#' @return The R6 content object (for piping)
#' 
#' @keywords internal
acl_add_group <- function(content, group_guid, role) {
  warn_experimental("acl_add")
  res <- content$get_connect()$POST(
    glue::glue("applications/{content$get_content()$guid}/groups"),
    body = list(
      app_role = role,
      guid = group_guid
    )
  )
  
  return(content)
}

#' ACL Add User
#' 
#' Add a user_guid to the content as an owner or viewer
#' 
#' @param content The R6 Content object
#' @param user_guid The user's GUID
#' @param role One of "owner" or "viewer"
#' 
#' @return The R6 content object (for piping)
#' 
#' @keywords internal
acl_add_user <- function(content, user_guid, role) {
  warn_experimental("acl_add")
  res <- content$get_connect()$POST(
    glue::glue("applications/{content$get_content()$guid}/users"),
    body = list(
      app_role = role,
      guid = user_guid
    )
  )
  return(content)
}

#' @rdname acl_add_user
acl_add_collaborator <- function(content, user_guid) {
  acl_add_user(content = content, user_guid = user_guid, role = "owner")
}

# TODO: Should this be a warning if the user is a collaborator? Will downgrade their permissions
# TODO: How should this behave if the content does not have access_type: acl?
#' @rdname acl_add_user
acl_add_viewer <- function(content, user_guid) {
  acl_add_user(content = content, user_guid = user_guid, role = "viewer")
}

#' @rdname acl_add_user
acl_remove_user <- function(content, user_guid) {
  warn_experimental("acl_remove")
  res <- content$get_connect()$DELETE(
    glue::glue("applications/{content$get_content()$guid}/users/{user_guid}")
  )
  return(content)
}

#' @rdname acl_add_user
acl_remove_collaborator <- acl_remove_user

#' @rdname acl_add_user
acl_remove_viewer <- acl_remove_user

#' @rdname acl_add_user
acl_add_self <- function(content) {
  acl_add_collaborator(content, content$get_connect()$GET("me")$guid)
}

#' @rdname acl_add_user
acl_remove_self <- function(content) {
  acl_remove_user(content, content$get_connect()$GET("me")$guid)
}

acl_user_role <- function(content, user_guid) {
  warn_experimental("acl_user_role")
  scoped_experimental_silence()
  acls <- get_acl_user_impl(content)
  if (is.null(user_guid) || is.na(user_guid)) {
    return(NULL)
  }
  user_entry <- purrr::flatten(purrr::keep(acls, ~ .x$guid == user_guid))

  return(user_entry$app_role)
}
