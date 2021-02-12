# GET ACL ----------------------------------------------------

#' Get ACL Details
#'
#' \lifecycle{experimental} Retrieve the Access Controls associated with a given
#' piece of content.
#'
#' NOTE: ACLs can still be stored, even when access_type for content is "all" or
#' "logged_in" users. In these cases, granting or removing "viewer" privileges
#' have no effect.
#'
#' - `get_acl_user()` returns user ACLs
#' - `get_acl_group()` returns group ACLs
#' - `get_acl_user_role()` returns the "role" for a
#'    particular user on a piece of content
#' - `get_acl_group_role()` returns the "role" for a
#'    particular group on a piece of content
#'
#' `get_acl()` is deprecated.
#'
#' @param content [Content] An R6 Content item as returned from `content_item()`
#' @param user_guid [character] A user guid. Get user guids using `get_users()`
#' @param group_guid [character] A group guid. Get group guids using
#'   `get_groups()`
#'
#' @return A list of users/groups who have access to the content
#'
#' @family content functions
#' @export
#'
#' @rdname get_acl
get_acl_user <- function(content) {
  warn_experimental("get_acl")

  content_info <- content$get_content_remote()
  prep <- get_acl_user_impl(content)
  out <- parse_connectapi_typed(prep, !!!connectapi_ptypes$acl_user)
  out$content_guid <- content_info$guid
  out$content_access_type <- content_info$access_type

  return(out)
}

#' @rdname get_acl
#' @export
get_acl_group <- function(content) {
  warn_experimental("get_acl")

  content_info <- content$get_content_remote()
  prep <- get_acl_group_impl(content)
  out <- parse_connectapi_typed(prep, !!!connectapi_ptypes$acl_group)
  if (nrow(out) > 0) {
    out$content_guid <- content_info$guid
    out$content_access_type <- content_info$access_type
  }

  return(out)
}

#' @rdname get_acl
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
      glue::glue(
        "Content (guid: {content_info$guid}) has access type ",
        "{content_info$access_type}: ",
        "ACLs for viewers have no effect"
      ),
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

#' @rdname get_acl
#' @export
get_acl_user_role <- function(content, user_guid) {
  warn_experimental("acl_user_role")
  scoped_experimental_silence()
  acls <- get_acl_user_impl(content)
  if (is.null(user_guid) || is.na(user_guid)) {
    return(NULL)
  }
  user_entry <- purrr::flatten(purrr::keep(acls, ~ .x$guid == user_guid))

  return(user_entry$app_role)
}


#' @rdname get_acl
#' @export
get_acl_group_role <- function(content, group_guid) {
  warn_experimental("acl_group_role")
  scoped_experimental_silence()
  acls <- get_acl_group_impl(content)
  if (is.null(group_guid) || is.na(group_guid)) {
    return(NULL)
  }
  group_entry <- purrr::flatten(purrr::keep(acls, ~ .x$guid == group_guid))

  return(group_entry$app_role)
}

# ACL ADD ----------------------------------------------------

#' ACL Add Users or Groups
#'
#' Add a user or group to the content as an "owner" (collaborator) or "viewer"
#'
#' - `acl_add_user()` allows you to add ACL for a user and specify role
#' - `acl_add_group()` allows you to add ACL for a group and specify role
#'
#' - `acl_add_collaborator()` is a helper to add a `user` collaborators
#' - `acl_add_viewer()` is a helper to add a `user` viewer
#'
#' - `acl_remove_user()` removes a user's ACLs from a piece of content
#' - `acl_remove_group()` removes a group's ACLs from a piece of content
#'
#' - `acl_add_self()` is useful for admins and adds the current user as a collaborator
#' - `acl_remove_self()` removes the current user's ACLs from a piece of content
#'
#' @param content The R6 Content object (as returned by `content_item()`)
#' @param user_guid The user's GUID. Use `get_users()`
#' @param group_guid The group's GUID. Use `get_groups()`
#' @param role One of "owner" or "viewer"
#'
#' @return The R6 content object (for piping)
#'
#' @rdname acl_add
#' @family content functions
#' @export
acl_add_user <- function(content, user_guid, role) {
  warn_experimental("acl_add")
  stopifnot(role %in% c("owner", "viewer"))
  res <- content$get_connect()$POST(
    glue::glue("applications/{content$get_content()$guid}/users"),
    body = list(
      app_role = role,
      guid = user_guid
    )
  )
  return(content)
}

#' @rdname acl_add
#' @export
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


# TODO: Need a "group" variant? A way to "determine" which?
#' @rdname acl_add
#' @export
acl_add_collaborator <- function(content, user_guid) {
  acl_add_user(content = content, user_guid = user_guid, role = "owner")
}

# TODO: Should this be a warning if the user is a collaborator? Will downgrade
# their permissions
# TODO: How should this behave if the content does not have access_type: acl?
#' @rdname acl_add
#' @export
acl_add_viewer <- function(content, user_guid) {
  acl_add_user(content = content, user_guid = user_guid, role = "viewer")
}

#' @rdname acl_add
#' @export
acl_remove_user <- function(content, user_guid) {
  warn_experimental("acl_remove")
  res <- content$get_connect()$DELETE(
    glue::glue("applications/{content$get_content()$guid}/users/{user_guid}")
  )
  return(content)
}

#' @rdname acl_add
#' @export
acl_add_self <- function(content) {
  acl_add_collaborator(content, content$get_connect()$GET("me")$guid)
}

#' @rdname acl_add
#' @export
acl_remove_self <- function(content) {
  acl_remove_user(content, content$get_connect()$GET("me")$guid)
}

#' @rdname acl_add
#' @export
acl_remove_group <- function(content, group_guid) {
  warn_experimental("acl_remove")
  res <- content$get_connect()$DELETE(
    glue::glue("applications/{content$get_content()$guid}/groups/{group_guid}")
  )
  return(content)
}
