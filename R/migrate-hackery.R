# migration hackery



# deploy the current_bundle
#' @export
deploy_current <- function(content) {
  res <- content$get_connect()$POST(glue::glue("v1/experimental/content/{content$get_content()$guid}/deploy"), body = list(bundle_id = content$get_content()$bundle_id))
  return(Task$new(connect = content$get_connect(), content = content$get_content(), task = res$task_id))
}

# because an admin cannot deploy...
#' @export
add_self <- function(content) {
  res <- content$get_connect()$POST(
    glue::glue("applications/{content$get_content()$guid}/users"),
    body = list(
      app_role = "owner",
      guid = content$get_connect()$GET("me")$guid
    )
  )
  return(content)
}

#' @export
remove_self <- function(content) {
  res <- content$get_connect()$DELETE(
    glue::glue("applications/{content$get_content()$guid}/users/{content$get_connect()$GET('me')$guid}")
  )
  return(content)
}
