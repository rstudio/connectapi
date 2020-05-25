#' Create a Remote User
#' 
#' @param connect A R6 Connect object
#' @param prefix character. The prefix of the user name to search for
#' @param expect number. The number of responses to expect for this search
#' @param check boolean. Whether to check for local existence first
#' 
#' @return The results of creating the users
#'
#' @export
users_create_remote <- function(connect, prefix, expect = 1, check = TRUE) {
  expect <- as.integer(expect)
  if (check && expect > 1) {
    stop(glue::glue("expect > 1 is not tested. Please set expect = 1, and specify a more narrow 'prefix'. You provided: expect={expect}"))
  }
  if (check) {
    local_users <- get_users(connect, prefix = prefix)
    if (nrow(local_users) > 0) {
      message(glue::glue("At least one user with username prefix '{prefix}' already exists"))
      return(local_users)
    }
  }
  
  remote_users <- connect$users_remote(prefix = prefix)
  if (remote_users$total != expect) {
    message(glue::glue("Found {remote_users$total} remote users. Expected {expect}"))
    if (remote_users$total > 0) {
      message(glue::glue("Users found: {glue::glue_collapse(purrr::map_chr(remote_users$results, ~ .x$username), sep = \", \")"))
    }
    stop("The expected user(s) were not found. Please specify a more accurate 'prefix'")
  }
  user_creation <- purrr::map(
    remote_users$results,
    function(.x, src) {
      message(glue::glue("Creating remote user: {.x[['username']]}"))
      src$users_create_remote(temp_ticket = .x[["temp_ticket"]])
    },
    src = connect 
  )
  message("Done creating remote users")
  return(get_users(connect, prefix = prefix))
}

#' Create a Remote Group
#' 
#' @param connect A R6 Connect object
#' @param prefix character. The prefix of the group name to search for
#' @param expect number. The number of responses to expect for this search
#' @param check boolean. Whether to check for local existence first
#' 
#' @return The results of creating the groups
#'
#' @export
groups_create_remote <- function(connect, prefix, expect = 1, check = TRUE) {
  expect <- as.integer(expect)
  if (check && expect > 1) {
    stop(glue::glue("expect > 1 is not tested. Please set expect = 1, and specify a more narrow 'prefix'. You provided: expect={expect}"))
  }
  if (check) {
    # TODO: limit = 1 due to a paging bug in RStudio Connect
    local_groups <- get_groups(connect, prefix = prefix, limit = 1)
    if (nrow(local_groups) > 0) {
      message(glue::glue("At least one group with name prefix '{prefix}' already exists"))
      return(local_groups)
    }
  }
  
  remote_groups <- connect$groups_remote(prefix = prefix)
  if (remote_groups$total != expect) {
    message(glue::glue("Found {remote_groups$total} remote groups. Expected {expect}"))
    if (remote_groups$total > 0) {
      message(glue::glue("Groups found: {glue::glue_collapse(purrr::map_chr(remote_groups$results, ~ .x$name), sep = \", \")"))
    }
    stop("The expected group(s) were not found. Please specify a more accurate 'prefix'")
  }
  group_creation <- purrr::map(
    remote_groups$results,
    function(.x, src) {
      message(glue::glue("Creating remote group: {.x[['name']]}"))
      src$groups_create_remote(temp_ticket = .x[["temp_ticket"]])
    },
    src = connect 
  )
  message("Done creating remote groups")
  return(get_groups(connect, prefix = prefix, limit = 1))
}
