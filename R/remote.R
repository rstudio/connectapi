#' Create a Remote User
#'
#' The remote user creation workflow involves authentication providers like LDAP
#' that involve a queryable identity store. This helper wraps the API calls
#' necessary to retrieve information about and then create such a user. It
#' functions with a "fuzzy match" `prefix` by default, but if you want to
#' instantiate users directly, you should set `exact = TRUE`.
#'
#' NOTE: there can be problems with usernames that are not unique. Please open
#' an issue if you run into any problems.
#'
#' @param connect An R6 Connect object.
#' @param prefix character. The prefix of the user name to search for.
#' @param expect number. Optional. The number of responses to expect for this search.
#' @param check boolean. Optional. Whether to check for local existence first.
#' @param exact boolean. Optional. Whether to only create users whose username
#'   exactly matches the provided `prefix`.
#'
#' @return The results of creating the users.
#'
#' @export
users_create_remote <- function(connect, prefix, expect = 1, check = TRUE, exact = FALSE) {
  expect <- as.integer(expect)
  if (check) {
    local_users <- get_users(connect, prefix = prefix)
    if (exact) {
      local_users <- local_users[local_users["username"] == prefix, ]
    }
    if (nrow(local_users) > 0) {
      if (!exact) {
        message(glue::glue("At least one user with username prefix '{prefix}' already exists"))
      } else {
        message(glue::glue("A user with username '{prefix}' already exists"))
      }
      return(local_users)
    }
  }

  remote_users <- connect$users_remote(prefix = prefix)
  remote_users_res <- remote_users[["results"]]
  if (exact) {
    remote_users_res <- purrr::keep(remote_users_res, ~ .x[["username"]] == prefix)
  }
  if (length(remote_users_res) != expect) {
    message(glue::glue("Found {length(remote_users_res)} remote users. Expected {expect}"))
    if (length(remote_users_res) > 0) {
      user_str <- toString(purrr::map_chr(remote_users_res, ~ .x[["username"]]))
      message(glue::glue("Users found: {user_str}"))
    }
    stop("The expected user(s) were not found. Please specify a more accurate 'prefix'")
  }
  user_creation <- purrr::map(
    remote_users_res,
    function(.x, src) {
      message(glue::glue("Creating remote user: {.x[['username']]}"))
      src$users_create_remote(temp_ticket = .x[["temp_ticket"]])
    },
    src = connect
  )
  message("Done creating remote users")
  return(user_creation)
}

#' Create a Remote Group
#'
#' @param connect An R6 Connect object.
#' @param prefix character. The prefix of the user name to search for.
#' @param expect number. Optional. The number of responses to expect for this search.
#' @param check boolean. Optional. Whether to check for local existence first.
#' @param exact boolean. Optional. Whether to only create groups whose name
#'   exactly matches the provided `prefix`.
#'
#' @return The results of creating the groups.
#'
#' @export
groups_create_remote <- function(connect, prefix, expect = 1, check = TRUE, exact = FALSE) {
  expect <- as.integer(expect)
  if (check) {
    # TODO: limit = 1 due to a paging bug in Posit Connect
    local_groups <- get_groups(connect, prefix = prefix, limit = 1)
    if (exact) {
      local_groups <- local_groups[local_groups["name"] == prefix, ]
    }
    if (nrow(local_groups) > 0) {
      if (!exact) {
        message(glue::glue("At least one group with name prefix '{prefix}' already exists"))
      } else {
        message(glue::glue("A group with the name '{prefix}' already exists"))

      }
      return(local_groups)
    }
  }

  remote_groups <- connect$groups_remote(prefix = prefix)
  remote_groups_res <- remote_groups[["results"]]
  if (exact) {
    remote_groups_res <- purrr::keep(remote_groups_res, ~ .x[["name"]] == prefix)
  }
  if (length(remote_groups_res) != expect) {
    message(glue::glue("Found {length(remote_groups_res)} remote groups. Expected {expect}"))
    if (length(remote_groups_res) > 0) {
      groups_found <- glue::glue_collapse(purrr::map_chr(remote_groups_res, ~ .x[["name"]]), sep = ", ")
      message(glue::glue("Groups found: {groups_found}"))
    }
    stop("The expected group(s) were not found. Please specify a more accurate 'prefix'")
  }
  group_creation <- purrr::map(
    remote_groups_res,
    function(.x, src) {
      message(glue::glue("Creating remote group: {.x[['name']]}"))
      src$groups_create_remote(temp_ticket = .x[["temp_ticket"]])
    },
    src = connect
  )
  message("Done creating remote groups")
  return(group_creation)
}
