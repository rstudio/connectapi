#' User
#'
#' Get user details
#'
#' `user_guid_from_username()` is a helper to retrieve a user GUID, given the
#' user's username. It is useful in Shiny applications for using `session$user`
#'
#' @param client A Connect R6 object
#' @param username The user's username
#'
#' @rdname user
#'
#' @export
user_guid_from_username <- function(client, username) {
  validate_R6_class(client, "Connect")

  user <- client$users(prefix = username)
  res <- user$results
  if (length(res) == 0) {
    stop("ERROR: user not found")
  } else if (length(res) > 1) {
    filt <- purrr::keep(res, ~ .x$username == username)
    if (length(filt) == 1) {
      return(filt[[1]]$guid)
    } else {
      warning("WARNING: multiple users found, but a unique exact match could not be found. Returning the first")
      return(res[[1]]$guid)
    }
  } else {
    return(res[[1]]$guid)
  }
}
