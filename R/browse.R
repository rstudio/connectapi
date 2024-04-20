#' Browse
#'
#' Browse to different locations on Connect via
#' utils::browseURL
#'
#' @param content A R6 Content object
#' @param connect A R6 Connect object
#'
#' @return The url that is opened in the browser
#'
#' @rdname browse
#' @export
browse_solo <- function(content) {
  validate_R6_class(content, "Content")
  url <- content$get_url()
  browse_url(url)
}

#' @rdname browse
#' @export
browse_dashboard <- function(content) {
  validate_R6_class(content, "Content")
  url <- content$get_dashboard_url()
  browse_url(url)
}

#' @rdname browse
#' @export
browse_api_docs <- function(connect) {
  validate_R6_class(connect, "Connect")
  url <- connect$docs("api", browse = FALSE)
  browse_url(url)
}

#' @rdname browse
#' @export
browse_connect <- function(connect) {
  validate_R6_class(connect, "Connect")
  url <- connect$server
  browse_url(url)
}

#' @importFrom utils browseURL
browse_url <- function(url) {
  browseURL(url)
  invisible(url)
}
