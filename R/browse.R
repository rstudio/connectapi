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
  url <- content$get_content()$url
  utils::browseURL(url)
  return(url)
}

#' @rdname browse
#' @export
browse_dashboard <- function(content) {
  validate_R6_class(content, "Content")
  url <- content$get_dashboard_url()
  utils::browseURL(url)
  return(url)
}

#' @rdname browse
#' @export
browse_api_docs <- function(connect) {
  validate_R6_class(connect, "Connect")
  url <- connect$docs("api", browse = FALSE)
  utils::browseURL(url)
  return(url)
}
