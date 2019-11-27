

#' Get user information from the RStudio Connect server
#' 
#' @param src The source object
#' @param page_size the number of records to return per page (max 500)
#' @param page_number the page number you wish to query
#' @param prefix Filters users by prefix (username, first name, or last name). 
#' The filter is case insensitive.
#' 
#' @examples 
#' \dontrun{
#'   library(connectapi)
#'   client <- connect()
#'   
#'   # get the first 20 users
#'   get_users(client, page_size = 20)
#'   
#'   # get the second twenty users
#'   get_users(client, page_size = 20, page_number = 2)
#' 
#' }
#' 
#' @export
get_users <- function(src, page_size = 20, page_number = 1, prefix = NULL){
  validate_R6_class("Connect", src)
  
  res <- src$users(
    page_size = page_size,
    page_number = page_number,
    prefix = prefix
  )
  
  ## TODO: Add a pagination option for users? This could be done using the 
  ## res$total value and changing the page_size and page_number arguments
  ## with a limit arguemnt. Then a while loop could be written similar 
  ## to page_cursor
  
  res <- res$results
  
  if (length(res) >= 400) {
    warning("The 'users' tbl_connect does not page and will return max 500 users")
  }
  
  out <- parse_connectapi(res)
  
  return(out)
}

#' Get group information from the RStudio Connect server
#' 
#' @param src The source object
#' @param page_size the number of records to return per page (max 500)
#' @param page_number the page number you wish to query
#' @param prefix Filters groups by prefix (group name). 
#' The filter is case insensitive.
#' 
#' @examples 
#' \dontrun{
#'   library(connectapi)
#'   client <- connect()
#'   
#'   # get the first 20 groups
#'   get_groups(client, page_size = 20)
#'   
#'   # get the second twenty groups
#'   get_groups(client, page_size = 20, page_number = 2)
#' 
#' }
#' 
#' @export
get_groups <- function(src, page_size = 20, page_number = 1, prefix = NULL){
  validate_R6_class("Connect", src)
  
  res <- src$groups(
    page_size = page_size,
    page_number = page_number,
    prefix = prefix
  )
  
  ## TODO: Add a pagination option for groups? This could be done using the 
  ## res$total value and changing the page_size and page_number arguments
  ## with a limit arguemnt. Then a while loop could be written similar 
  ## to page_cursor
  
  res <- res$results
  
  if (length(res) >= 400) {
    warning("The 'groups' tbl_connect does not page and will return max 500 users")
  }
}

#' Get information about content on the RStudio Connect server
#' 
#' @param src The source object
#' @param filter a named list of filter options, e.g. list(name = 'appname')
#' @param limit the maximum number of records to return
#' @param page_size the number of records to return per page
#' 
#' @examples
#' \dontrun{
#'   library(connectapi)
#'   client <- connect()
#'   
#'   get_content(client, limit = 20)
#' }
#' 
#' @export
get_content <- function(src, filter = NULL, limit = 25, page_size = 25){
  validate_R6_class("Connect", src)
  
  warn_experimental("'get_content'")
  
  ## TODO Add more arguments that can build the filter function for users
  ## so that they know explicitely what arguments that can pass
  
  res <- src$get_apps(
    filter = filter,
    .limit = limit,
    page_size = page_size
  )
  
  out <- parse_connectapi(res)
  
  return(out)
}


#' 
#' 
get_usage_shiny <- function(src, content_guid = NULL, 
                            min_data_version = NULL,
                            from = NULL,
                            to = NULL,
                            limit = 20,
                            previous = NULL,
                            nxt = NULL,
                            asc_order = TRUE){
  validate_R6_class("Connect", src)
  
  res <- src$inst_shiny_usage(
    content_guid = content_guid, 
    min_data_version = min_data_version, 
    from = from,
    to = to, 
    limit = limit, 
    previous = previous, 
    nxt = nxt,
    asc_order = asc_order
  )
  
  res <- page_cursor(src, res, limit = limit)
  
  out <- parse_connectapi(res)
  
  return(out)
}


get_usage_static <- function(src){
  validate_R6_class("Connect", src)
  
  res <- src$inst_content_visits()
  
  res <- page_cursor(src, res, limit = limit)
  
  out <- parse_connectapi(res)
  
  return(out)
}







parse_connectapi <- function(data){
  purrr::map_df(
    data, 
    function(x) {
      purrr::map(
        .x = x,
        .f = function(y) {
          prep <- purrr::pluck(y, .default = NA)
          # TODO: Should figure out what we want to do about sub-objects...
          # i.e. content: git details... could build a nested list...?
          if (length(prep) > 1)
            prep <- NA
          return(prep)
        }
      )
    }
  )
}
