

#' Get user information from the RStudio Connect server
#' 
#' @param src The source object
#' @param page_size the number of records to return per page (max 500)
#' @param page_number the page number you wish to query
#' @param prefix Filters users by prefix (username, first name, or last name). 
#' The filter is case insensitive.
#' 
#' @details 
#' Please see https://docs.rstudio.com/connect/api/#getUsers for more information 
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
#' @details 
#' Please see https://docs.rstudio.com/connect/api/#getGroups for more information 
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
#' @return 
#' A tibble with the following columns:
#' \itemize{
#'   \item{\strong{access_type}}{Access type describes how this content manages
#'    its viewers. The value all is the most permissive; any visitor to RStudio
#'    Connect will be able to view this content. The value logged_in indicates
#'    that all RStudio Connect accounts may view the content. The acl value 
#'    lets specifically enumerated users and groups view the content. Users 
#'    configured as collaborators may always view content. It may have a 
#'    value of all, logged_in or acl.}
#'    \item{\strong{app_mode}}{The runtime model for this content. Has a value 
#'    of unknown before data is deployed to this item. Automatically assigned 
#'    upon the first successful bundle deployment.}
#'    \item{\strong{bundle_id}}{The identifier for the active deployment bundle. 
#'    Automatically assigned upon the successful deployment of that bundle.}
#'    \item{\strong{connection_timeout}}{Maximum number of seconds allowed 
#'    without data sent or received across a client connection. A value of 0 
#'    means connections will never time-out (not recommended). When null, the 
#'    default Scheduler.ConnectionTimeout is used. Applies only to content 
#'    types that are executed on demand.}
#'    \item{\strong{content_category}}{Describes the specialization of the content 
#'    runtime model. Automatically assigned upon the first successful bundle 
#'    deployment.}
#'    \item{\strong{created_time}}{The timestamp (RFC3339) indicating when this 
#'    content was created.}
#'    \item{\strong{description}}{A rich description of this content}
#'    \item{\strong{guid}}{The unique identifier of this content item.}
#'    \item{\strong{has_parameters}}{True when R Markdown rendered content 
#'    allows parameter configuration. Automatically assigned upon the first 
#'    successful bundle deployment. Applies only to content with an app_mode
#'    of rmd-static.}
#'    \item{\strong{idle_timeout}}{The maximum number of seconds a worker process 
#'    for an interactive application to remain alive after it goes idle (no 
#'    active connections). When null, the default Scheduler.IdleTimeout is used. 
#'    Applies only to content types that are executed on demand.}
#'    \item{\strong{init_timeout}}{The maximum number of seconds allowed for an 
#'    interactive application to start. RStudio Connect must be able to connect 
#'    to a newly launched Shiny application, for example, before this threshold
#'    has elapsed. When null, the default Scheduler.InitTimeout is used. Applies
#'    only to content types that are executed on demand.}
#'    \item{\strong{}}
#' }
#' 
#' @details 
#' Please see https://docs.rstudio.com/connect/api/#getContent for more information 
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


#' Get usage information for deployed shiny applications
#' 
#' @param src the source object
#' @param content_guid Filter results by content GUID
#' @param min_data_version Filter by data version. Records with a data version
#' lower than the given value will be excluded from the set of results.
#' @param from The timestamp that starts the time window of interest. Any usage
#' information that ends prior to this timestamp will not be returned. 
#' Individual records may contain a starting time that is before this if they 
#' end after it or have not finished. Must be of class Date or POSIX
#' @param to The timestamp that ends the time window of interest. Any usage 
#' information that starts after this timestamp will not be returned. 
#' Individual records may contain an ending time that is after this 
#' (or no ending time) if they start before it. Must be of class Date or
#' POSIX
#' @param limit The number of records to return. 
#' @param previous Retrieve the previous page of Shiny application usage 
#' logs relative to the provided value. This value corresponds to an internal 
#' reference within the server and should be sourced from the appropriate 
#' attribute within the paging object of a previous response.
#' @param next Retrieve the next page of Shiny application usage logs 
#' relative to the provided value. This value corresponds to an internal 
#' reference within the server and should be sourced from the appropriate 
#' attribute within the paging object of a previous response.
#' @param asc_order Defaults to TRUE; Determines if the response records 
#' should be listed in ascending or descending order within the response. 
#' Ordering is by the started timestamp field.
#' 
#' 
#' @return 
#' A tibble with the following columns:
#' \itemize{
#'   \item{\strong{content_guid}}{The GUID, in RFC4122 format, of the Shiny application this information pertains to.}
#'   \item{\strong{user_guid}}{The GUID, in RFC4122 format, of the user that visited the application.}
#'   \item{\strong{started}}{The timestamp, in RFC3339 format, when the user opened the application.}
#'   \item{\strong{ended}}{The timestamp, in RFC3339 format, when the user left the application.}
#'   \item{\strong{data_version}}{The data version the record was recorded with.}
#' }
#' 
#' @details 
#' Please see https://docs.rstudio.com/connect/api/#getShinyAppUsage for more information 
#' 
#' @examples 
#' \dontrun{
#'   library(connectapi)
#'   client <- connect()
#'   
#'   from <- Sys.Date() - lubridate::days(5)
#'   get_usage_shiny(client, limit = 20, from = from)
#' }
#' 
#' @export
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

#'
#'
#'
#'
#' @details 
#' Please see https://docs.rstudio.com/connect/api/#getContentVisits for more information 
#'
#'
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
