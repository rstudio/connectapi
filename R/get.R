#' Get user information from the Posit Connect server
#'
#' @param src The source object
#' @param page_size the number of records to return per page (max 500)
#' @param prefix Filters users by prefix (username, first name, or last name).
#' The filter is case insensitive.
#' @param limit The max number of records to return
#' @param user_role Optionally filter by user role ("administrator",
#' "publisher", "viewer"). Pass in a vector of multiple roles to match any value
#' (boolean OR). When `NULL` (the default), results are not filtered.
#' @param account_status Optionally filter by account status ("locked",
#' "licensed", "inactive"). Pass a vector of multiple statuses to match any
#' value (boolean OR). When `NULL` (the default), results are not filtered.

#'
#' @return
#' A tibble with the following columns:
#'
#'   * `email`: The user's email
#'   * `username`: The user's username
#'   * `first_name`: The user's first name
#'   * `last_name`: The user's last name
#'   * `user_role`: The user's role. It may have a value of administrator,
#'     publisher or viewer.
#'   * `created_time`: The timestamp (in RFC3339 format) when the user was
#'     created in the Posit Connect server
#'   * `updated_time`: The timestamp (in RFC3339 format) when the user was last
#'     updated in the Posit Connect server
#'   * `active_time`: The timestamp (in RFC3339 format) when the user was last
#'     active on the Posit Connect server
#'   * `confirmed`: When false, the created user must confirm their account
#'     through an email. This feature is unique to password authentication.
#'   * `locked`: Whether or not the user is locked
#'   * `guid`: The user's GUID, or unique identifier, in UUID RFC4122 format
#'
#' @details
#' Please see https://docs.posit.co/connect/api/#get-/v1/users for more information.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#'
#' # Get all users
#' get_users(client)
#'
#' # Get all licensed users
#' get_users(client, account_status = "licensed")
#'
#' # Get all users who are administrators or publishers
#' get_users(client, user_role = c("administrator", "publisher"))
#' }
#'
#' @export
get_users <- function(
  src,
  page_size = 500,
  prefix = NULL,
  limit = Inf,
  user_role = NULL,
  account_status = NULL
) {
  validate_R6_class(src, "Connect")

  res <- page_offset(
    src,
    src$users(
      page_size = page_size,
      prefix = prefix,
      user_role = user_role,
      account_status = account_status
    ),
    limit = limit
  )

  out <- parse_connectapi_typed(
    res,
    datetime_cols = datetime_columns$users
  )

  return(out)
}

#' Get information about content on the Posit Connect server
#'
#' @param src A Connect object
#' @param guid The guid for a particular content item
#' @param owner_guid The unique identifier of the user who owns the content
#' @param name The content name specified when the content was created
#' @param ... Extra arguments. Currently not used.
#' @param .p Optional. A predicate function, passed as-is to `purrr::keep()`
#'   before turning the response into a tibble. Can be useful for performance.
#'
#' @return
#' A tibble with the following columns:
#'   * `guid`: The unique identifier of this content item.
#'   * `name`: A simple, URL-friendly identifier. Allows alpha-numeric
#'     characters, hyphens ("-"), and underscores ("_").
#'   * `title`: The title of this content.
#'   * `description`: A rich description of this content
#'   * `access_type`: Access type describes how this content manages its
#'     viewers. It may have a value of `all`, `logged_in` or `acl`.
#'     The value `all` is the most permissive; any visitor to Posit
#'     Connect will be able to view this content. The value `logged_in`
#'     indicates that all Posit Connect accounts may view the content. The
#'     `acl` value lets specifically enumerated users and groups view the
#'     content. Users configured as collaborators may always view content.
#'   * `connection_timeout`: Maximum number of seconds allowed without data
#'     sent or received across a client connection. A value of 0 means
#'     connections will never time-out (not recommended). When null, the
#'     default `Scheduler.ConnectionTimeout` is used. Applies only to content
#'     types that are executed on demand.
#'   * `read_timeout`: Maximum number of seconds allowed without data received
#'     from a client connection. A value of 0 means a lack of client (browser)
#'     interaction never causes the connection to close. When null, the default
#'     `Scheduler.ReadTimeout` is used. Applies only to content types that are
#'     executed on demand.
#'   * `init_timeout`: The maximum number of seconds allowed for an interactive
#'     application to start. Posit Connect must be able to connect
#'     to a newly launched Shiny application, for example, before this threshold
#'     has elapsed. When null, the default `Scheduler.InitTimeout` is
#'     used. Applies only to content types that are executed on demand.
#'   * `idle_timeout`: The maximum number of seconds a worker process
#'     for an interactive application to remain alive after it goes idle (no
#'     active connections). When null, the default `Scheduler.IdleTimeout`
#'     is used. Applies only to content types that are executed on demand.
#'   * `max_processes`: Specifies the total number of concurrent processes
#'     allowed for a single interactive application. When null, the
#'     default `Scheduler.MaxProcesses` setting is used. Applies only to
#'     content types that are executed on demand.
#'   * `min_processes`: Specifies the minimum number of concurrent
#'     processes allowed for a single interactive application. When null, the
#'     default `Scheduler.MinProcesses` is used. Applies only to content types
#'    that are executed on demand.
#'   * `max_conns_per_process`: Specifies the maximum number of
#'     client connections allowed to an individual process. Incoming connections
#'     which will exceed this limit are routed to a new process or rejected.
#'     When null, the default `Scheduler.MaxConnsPerProcess` is used. Applies
#'     only to content types that are executed on demand.
#'   * `load_factor`: Controls how aggressively new processes are spawned.
#'     When null, the default `Scheduler.LoadFactor` is used. Applies only to
#'     content types that are executed on demand.
#'   * `created_time`: The timestamp (RFC3339) indicating when this
#'     content was created.
#'   * `last_deployed_time`: The timestamp (RFC3339) indicating when
#'     this content last had a successful bundle deployment performed.
#'   * `bundle_id`: The identifier for the active deployment bundle.
#'     Automatically assigned upon the successful deployment of that bundle.
#'   * `app_mode`: The runtime model for this content. Has a value
#'     of `unknown` before data is deployed to this item. Automatically assigned
#'     upon the first successful bundle deployment. Allowed: `api`,
#'     `jupyter-static`, `python-api`, `python-bokeh`, `python-dash`,
#'     `python-streamlit`, `rmd-shiny`, `rmd-static`, `shiny`, `static`,
#'     `tensorflow-saved-model`, `unknown`.
#'   * `content_category`: Describes the specialization of the content
#'     runtime model. Automatically assigned upon the first successful bundle
#'     deployment.
#'   * `parameterized`: True when R Markdown rendered content
#'     allows parameter configuration. Automatically assigned upon the first
#'     successful bundle deployment. Applies only to content with an app_mode
#'     of rmd-static.
#'   * `r_version`: The version of the R interpreter associated
#'     with this content. The value null represents that an R interpreter is
#'     not used by this content or that the R package environment has not been
#'     successfully restored. Automatically assigned upon the successful
#'     deployment of a bundle.
#'   * `py_version`: The version of the Python interpreter
#'     associated with this content. The value null represents that a Python
#'     interpreter is not used by this content or that the Python package
#'     environment has not been successfully restored. Automatically assigned
#'     upon the successful deployment of a bundle.
#'   * `run_as`: The UNIX user that executes this content.
#'     When null, the default Applications.RunAs is used. Applies
#'     only to executable content types - not static.
#'   * `run_as_current_user`: Indicates if this content is allowed
#'     to execute as the logged-in user when using PAM authentication.
#'     Applies only to executable content types - not static.
#'   * `owner_guid`: The unique identifier for the owner
#'   * `content_url`: The URL associated with this content. Computed
#'     from the GUID for this content.
#'   * `dashboard_url`: The URL within the Connect dashboard where
#'     this content can be configured. Computed from the GUID for this content.
#'   * `role`: The relationship of the accessing user to this
#'     content. A value of owner is returned for the content owner. editor
#'     indicates a collaborator. The viewer value is given to users who are
#'     permitted to view the content. A none role is returned for
#'     administrators who cannot view the content but are permitted to view
#'     its configuration. Computed at the time of the request.
#'   * `vanity_url`: The vanity URL associated with this content item.
#'   * `id`: The internal numeric identifier of this content item.
#'   * `tags`: Tags associated with this content item. Each entry is a list
#'     with the following fields:
#'     * `id`: The identifier for the tag.
#'     * `name`: The name of the tag.
#'     * `parent_id`: The identifier for the parent tag. Null if the tag is a
#'       top-level tag.
#'     * `created_time`: The timestamp (RFC3339) indicating when the tag was
#'       created.
#'     * `updated_time`: The timestamp (RFC3339) indicating when the tag was
#'       last updated.
#'   * `owner`: Basic details about the owner of this content item. Each entry
#'     is a list with the following fields:
#'     * `guid`: The user's GUID, or unique identifier, in UUID RFC4122 format.
#'     * `username`: The user's username.
#'     * `first_name`: The user's first name.
#'     * `last_name`: The user's last name.
#'
#' @details
#' Please see https://docs.posit.co/connect/api/#get-/v1/content for more
#' information.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#'
#' get_content(client)
#' }
#'
#' @export
get_content <- function(
  src,
  guid = NULL,
  owner_guid = NULL,
  name = NULL,
  ...,
  .p = NULL
) {
  validate_R6_class(src, "Connect")

  # The capability to return vanity URLs `vanity_url` was added in Connect
  # v2024.06.0.
  if (compare_connect_version(src$version, "2024.06.0") < 0) {
    include <- "tags,owner"
  } else {
    include <- "tags,owner,vanity_url"
  }

  res <- src$content(
    guid = guid,
    owner_guid = owner_guid,
    name = name,
    include = include
  )

  if (!is.null(guid)) {
    # convert a single item to a list
    res <- list(res)
  }

  if (!is.null(.p)) {
    res <- res %>% purrr::keep(.p = .p)
  }

  out <- parse_connectapi_typed(
    res,
    datetime_cols = datetime_columns$content
  )

  return(out)
}


#' Get Content List with Permissions
#'
#' `r lifecycle::badge('experimental')` These functions are experimental placeholders until the API supports
#' this behavior.
#'
#' `content_list_with_permissions` loops through content and retrieves
#' permissions for each item (with a progress bar). This can take a long time
#' for lots of content! Make sure to use the optional `.p` argument as a predicate
#' function that filters the content list before it is transformed.
#'
#' `content_list_guid_has_access` works with a `content_list_with_permissions`
#' dataset by checking whether a given GUID (either user or group) has access to
#' the content by:
#' - checking if the content has access_type == "all"
#' - checking if the content has access_type == "logged_in"
#' - checking if the provided guid is the content owner
#' - checking if the provided guid is in the list of content permissions (in the "permissions" column)
#'
#' @param src A Connect R6 object
#' @param ... Extra arguments. Currently not used
#' @param .p Optional. A predicate function, passed as-is to `purrr::keep()`. See
#'   `get_content()` for more details. Can greatly help performance by reducing
#'   how many items to get permissions for
#' @param content_list A "content list with permissions" as returned by `content_list_with_permissions()`
#' @param guid A user or group GUID to filter the content list by whether they have access
#'
#' @rdname content_list_with_permissions
#'
#' @export
content_list_with_permissions <- function(src, ..., .p = NULL) {
  warn_experimental("content_list_with_permissions")

  message("Getting content list")
  content_list <- get_content(src, .p = .p)

  message("Getting permission list")
  pb <- optional_progress_bar(
    total = nrow(content_list),
    format = "[:bar] :percent :eta"
  )
  content_list[["permission"]] <- purrr::pmap(
    content_list,
    function(...) {
      pb$tick()
      get_content_permissions(Content$new(connect = src, content = list(...)))
    }
  )

  content_list
}

#' Content List
#'
#' `r lifecycle::badge('experimental')` Get a content list
#'
#' `content_list_by_tag()` retrieves a content list by tag
#'
#' @param src An R6 Connect object
#' @param tag A `connect_tag_tree` object or tag ID
#'
#' @rdname content_list
#' @export
content_list_by_tag <- function(src, tag) {
  validate_R6_class(src, "Connect")
  tag_id <- .get_tag_id(tag)

  res <- src$GET(v1_url("tags", tag_id, "content"))

  out <- parse_connectapi_typed(
    res,
    datetime_cols = datetime_columns$content
  )
  return(out)
}

#' @rdname content_list_with_permissions
#' @export
content_list_guid_has_access <- function(content_list, guid) {
  warn_experimental("content_list_filter_by_guid")
  rows_keep <- content_list$access_type %in%
    c("all", "logged_in") |
    content_list$owner_guid == guid |
    purrr::map_lgl(content_list$permission, ~ guid %in% .x$principal_guid)
  content_list[rows_keep, ]
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
#' @param nxt Retrieve the next page of Shiny application usage logs
#' relative to the provided value. This value corresponds to an internal
#' reference within the server and should be sourced from the appropriate
#' attribute within the paging object of a previous response.
#' @param asc_order Defaults to TRUE; Determines if the response records
#' should be listed in ascending or descending order within the response.
#' Ordering is by the started timestamp field.
#'
#' @return
#' A tibble with the following columns:
#'
#'   * `content_guid`: The GUID, in RFC4122 format, of the
#'     Shiny application this information pertains to.
#'   * `user_guid`: The GUID, in RFC4122 format, of the user
#'     that visited the application.
#'   * `started`: The timestamp, in RFC3339 format, when the
#'     user opened the application.
#'   * `ended`: The timestamp, in RFC3339 format, when the
#'     user left the application.
#'   * `data_version`: The data version the record was recorded
#'     with. The Shiny Application Events section of the Posit Connect Admin
#'     Guide explains how to interpret data_version values.
#'
#' @details
#' Please see https://docs.posit.co/connect/api/#get-/v1/instrumentation/shiny/usage
#' for more information.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#'
#' from <- Sys.Date() - lubridate::days(5)
#' get_usage_shiny(client, limit = 20, from = from)
#' }
#'
#' @export
get_usage_shiny <- function(
  src,
  content_guid = NULL,
  min_data_version = NULL,
  from = NULL,
  to = NULL,
  limit = 500,
  previous = NULL,
  nxt = NULL,
  asc_order = TRUE
) {
  validate_R6_class(src, "Connect")

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

  out <- parse_connectapi_typed(res, datetime_cols = datetime_columns$usage_shiny)

  return(out)
}

#' Get usage information from deployed static content
#'
#' This function retrieves usage information from static content
#' on the Posit Connect server (e.g. Rmarkdown, Jupyter Notebooks)
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
#' @param nxt Retrieve the next page of Shiny application usage logs
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
#'
#'   * `content_guid`: The GUID, in RFC4122 format, of the Shiny
#'     application this information pertains to.
#'   * `user_guid`: The GUID, in RFC4122 format, of the user that
#'     visited the application.
#'   * `variant_key`: The key of the variant the user visited.
#'     This will be null for static content.
#'   * `time`: The timestamp, in RFC3339 format, when the user
#'     visited the content.
#'   * `rendering_id`: The ID of the rendering the user visited.
#'     This will be null for static content.
#'   * `bundle_id`: The ID of the particular bundle used.
#'   * `data_version`: The data version the record was recorded
#'     with. The Rendered and Static Content Visit Events section of the
#'     Posit Connect Admin Guide explains how to interpret data_version
#'     values.
#'
#' @details
#' Please see https://docs.posit.co/connect/api/#get-/v1/instrumentation/content/visits
#' for more information.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#'
#' from <- Sys.Date() - lubridate::days(5)
#' get_usage_static(client, limit = 20, from = from)
#' }
#'
#' @export
get_usage_static <- function(
  src,
  content_guid = NULL,
  min_data_version = NULL,
  from = NULL,
  to = NULL,
  limit = 500,
  previous = NULL,
  nxt = NULL,
  asc_order = TRUE
) {
  validate_R6_class(src, "Connect")

  res <- src$inst_content_visits(
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

  out <- parse_connectapi_typed(res, datetime_cols = datetime_columns$usage_static)

  return(out)
}

#' Get usage information for deployed content
#'
#' @description
#' Retrieve content hits for all available content on the server. Available
#' content depends on the user whose API key is in use. Administrator accounts
#' will receive data for all content on the server. Publishers will receive data
#' for all content they own or collaborate on.
#'
#' If no date-times are provided, all usage data will be returned.
#'
#' @param client A `Connect` R6 client object.
#' @param content_guid Optional. A single content GUID or a character vector of
#'   GUIDs to filter results. When multiple GUIDs are provided they are
#'   collapsed with `"|"`.
#' @param from Optional date-time (`POSIXct` or `POSIXlt`). Only
#'   records after this time are returned. If not provided, records
#'   are returned back to the first record available.
#' @param to Optional date-time (`POSIXct` or `POSIXlt`). Only records
#'   before this time are returned. If not provided, all records up to
#'   the most recent are returned.
#'
#' @return A list of usage records. Each record is a list with all elements
#'   as character strings unless otherwise specified.
#'
#' * `id`: A string identifier for the hit.
#' * `user_guid`: The user GUID if the visitor is logged-in, `NULL` for
#'   anonymous hits.
#' * `content_guid`: The GUID of the visited content.
#' * `timestamp`: The time of the hit in RFC3339 format.
#' * `data`: A nested list with optional fields:
#'     * `path`: The request path (if recorded).
#'     * `user_agent`: The user agent string (if available).
#'
#' Use [as.data.frame()] or [tibble::as_tibble()] to convert to a flat
#' table with parsed types. In the resulting data frame:
#'
#' * `timestamp` is parsed to `POSIXct`.
#' * `path` and `user_agent` are extracted from the nested `data` field.
#'
#' By default, [as.data.frame()] attempts to extract the nested fields using
#' the \pkg{tidyr} package. If \pkg{tidyr} is not available, or if you want to
#' skip unnesting, call `as.data.frame(x, unnest = FALSE)` to leave `data` as
#' a list-column.
#'
#' @details
#'
#' The data returned by `get_usage()` includes all content types. For Shiny
#' content, the `timestamp` indicates the *start* of the Shiny session.
#' Additional fields for Shiny and non-Shiny are available respectively from
#' [get_usage_shiny()] and [get_usage_static()]. `get_usage_shiny()` includes a
#' field for the session end time; `get_usage_static()` includes variant,
#' rendering, and bundle identifiers for the visited content.
#'
#' When possible, however, we recommend using `get_usage()` over
#' `get_usage_static()` or `get_usage_shiny()`, as it is faster and more efficient.
#'
#' @seealso [as.data.frame.connect_list_hits()], [as_tibble.connect_list_hits()]
#'
#' @examples
#' \dontrun{
#' client <- connect()
#'
#' # Fetch the last 2 days of hits
#' usage <- get_usage(client, from = Sys.Date() - 2, to = Sys.Date())
#'
#' # Fetch usage after a specified date and convert to a data frame.
#' usage <- get_usage(
#'   client,
#'   from = as.POSIXct("2025-05-02 12:40:00", tz = "UTC")
#' )
#'
#' # Fetch usage for a specific content item
#' usage <- get_usage(client, content_guid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
#'
#' # Fetch all usage
#' usage <- get_usage(client)
#'
#' # Convert to tibble or data frame
#' usage_df <- tibble::as_tibble(usage)
#'
#' # Skip unnesting if tidyr is not installed
#' usage_df <- as.data.frame(usage, unnest = FALSE)
#' }
#'
#' @export
get_usage <- function(client, content_guid = NULL, from = NULL, to = NULL) {
  error_if_less_than(client$version, "2025.04.0")

  if (!is.null(content_guid)) {
    error_if_less_than(client$version, "2026.01.0")
    if (length(content_guid) > 1) {
      content_guid <- paste0(content_guid, collapse = "|")
    }
  }

  usage <- client$GET(
    v1_url("instrumentation", "content", "hits"),
    query = list(
      content_guid = content_guid,
      from = make_timestamp(from),
      to = make_timestamp(to)
    )
  )

  class(usage) <- c("connect_list_hits", class(usage))
  usage
}

#' Convert usage data to a data frame
#'
#' @description
#' Converts an object returned by [get_usage()] into a data frame with parsed
#' column types. By default, extracts `path` and `user_agent` from the `data`
#' field, if available.
#'
#' @param x A `connect_list_hits` object (from [get_usage()]).
#' @param row.names Passed to [base::as.data.frame()].
#' @param optional Passed to [base::as.data.frame()].
#' @param ... Passed to [base::as.data.frame()].
#' @param unnest Logical; if `TRUE` (default), extracts nested fields using
#'   \pkg{tidyr}. Set to `FALSE` to skip unnesting.
#'
#' @return A `data.frame` with one row per usage record.
#' @export
#' @method as.data.frame connect_list_hits
as.data.frame.connect_list_hits <- function(
  x,
  row.names = NULL, # nolint
  optional = FALSE,
  ...,
  unnest = TRUE
) {
  usage_df <- parse_connectapi_typed(x, datetime_cols = "timestamp")
  if (unnest) {
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop(
        "`unnest = TRUE` requires tidyr. Install tidyr or set `unnest = FALSE`.",
        call. = FALSE
      )
    }
    usage_df <- tidyr::unnest_wider(
      usage_df,
      "data",
      ptype = list(path = character(0), user_agent = character(0))
    )
  }
  as.data.frame(usage_df, row.names = row.names, optional = optional, ...)
}

#' Convert usage data to a tibble
#'
#' @description
#' Converts an object returned by [get_usage()] to a tibble via
#' [as.data.frame.connect_list_hits()].
#'
#' @param x A `connect_list_hits` object.
#' @param ... Passed to [as.data.frame()].
#'
#' @return A tibble with one row per usage record.
#' @export
#' @importFrom tibble as_tibble
#' @method as_tibble connect_list_hits
as_tibble.connect_list_hits <- function(x, ...) {
  tibble::as_tibble(as.data.frame(x, ...))
}

#' Get Audit Logs from Posit Connect Server
#'
#' @param src The source object
#' @param limit The number of records to return.
#' @param previous Retrieve the previous page of Shiny application usage
#' logs relative to the provided value. This value corresponds to an internal
#' reference within the server and should be sourced from the appropriate
#' attribute within the paging object of a previous response.
#' @param nxt Retrieve the next page of Shiny application usage logs
#' relative to the provided value. This value corresponds to an internal
#' reference within the server and should be sourced from the appropriate
#' attribute within the paging object of a previous response.
#' @param asc_order Defaults to TRUE; Determines if the response records
#' should be listed in ascending or descending order within the response.
#' Ordering is by the started timestamp field.
#'
#' @return
#' A tibble with the following columns:
#'
#'   * `id`: ID of the audit action
#'   * `time`: Timestamp in RFC3339 format when action was taken
#'   * `user_id`: User ID of the actor who made the audit action
#'   * `user_description`: Description of the actor
#'   * `action`: Audit action taken
#'   * `event_description`: Description of action
#'
#' @details
#' Please see https://docs.posit.co/connect/api/#get-/v1/audit_logs for more
#' information.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#'
#' # get the last 20 audit logs
#' get_audit_logs(client, limit = 20, asc_order = FALSE)
#' }
#'
#' @export
get_audit_logs <- function(
  src,
  limit = 500,
  previous = NULL,
  nxt = NULL,
  asc_order = TRUE
) {
  validate_R6_class(src, "Connect")

  res <- src$audit_logs(
    limit = limit,
    previous = previous,
    nxt = nxt,
    asc_order = asc_order
  )

  res <- page_cursor(src, res, limit = limit)

  out <- parse_connectapi_typed(res, datetime_cols = datetime_columns$audit_logs)

  return(out)
}

#' Get Real-Time Process Data
#'
#' `r lifecycle::badge('experimental')`
#' This returns real-time process data from the Posit Connect API. It requires
#' administrator privileges to use. NOTE that this only returns data for the
#' server that responds to the request (i.e. in a Highly Available cluster)
#'
#' @param src The source object
#'
#' @return
#' A tibble with the following columns:
#'
#'   * `pid`: The PID of the current process
#'   * `appId`: The application ID
#'   * `appGuid`: The application GUID
#'   * `appName`: The application name
#'   * `appUrl`: The application URL
#'   * `appRunAs`: The application RunAs user
#'   * `type`: The type of process
#'   * `cpuCurrent`: The current CPU usage
#'   * `cpuTotal`: The total CPU usage
#'   * `ram`: The current RAM usage
#'
#' @export
get_procs <- function(src) {
  validate_R6_class(src, "Connect")
  warn_experimental("get_procs")

  scoped_experimental_silence()
  raw_proc_data <- src$procs()

  proc_prep <- purrr::imap(
    raw_proc_data,
    function(x, y) {
      c(list(pid = y), x)
    }
  )
  tbl_data <- parse_connectapi_typed(proc_prep)
  tbl_data <- coerce_fs_bytes(tbl_data, "ram")

  return(tbl_data)
}

#' Perform an OAuth credential exchange to obtain a visitor's OAuth access token.
#'
#' @param connect A Connect R6 object.
#' @param user_session_token The content visitor's session token. This token
#' can only be obtained when the content is running on a Connect server. The token
#' identifies the user who is viewing the content interactively on the Connect server.
#' Read this value from the HTTP header: `Posit-Connect-User-Session-Token`
#' @param requested_token_type Optional. The requested token type. If unset, will
#' default to `urn:ietf:params:oauth:token-type:access_token`. Otherwise, this can
#' be set to `urn:ietf:params:aws:token-type:credentials` for AWS integrations or
#' `urn:posit:connect:api-key` for Connect API Key integrations.
#' @param audience Optional. The GUID of an OAuth integration associated with
#' this piece of content.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' library(plumber)
#' client <- connect()
#'
#' #* @get /do
#' function(req) {
#'   user_session_token <- req$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
#'   credentials <- get_oauth_credentials(client, user_session_token)
#'
#'   # ... do something with `credentials$access_token` ...
#'
#'   "done"
#' }
#' }
#'
#' @return The OAuth credential exchange response.
#'
#' @details
#' Please see https://docs.posit.co/connect/user/oauth-integrations/#obtaining-a-viewer-oauth-access-token
#' for more information.
#'
#' @seealso [get_integrations()], [get_oauth_content_credentials()]
#'
#' @export
get_oauth_credentials <- function(
  connect,
  user_session_token,
  requested_token_type = NULL,
  audience = NULL
) {
  validate_R6_class(connect, "Connect")

  if (!is.null(audience)) {
    error_if_less_than(connect$version, "2025.07.0")
  }

  url <- v1_url("oauth", "integrations", "credentials")
  body <- list(
    grant_type = "urn:ietf:params:oauth:grant-type:token-exchange",
    subject_token_type = "urn:posit:connect:user-session-token",
    subject_token = user_session_token,
    requested_token_type = requested_token_type,
    audience = audience
  )
  connect$POST(
    url,
    encode = "form",
    body = body
  )
}

#' Perform an OAuth credential exchange to obtain a content-specific OAuth
#' access token.
#'
#' @param connect A Connect R6 object.
#' @param content_session_token Optional. The content session token. This token
#' can only be obtained when the content is running on a Connect server. The
#' token identifies the service account integration previously configured by
#' the publisher on the Connect server. Defaults to the value found in the file
#' indicated by the `CONNECT_CONTENT_SESSION_TOKEN_FILE` environment variable
#' on Connect >= 2026.02.0, or from the value of the environment variable
#' `CONNECT_CONTENT_SESSION_TOKEN` on earlier versions.
#' @param requested_token_type Optional. The requested token type. If unset,
#' will default to `urn:ietf:params:oauth:token-type:access_token`. Otherwise,
#' this can be set to `urn:ietf:params:aws:token-type:credentials` for AWS
#' integrations or `urn:posit:connect:api-key` for Connect API Key integrations.
#' @param audience Optional. The GUID of an OAuth integration associated with
#' this piece of content.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' library(plumber)
#' client <- connect()
#'
#' #* @get /do
#' function(req) {
#'   credentials <- get_oauth_content_credentials(client)
#'
#'   # ... do something with `credentials$access_token` ...
#'
#'   "done"
#' }
#' }
#'
#' @return The OAuth credential exchange response.
#'
#' @details
#' Please see https://docs.posit.co/connect/user/oauth-integrations/#obtaining-a-service-account-oauth-access-token
#' for more information.
#'
#' @seealso [get_integrations()], [get_oauth_credentials()]
#'
#' @export
get_oauth_content_credentials <- function(
  connect,
  content_session_token = NULL,
  requested_token_type = NULL,
  audience = NULL
) {
  validate_R6_class(connect, "Connect")
  error_if_less_than(connect$version, "2024.12.0")

  if (!is.null(audience)) {
    error_if_less_than(connect$version, "2025.07.0")
  }

  # First, try CONNECT_CONTENT_SESSION_TOKEN_FILE
  if (is.null(content_session_token)) {
    token_file <- Sys.getenv("CONNECT_CONTENT_SESSION_TOKEN_FILE")
    if (nzchar(token_file)) {
      content_session_token <- readLines(token_file, n = 1, warn = FALSE)
    }
  }
  # If that doesn't exist (it was added in 2026.02.0), look for the token itself
  if (is.null(content_session_token)) {
    content_session_token <- Sys.getenv("CONNECT_CONTENT_SESSION_TOKEN")
    if (nchar(content_session_token) == 0) {
      stop(
        "Could not find the CONNECT_CONTENT_SESSION_TOKEN environment variable."
      )
    }
  }
  url <- v1_url("oauth", "integrations", "credentials")
  body <- list(
    grant_type = "urn:ietf:params:oauth:grant-type:token-exchange",
    subject_token_type = "urn:posit:connect:content-session-token",
    subject_token = content_session_token,
    requested_token_type = requested_token_type,
    audience = audience
  )
  connect$POST(
    url,
    encode = "form",
    body = body
  )
}

#' Obtain a visitor's AWS credentials
#'
#' @param connect A Connect R6 object.
#' @param user_session_token The content visitor's session token. This token
#' can only be obtained when the content is running on a Connect server. The token
#' identifies the user who is viewing the content interactively on the Connect server.
#' Read this value from the HTTP header: `Posit-Connect-User-Session-Token`
#' @param audience Optional. The GUID of an OAuth integration associated with
#' this piece of content.
#'
#' @return The AWS credentials as a list with fields named `access_key_id`,
#' `secret_access_key`, `session_token`, and `expiration`.
#'
#' @details
#' Please see https://docs.posit.co/connect/user/oauth-integrations/#obtaining-service-account-aws-credentials
#' for more information. See the example below of using this function in a
#' Plumber API with paws to access S3. Any library that allows you to pass
#' AWS credentials will be able to utilize the credentials returned from
#' this function call.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' library(plumber)
#' library(paws)
#' client <- connect()
#'
#' #* @get /do
#' function(req) {
#'   user_session_token <- req$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
#'   aws_credentials <- get_aws_credentials(client, user_session_token)
#'
#'   # Create S3 client with AWS credentials from Connect
#'   svc <- paws::s3(
#'     credentials = list(
#'       creds = list(
#'         access_key_id = aws_credentials$access_key_id,
#'         secret_access_key = aws_credentials$secret_access_key,
#'         session_token = aws_credentials$session_token
#'       )
#'     )
#'   )
#'
#'   # Get object from S3
#'   obj <- svc$get_object(
#'     Bucket = "my-bucket",
#'     Key = "my-data.csv"
#'   )
#'
#'   "done"
#' }
#' }
#'
#' @export
get_aws_credentials <- function(connect, user_session_token, audience = NULL) {
  error_if_less_than(connect$version, "2025.03.0")

  if (!is.null(audience)) {
    error_if_less_than(connect$version, "2025.07.0")
  }

  response <- get_oauth_credentials(
    connect,
    user_session_token,
    requested_token_type = "urn:ietf:params:aws:token-type:credentials",
    audience = audience
  )

  # Extract access token and decode it
  access_token <- rawToChar(base64enc::base64decode(response$access_token))
  credentials <- jsonlite::fromJSON(access_token)

  list(
    access_key_id = credentials$accessKeyId,
    secret_access_key = credentials$secretAccessKey,
    session_token = credentials$sessionToken,
    expiration = credentials$expiration
  )
}

#' Obtain AWS credentials for your content.
#'
#' @param connect A Connect R6 object.
#' @param content_session_token Optional. The content session token. This token
#' can only be obtained when the content is running on a Connect server. The
#' token identifies the service account integration previously configured by
#' the publisher on the Connect server. Defaults to the value from the
#' environment variable: `CONNECT_CONTENT_SESSION_TOKEN`
#' @param audience Optional. The GUID of an OAuth integration associated with
#' this piece of content.
#'
#' @return The AWS credentials as a list with fields named `access_key_id`,
#' `secret_access_key`, `session_token`, and `expiration`.
#'
#' @details
#' Please see https://docs.posit.co/connect/user/oauth-integrations/#obtaining-service-account-aws-credentials
#' for more information. See the example below of using this function with
#' `paws` to access S3. Any library that allows you to pass AWS credentials
#' will be able to utilize the credentials returned from this function call.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' library(paws)
#'
#' client <- connect()
#' # Pulls the content session token from the environment
#' # when deployed into Connect.
#' aws_credentials <- get_aws_content_credentials(client)
#'
#' # Create S3 client with AWS credentials from Connect
#' svc <- paws::s3(
#'   credentials = list(
#'     creds = list(
#'       access_key_id = aws_credentials$access_key_id,
#'       secret_access_key = aws_credentials$secret_access_key,
#'       session_token = aws_credentials$session_token
#'     )
#'   )
#' )
#'
#' # Get object from S3
#' obj <- svc$get_object(
#'   Bucket = "my-bucket",
#'   Key = "my-data.csv"
#' )
#' }
#'
#' @export
get_aws_content_credentials <- function(
  connect,
  content_session_token = NULL,
  audience = NULL
) {
  error_if_less_than(connect$version, "2025.03.0")

  if (!is.null(audience)) {
    error_if_less_than(connect$version, "2025.07.0")
  }

  response <- get_oauth_content_credentials(
    connect,
    content_session_token,
    requested_token_type = "urn:ietf:params:aws:token-type:credentials",
    audience = audience
  )

  # Extract access token and decode it
  access_token <- rawToChar(base64enc::base64decode(response$access_token))
  credentials <- jsonlite::fromJSON(access_token)

  list(
    access_key_id = credentials$accessKeyId,
    secret_access_key = credentials$secretAccessKey,
    session_token = credentials$sessionToken,
    expiration = credentials$expiration
  )
}

#' Get available runtimes on server
#'
#' Get a table showing available versions of R, Python, Quarto, and Tensorflow
#' on the Connect server.
#'
#' @param client A `Connect` object.
#' @param runtimes Optional. A character vector of runtimes to include. Must be
#' some combination of `"r"`, `"python"`, `"quarto"`, and `"tensorflow"`. Quarto
#' is only supported on Connect >= 2021.08.0, and Tensorflow is only supported
#' on Connect >= 2024.03.0.

#' @return A tibble with columns for `runtime`, `version`, and `cluster_name`
#' and `image_name`. Cluster name and image name are only meaningful on Connect
#' instances running off-host execution.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#' get_runtimes(client, runtimes = c("r", "python", "tensorflow"))
#' }
#'
#' @export
get_runtimes <- function(client, runtimes = NULL) {
  validate_R6_class(client, "Connect")

  # Construct valid runtimes for the Connect version.
  supported <- c(
    "r",
    "python",
    if (compare_connect_version(client$version, "2021.08.0") >= 0) {
      "quarto"
    },
    if (compare_connect_version(client$version, "2024.05.0") >= 0) {
      "tensorflow"
    }
  )
  if (is.null(runtimes)) {
    runtimes <- supported
  } else {
    if (any(!runtimes %in% supported)) {
      stop(glue::glue(
        "`runtimes` must be one of ",
        "{paste(paste0('\"', supported, '\"'), collapse = ', ')}; ",
        "received: {paste(paste0('\"', runtimes, '\"'), collapse = ', ')}."
      ))
    }
  }

  purrr::list_rbind(purrr::map(
    runtimes,
    function(runtime) {
      res <- client$GET(paste0("v1/server_settings/", runtime))
      res_df <- purrr::list_rbind(
        purrr::map(res$installations, ~ tibble::as_tibble(.))
      )
      tibble::add_column(res_df, runtime = runtime, .before = 1)
    }
  ))
}

#' All package dependencies on the server
#'
#' @description Get a data frame of package dependencies used by all content
#' items on the server.
#'
#' @usage get_packages(src, name = NULL, page_size = 100000, limit = Inf)
#'
#' @param src A `Connect` client object.
#' @param name Optional package name to filter by. Python package are normalized
#' during matching; R package names must match exactly.
#' @param page_size Optional. Integer specifying page size for API
#' paging.
#' @param limit Optional. Specify the maximum number of records after which
#' to cease paging.
#'
#' @return A data frame with the following columns:
#'
#' - `language`: Language ecosystem the package belongs to (`r` or `python`)
#' - `language_version`: Version of R or Python used by the content
#' - `name`: Package name
#' - `version`: Package version
#' - `hash`: Package description hash for R packages
#' - `bundle_id`: Identifier for the bundle that depends on this package
#' - `content_id`: Numeric identifier for the content that depends on this
#'   package
#' - `content_guid`: The unique identifier of the content item that depends on
#'   this package
#'
#' @examples
#' \dontrun{
#' client <- connect()
#' packages <- get_packages(client)
#' }
#'
#' @family packages functions
#' @export
get_packages <- function(src, name = NULL, page_size = 100000, limit = Inf) {
  validate_R6_class(src, "Connect")
  error_if_less_than(src$version, "2024.11.0")
  res <- page_offset(
    src,
    src$packages(
      name = name,
      page_size = page_size
    )
  )
  out <- parse_connectapi_typed(res)

  # Connect is standardizing on using `content_id` and `content_guid`.
  # Handle that name change now in a forward-compatible way.
  if ("app_id" %in% names(out)) {
    out$content_id <- out$app_id
    out$app_id <- NULL
  }
  if ("app_guid" %in% names(out)) {
    out$content_guid <- out$app_guid
    out$app_guid <- NULL
  }

  out
}

#' Get all vanity URLs
#'
#' Get a table of all vanity URLs on the server. Requires administrator
#' privileges.
#'
#' @param client A `Connect` object.
#'
#' @return A tibble with columns for `content_guid`, `path`, and
#' `created_time`.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#' get_vanity_urls(client)
#' }
#'
#' @export
get_vanity_urls <- function(client) {
  res <- client$vanities()
  parse_connectapi_typed(res, datetime_cols = "created_time")
}
