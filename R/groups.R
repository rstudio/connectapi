#' Get group information from the Posit Connect server
#'
#' @param src The source object.
#' @param page_size The number of records to return per page (max 500).
#' @param prefix Filters groups by prefix (group name).
#' The filter is case insensitive.
#' @param limit The number of groups to retrieve before paging stops.
#'
#' `limit` will be ignored is `prefix` is not `NULL`.
#' To limit results when `prefix` is not `NULL`, change `page_size`.
#'
#' @return
#' A tibble with the following columns:
#'
#'   * `guid`: The unique identifier of the group
#'   * `name`: The group name
#'   * `owner_guid`: The group owner's unique identifier. When using LDAP or
#'     Proxied authentication with group provisioning enabled this property
#'     will always be null.
#'
#' @details
#' Please see https://docs.posit.co/connect/api/#get-/v1/groups for more information.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#'
#' # get all groups
#' get_groups(client, limit = Inf)
#' }
#'
#' @family groups functions
#' @export
get_groups <- function(src, page_size = 500, prefix = NULL, limit = Inf) {
  validate_R6_class(src, "Connect")

  # The `v1/groups` endpoint always returns the first page when `prefix` is
  # specified, so the page_offset function, which increments until it hits an
  # empty page, fails.
  if (!is.null(prefix)) {
    response <- src$groups(page_size = page_size, prefix = prefix)
    res <- response$results
  } else {
    res <- page_offset(src, src$groups(page_size = page_size, prefix = NULL), limit = limit)
  }

  out <- parse_connectapi_typed(res, connectapi_ptypes$groups)

  return(out)
}

#' Get users within a specific group
#'
#' @param src The source object
#' @param guid A group GUID identifier
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
#'   * `created_time`: The timestamp (in RFC3339 format) when the user
#'     was created in the Posit Connect server
#'   * `updated_time`: The timestamp (in RFC3339 format) when the user
#'     was last updated in the Posit Connect server
#'   * `active_time`: The timestamp (in RFC3339 format) when the user
#'     was last active on the Posit Connect server
#'   * `confirmed`: When false, the created user must confirm their
#'     account through an email. This feature is unique to password
#'     authentication.
#'   * `locked`: Whether or not the user is locked
#'   * `guid`: The user's GUID, or unique identifier, in UUID RFC4122 format
#'
#' @details
#' Please see https://docs.posit.co/connect/api/#get-/v1/groups/-group_guid-/members
#' for more information.
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#'
#' # get the first 20 groups
#' groups <- get_groups(client)
#'
#' group_guid <- groups$guid[1]
#'
#' get_group_members(client, guid = group_guid)
#' }
#'
#' @family groups functions
#' @export
get_group_members <- function(src, guid) {
  validate_R6_class(src, "Connect")

  res <- src$group_members(guid)

  out <- parse_connectapi(res$results)

  return(out)
}

#' Get content access permissions for a group or groups
#'
#' @param src The source object
#' @param groups A data frame or tibble of groups
#'
#' @return
#' A tibble with the following columns:

#'   * `group_guid`: The group's GUID
#'   * `group_name`: The group's name
#'   * `content_guid`: The content item's GUID
#'   * `content_name`: The content item's name
#'   * `content_title`: The content item's title
#'   * `access_type`: The access type of the content item ("all", "logged_in", or "acl")
#'   * `role`: The access type that members of the group have to the
#'     content item, "publisher" or "viewer".
#'
#' @examples
#' \dontrun{
#' library(connectapi)
#' client <- connect()
#'
#' # Get a data frame of groups
#' groups <- get_groups(client)
#'
#' # Get permissions for a single group by passing in the corresponding row.
#' get_group_content(client, groups[1, ])
#' dplyr::filter(groups, name = "research_scientists") %>%
#'   get_group_content(client, groups = .)
#'
#' # Get permissions for all groups by passing in the entire groups data frame.
#' get_group_content(client, groups)
#' }
#'
#' @family groups functions
#' @export
get_group_content <- function(src, groups) {
  validate_R6_class(src, "Connect")

  purrr::pmap_dfr(
    dplyr::select(groups, guid, name),
    get_group_content_impl,
    src = src
  )
}

get_group_content_impl <- function(src, guid, name) {
  validate_R6_class(src, "Connect")

  res <- src$group_content(guid)
  parsed <- parse_connectapi_typed(res, connectapi_ptypes$group_content)

  dplyr::transmute(parsed,
    group_guid = guid,
    group_name = name,
    content_guid,
    content_name,
    content_title,
    access_type,
    role = purrr::map_chr(
      permissions,
      extract_role,
      principal_name = name
    )
  )
}

extract_role <- function(permissions, principal_name) {
  matched <- purrr:::keep(
    permissions,
    ~ .x[["principal_name"]] == principal_name && .x[["principal_type"]] == "group")
  if (length(matched) == 1) {
    return(matched[[1]][["principal_role"]])
  } else {
    stop("Unexpected permissions structure.")
  }
}
