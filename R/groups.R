#' Get group information from the Posit Connect server
#'
#' @param src The source object.
#' @param page_size The number of records to return per page (max 500).
#' @param prefix Filters groups by prefix (group name).
#' The filter is case insensitive.
#' @param limit The number of groups to retrieve before paging stops. Default
#' is to return all results; however, for Connect server versions older than
#' 2025.04.0, `limit` is capped at 500 when `prefix` is provided.
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

  # Prior to 2025.04.0, the `v1/groups` endpoint ignored page_number when
  # `prefix` was provided, always returning the first page.
  # So for those versions, we should request the max page size needed to
  # satisfy `limit`, and make them equal, so that page_offset() only fetches
  # one page.
  # Proper pagination added in https://github.com/posit-dev/connect/pull/31086
  if (
    !is.null(prefix) && compare_connect_version(src$version, "2025.04.0") < 0
  ) {
    # The max page size allowed by the server is 500, so in this version of
    # Connect, that's the maximum number of groups we can get.
    page_size <- limit <- min(500, limit)
  }
  res <- page_offset(
    src,
    src$groups(page_size = page_size, prefix = prefix),
    limit = limit
  )

  parse_connectapi_typed(res)
}

#' Get users within a specific group
#'
#' @param src A Connect client object
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

  parse_connectapi_typed(
    res$results,
    datetime_cols = c("created_time", "updated_time", "active_time")
  )
}

#' Get content access permissions for a group or groups
#'
#' @param src A Connect client object
#' @param guids A character vector of group guids
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
#' get_group_content(client, groups[1, "guid"])
#' dplyr::filter(groups, name = "research_scientists") %>%
#'   dplyr::pull(guid) %>%
#'   get_group_content(client, .)
#'
#' # Get permissions for all groups by passing in all group guids.
#' get_group_content(client, groups$guid)
#' }
#'
#' @family groups functions
#' @export
get_group_content <- function(src, guids) {
  validate_R6_class(src, "Connect")

  purrr::list_rbind(
    purrr::map(
      guids,
      ~ get_one_groups_content(src = src, guid = .x)
    )
  )
}

get_one_groups_content <- function(src, guid) {
  validate_R6_class(src, "Connect")

  res <- src$group_content(guid)
  if (length(res) == 0) {
    return(tibble::tibble(
      group_guid = NA_character_,
      group_name = NA_character_,
      content_guid = NA_character_,
      content_name = NA_character_,
      content_title = NA_character_,
      access_type = NA_character_,
      role = NA_character_
    ))
  }
  parsed <- parse_connectapi_typed(res)

  permissions_df <- purrr::list_rbind(
    purrr::map(
      parsed$permissions,
      ~ tibble::as_tibble(unlist(
        purrr::keep(
          .x,
          ~ .x[["principal_guid"]] == guid
        ),
        recursive = FALSE
      ))
    )
  )

  tibble::tibble(
    group_guid = guid,
    group_name = permissions_df$principal_name,
    content_guid = parsed$content_guid,
    content_name = parsed$content_name,
    content_title = parsed$content_title,
    access_type = parsed$access_type,
    role = permissions_df$principal_role
  )
}
