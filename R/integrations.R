#' Get OAuth integrations
#'
#' @description
#' Retrieve OAuth integrations either from the Connect server or associated with a specific content item.
#'
#' If `x` is a `Connect` object, this function lists all OAuth integrations on the server.
#' If `x` is a `Content` object, it returns the integrations associated with that content item.
#'
#' You must have administrator or publisher privileges to use this function.
#'
#' @param x A `Connect` or `Content` R6 object.
#'
#' @return A list of class `connect_integration_list`, where each element is a `connect_integration` object
#'   with the following fields. (Raw API fields are character strings unless noted otherwise):
#'
#'   * `id`: The internal identifier of this OAuth integration.
#'   * `guid`: The GUID of this OAuth integration.
#'   * `created_time`: Timestamp (RFC3339) when the integration was created.
#'   * `updated_time`: Timestamp (RFC3339) when the integration was last updated.
#'   * `name`: A descriptive name.
#'   * `description`: A brief description.
#'   * `template`: The template used to configure the integration.
#'   * `auth_type`: The OAuth flow used.
#'   * `config`: A list with integration-specific config fields.
#'
#' Use [as.data.frame()] or [tibble::as_tibble()] to convert the result to a data frame with parsed types.
#'
#' @seealso
#' [get_integration()], [set_integrations()], [get_associations()]
#'
#' @family oauth integration functions
#'
#' @examples
#' \dontrun{
#' # From a Connect client
#' client <- connect()
#' integrations <- get_integrations(client)
#'
#' # Filter or update specific ones
#' github_integration <- purrr::keep(integrations, \(x) x$template == "github")[[1]]
#'
#' json_payload <- jsonlite::toJSON(list(
#'   description = "Updated Description",
#'   config = list(client_secret = "new-secret")
#' ), auto_unbox = TRUE)
#'
#' client$PATCH(
#'   paste0("v1/oauth/integrations/", github_integration$guid),
#'   body = json_payload
#' )
#'
#' # From a Content item
#' content <- content_item(client, "12345678-90ab-cdef-1234-567890abcdef")
#' content_integrations <- get_integrations(content)
#'
#' # Filter content integrations
#' snowflake_integrations <- purrr::keep(content_integrations, ~ .x$template == "snowflake")
#' }
#'
#' @export
get_integrations <- function(x) {
  UseMethod("get_integrations")
}

#' @export
get_integrations.default <- function(x) {
  stop(
    "Cannot get integrations for an object of class '",
    class(x)[1],
    "'. 'x' must be a 'Connect' or 'Content' object."
  )
}

#' @export
get_integrations.Connect <- function(x) {
  error_if_less_than(x$version, "2024.12.0")
  integrations <- x$GET(v1_url("oauth", "integrations"))
  integrations <- purrr::map(integrations, ~ as_integration(.x, client = x))
  class(integrations) <- c("connect_integration_list", class(integrations))
  integrations
}

#' @export
get_integrations.Content <- function(x) {
  error_if_less_than(x$connect$version, "2024.12.0")
  assoc <- x$connect$GET(v1_url(
    "content",
    x$content$guid,
    "oauth",
    "integrations",
    "associations"
  ))
  integrations <- purrr::map(
    assoc,
    ~ get_integration(x$connect, .x$oauth_integration_guid)
  )
  class(integrations) <- c("connect_integration_list", class(integrations))
  integrations
}

#' Convert integrations list to a data frame
#'
#' @description
#' Converts a list returned by [get_integrations()] into a data frame.
#'
#' @param x A `connect_integration_list` object (from [get_integrations()]).
#' @param row.names Passed to [base::as.data.frame()].
#' @param optional Passed to [base::as.data.frame()].
#' @param ... Passed to [base::as.data.frame()].
#'
#' @return A `data.frame` with one row per integration.
#' @export
as.data.frame.connect_integration_list <- function(
  x,
  row.names = NULL, # nolint
  optional = FALSE,
  ...
) {
  integrations_tbl <- as_tibble(x)
  as.data.frame(
    integrations_tbl,
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' Convert integration list to a tibble
#'
#' @description
#' Converts a list returned by [get_integrations()] to a tibble.
#'
#' @param x A `connect_integration_list` object.
#' @param ... Unused.
#'
#' @return A tibble with one row per integration.
#' @export
as_tibble.connect_integration_list <- function(x, ...) {
  parse_connectapi_typed(
    x,
    datetime_cols = c("created_time", "updated_time")
  )
}

# Integration class ----

#' Convert objects to integration class
#'
#' @param x An object to convert to an integration.
#' @param client The Connect client object where the integration comes from.
#'
#' @return An integration object. The object has all the fields from the
#' integrations endpoint (see [get_integrations()]) and a Connect client as a
#' `client` attribute (`attr(x, "client")`)
as_integration <- function(x, client) {
  if (!inherits(x, "list")) {
    stop(
      "Cannot convert object of class '",
      class(x)[1],
      "' to an integration"
    )
  }
  structure(x, class = c("connect_integration", "list"), client = client)
}

#' @export
print.connect_integration <- function(x, ...) {
  cat("Integration:", x$name, "\n")
  cat("GUID:", x$guid, "\n")
  cat("Template:", x$template, "\n")
  invisible(x)
}

#' Get the details of an OAuth integration
#'
#' @description
#' Given the GUID of an OAuth integration available on a Connect server, retrieve
#' its details. You must have administrator or publisher privileges to perform
#' this action.
#'
#' @param client A `Connect` R6 client object.
#' @param guid The GUID of an integration available on the Connect server.
#'
#' @return A `connect_integration` object representing an OAuth integration,
#' which has the following fields:
#'
#'   * `id`: The internal identifier of this OAuth integration.
#'   * `guid`: The GUID of this OAuth integration.
#'   * `created_time`: The timestamp (RFC3339) indicating when this integration
#'     was created.
#'   * `updated_time`: The timestamp (RFC3339) indicating when this integration
#'     was last updated
#'   * `name`: A descriptive name to identify the OAuth integration.
#'   * `description`: A brief text to describe the OAuth integration.
#'   * `template`: The template used to configure this OAuth integration.
#'   * `auth_type`: The authentication type indicates which OAuth flow is used by
#'     this integration.
#'   * `config`: A list with the OAuth integration configuration. Fields
#'     differ between integrations.
#'
#' @seealso [get_integrations()], [get_associations()], [set_integrations()]
#'
#' @examples
#' \dontrun{
#' client <- connect()
#' x <- get_integration(client, guid)
#' }
#'
#' @family oauth integration functions
#' @export
get_integration <- function(client, guid) {
  validate_R6_class(client, "Connect")
  error_if_less_than(client$version, "2024.12.0")
  as_integration(client$GET(v1_url("oauth", "integrations", guid)), client)
}

# Get and set integrations on content ----

#' Set all OAuth integrations for a content item
#'
#' @description
#' Removes all existing OAuth integrations associated with a content item, and
#' creates associations with the integrations provided. You must have
#' administrator or publisher privileges to perform this action.
#'
#' @param content A `Content` R6 object representing the content item to modify.
#' @param integrations The complete set of integrations to be associated with the
#'   content. May be a single `connect_integration` object, a list of
#'   `connect_integration` objects, or `NULL`. Passing in an empty list or
#'   explicitly passing `NULL` will remove all associated integrations from the
#'   content.
#'
#' @return Invisibly returns `NULL`.
#'
#' @seealso
#' [get_integrations()], [get_integration()], [get_associations()], [content_item()]
#'
#' @examples
#' \dontrun{
#' client <- connect()
#'
#' content <- content_item(client, "12345678-90ab-cdef-1234-567890abcdef")
#'
#' integrations <- get_integrations(client)
#'
#' # Associate a single integration
#' github_integration <- purrr::keep(integrations, \(x) x$template == "github")[[1]]
#' set_integrations(content, github_integration)
#'
#' # Associate multiple integrations at once
#' selected_integrations <- integrations[1:2]
#' set_integrations(content, selected_integrations)
#'
#' # Unset integrations
#' set_integrations(content, NULL)
#' }
#'
#' @family oauth integration functions
#' @family content functions
#' @export
set_integrations <- function(content, integrations) {
  validate_R6_class(content, "Content")
  # Handle a single integration
  if (inherits(integrations, "connect_integration")) {
    integrations <- list(integrations)
  } else if (!is.null(integrations) && !inherits(integrations, "list")) {
    stop(
      "'integrations' must be a 'connect_integration' class object, a list, ",
      "or NULL."
    )
  }
  # Ensure that all the items we've been passed are integrations
  if (!purrr::every(integrations, ~ inherits(.x, "connect_integration"))) {
    stop("All items must be 'connect_integration' objects")
  }

  payload <- purrr::map(integrations, ~ list(oauth_integration_guid = .x$guid))

  content$connect$PUT(
    v1_url(
      "content",
      content$content$guid,
      "oauth",
      "integrations",
      "associations"
    ),
    body = payload
  )
  invisible(NULL)
}

#' Get OAuth associations for a piece of content
#'
#' @description
#' Given a `Content` object, retrieves a list of its
#' OAuth associations. An association contains a content GUID and an association
#' GUID, and indicates that the integration can be used by the content when it
#' runs.
#'
#' @param x A `Content` object
#'
#' @return A list of OAuth integration associations. Each association includes details such as:
#' * `app_guid`: The content item's GUID (deprecated, use `content_guid` instead).
#' * `content_guid`: The content item's GUID.
#' * `oauth_integration_guid`: The GUID of the OAuth integration.
#' * `oauth_integration_name`: The name of the OAuth integration.
#' * `oauth_integration_description`: A description of the OAuth integration.
#' * `oauth_integration_template`: The template used for this OAuth integration.
#' * `oauth_integration_auth_type`: The authentication type (e.g., "Viewer" or "Service Account").
#' * `created_time`: The timestamp when the association was created.
#'
#' @seealso
#' [set_integrations()], [get_integrations()], [get_integration()]
#'
#' @examples
#' \dontrun{
#' client <- connect()
#'
#' # Get OAuth associations for an app.
#' my_app <- content_item(client, "12345678-90ab-cdef-1234-567890abcdef")
#' my_app_associations <- get_associations(my_app)
#'
#' # Given those associations, retrieve the integrations themselves.
#' my_app_integrations <- purrr::map(
#'   my_app_associations,
#'   ~ get_integration(client, .x$oauth_integration_guid)
#' )
#' }
#'
#' @family oauth integration functions
#' @family content functions
#' @export
get_associations <- function(x) {
  validate_R6_class(x, "Content")
  x$connect$GET(v1_url(
    "content",
    x$content$guid,
    "oauth",
    "integrations",
    "associations"
  ))
}


# Manage integrations ----

#' Create an OAuth integration
#'
#' @description
#' Creates a new OAuth integration on the Posit Connect server. OAuth integrations
#' allow content to access external resources using OAuth credentials.
#'
#' You must have administrator privileges to perform this action.
#'
#' See the Posit Connect documentation on
#' [OAuth integrations](https://docs.posit.co/connect/admin/integrations/oauth-integrations/) for
#' more information.
#'
#' @param client A `Connect` R6 client object.
#' @param name A descriptive name to identify the integration.
#' @param description Optional, default `NULL.` A brief description of the integration.
#' @param template The template to use to configure this integration (e.g.,
#'   "custom", "github", "google", "connect").
#' @param config A list containing the configuration for the integration. The
#'   required fields vary depending on the template selected.
#'
#' @return A `connect_integration` object representing the newly created
#'   integration. See [get_integration()] for details on the returned object.
#'
#' @seealso [get_integrations()], [get_integration()], [update_integration()],
#'   [delete_integration()]
#'
#' @examples
#' \dontrun{
#' client <- connect()
#'
#' # Create a GitHub OAuth integration
#' github_integration <- create_integration(
#'   client,
#'   name = "GitHub Integration",
#'   description = "Integration with GitHub for OAuth access",
#'   template = "github",
#'   config = list(
#'     client_id = "your-client-id",
#'     client_secret = "your-client-secret"
#'   )
#' )
#'
#' # Create a custom OAuth integration
#' custom_integration <- create_integration(
#'   client,
#'   name = "Custom API Integration",
#'   description = "Integration with our custom API",
#'   template = "custom",
#'   config = list(
#'     auth_mode = "Confidential",
#'     auth_type = "Viewer",
#'     authorization_uri = "https://api.example.com/oauth/authorize",
#'     client_id = "your-client-id",
#'     client_secret = "your-client-secret",
#'     token_uri = "https://api.example.com/oauth/token"
#'   )
#' )
#' }
#'
#' @family oauth integration functions
#' @export
create_integration <- function(
  client,
  name,
  description = NULL,
  template,
  config
) {
  validate_R6_class(client, "Connect")
  error_if_less_than(client$version, "2024.12.0")
  result <- client$POST(
    v1_url("oauth", "integrations"),
    body = list(
      name = name,
      description = description,
      template = template,
      config = config
    )
  )
  as_integration(result, client)
}

#' Update an OAuth integration
#'
#' @description
#' Updates an existing OAuth integration. All fields except `integration` are optional,
#' and are unchanged if not provided.
#'
#' You must have administrator privileges to perform this action.
#'
#' See the Posit Connect documentation on
#' [OAuth integrations](https://docs.posit.co/connect/admin/integrations/oauth-integrations/) for
#' more information.
#'
#' @param integration A `connect_integration` object (as returned by [get_integrations()],
#'   [get_integration()], or [create_integration()]).
#' @param name A new name for the integration.
#' @param description A new description for the integration.
#' @param template The template to use (generally not changed after creation).
#' @param config A list with updated OAuth integration configuration. If `NULL`
#'   (default), the configuration remains unchanged. You can update individual
#'   configuration fields without affecting others.
#'
#' @return A `connect_integration` object representing the updated OAuth
#'   integration. See [get_integration()] for details on the returned object.
#'
#' @seealso [get_integrations()], [get_integration()], [create_integration()],
#'   [delete_integration()]
#'
#' @examples
#' \dontrun{
#' client <- connect()
#'
#' # Get an existing integration
#' integration <- get_integration(client, "your-integration-guid")
#'
#' # Update the integration's name and description
#' updated_integration <- update_integration(
#'   integration,
#'   name = "Updated GitHub Integration",
#'   description = "A more descriptive description."
#' )
#'
#' # Update only the client secret in the configuration
#' updated_integration <- update_integration(
#'   integration,
#'   config = list(
#'     client_secret = "your-new-client-secret"
#'   )
#' )
#' }
#'
#' @family oauth integration functions
#' @export
update_integration <- function(
  integration,
  name = NULL,
  description = NULL,
  template = NULL,
  config = NULL
) {
  if (!inherits(integration, "connect_integration")) {
    stop("'integration' must be a 'connect_integration' object")
  }
  client <- attr(integration, "client")
  validate_R6_class(client, "Connect")
  error_if_less_than(client$version, "2024.12.0")
  result <- client$PATCH(
    v1_url("oauth", "integrations", integration$guid),
    body = list(
      name = name,
      description = description,
      template = template,
      config = config
    )
  )
  as_integration(result, client)
}

#' Delete an OAuth integration
#'
#' @description
#' Deletes an OAuth integration from the Posit Connect server. This permanently
#' removes the integration and any associated content associations.
#'
#' You must have administrator privileges to perform this action.
#'
#' See the Posit Connect documentation on
#' [OAuth integrations](https://docs.posit.co/connect/admin/integrations/oauth-integrations/) for
#' more information.
#'
#' @param integration A `connect_integration` object (as returned by [get_integrations()],
#'   [get_integration()], or [create_integration()]).
#'
#' @return Returns `NULL` invisibly if successful.
#'
#' @seealso [get_integrations()], [get_integration()], [create_integration()],
#'   [update_integration()]
#'
#' @examples
#' \dontrun{
#' client <- connect()
#'
#' # Get an integration to delete
#' integration <- get_integration(client, "your-integration-guid")
#'
#' # Delete the integration
#' delete_integration(integration)
#' }
#'
#' @family oauth integration functions
#' @export
delete_integration <- function(integration) {
  if (!inherits(integration, "connect_integration")) {
    stop("'integration' must be a 'connect_integration' object")
  }
  client <- attr(integration, "client")
  validate_R6_class(client, "Connect")
  error_if_less_than(client$version, "2024.12.0")
  client$DELETE(v1_url("oauth", "integrations", integration$guid))
  invisible(NULL)
}
