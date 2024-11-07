#' Promote content from one Connect server to another
#'
#' @param from The url for the server containing the content (the originating
#'   server)
#'
#' @param to   The url for the server where the content will be deployed (the
#'   destination server)
#'
#' @param to_key An API key on the destination "to" server. If the destination
#'   content is going to be updated, the API key must belong to a user with
#'   collaborator access on the content that will be updated. If the destination
#'   content is to be created new, the API key must belong to a user with
#'   publisher privileges.
#'
#' @param from_key An API key on the originating "from" server. The API key must
#'   belong to a user with collaborator access to the content to be promoted.
#'
#' @param name The name of the content on the originating "from" server.
#'   If content with the same name is found on the destination server,
#'   the content will be updated. If no content on the destination server
#'   has a matching name, a new endpoint will be created.
#'
#' @return The URL for the content on the destination "to" server
#' @export
promote <- function(from,
                    to,
                    to_key,
                    from_key,
                    name) {
  # TODO Validate Inputs

  # set up clients
  from_client <- connect(server = from, api_key = from_key)
  to_client <- connect(server = to, api_key = to_key)

  # find app on "from" server
  from_app <- from_client$get_apps(list(name = name))
  if (length(from_app) != 1) {
    stop(sprintf(
      "Found %d apps matching app name %s on %s. Content must have a unique name.",
      length(from_app),
      name,
      from
    ))
  }
  from_app <- content_item(from_client, guid = from_app[[1]]$guid)

  # download bundle
  bundle <- download_bundle(from_app)

  # find or create app to update
  to_app <- content_ensure(connect = to_client, name = name)
  to_app <- content_item(connect = to_client, guid = to_app$guid)

  task <- deploy(to_client, bundle = bundle, guid = to_app$get_content()$guid)

  poll_task(task)

  to_app_url <- to_app$get_url()

  return(to_app_url)
}
