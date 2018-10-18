#' Promote content from one Connect server to another
#'
#' @param from The url for the server containing the content (the originating
#'   server)
#' @param to   The url for the server where the content will be deployed (the
#'   destination server)
#' @param to_key An API key on the destination "to" server. If the destination
#'   content is going to be updated, the API key must belong to a user with
#'   collaborator access on the content that will be updated. If the destination
#'   content is to be created new, the API key must belong to a user with
#'   publisher priviliges.
#' @param from_key An API key on the originating "from" server. The API key must
#'   belong to a user with collaborator access to the content to be promoted.
#' @param app_name The name of the content on the originating "from" server.
#'   If content with the same name is found on the destination server,
#'   the content will be updated. If no content on the destination server
#'   has a matching name, a new endpoint will be created.
#' @return The URL for the content on the destination "to" server
#' @export
promote <- function(from,
                    to,
                    to_key,
                    from_key,
                    app_name) {

  # TODO Validate Inputs

  #set up clients
  from_client <- Connect$new(host = from, api_key = from_key)
  to_client <- Connect$new(host = to, api_key = to_key)

  # find app on "from" server
  from_app <- from_client$get_apps(list(name = app_name))
  if (length(from_app) != 1) {
    stop(sprintf('Found %d apps matching app name %s on %s. Content must have a unique name.', length(from_app), app_name, from))
  }

  # download bundle
  bundle <- from_client$download_bundle(from_app[[1]]$bundle_id)

  # find or create app to update
  to_app <- to_client$get_apps(list(name = app_name))
  if (length(to_app) > 1) {
    stop(sprintf('Found %d apps matching %s on %s, content must have a unique name.', length(to_app), app_name, to))
  } else if (length(to_app) == 0) {
    # create app
    to_app <- to_client$create_app(app_name)
    warning(sprintf('Creating NEW app %d with name %s on %s', to_app$id, app_name, to))
  } else {
    to_app <- to_app[[1]]
    warning(sprintf('Updating EXISTING app %d with name %s on %s', to_app$id, app_name, to))
  }

  task_id <- deploy_bundle(
    connect = to_client,
    bundle = bundle,
    app = to_app$id
  )
  
  poll_task(connect = to_client, task_id = task_id)
  
  to_app_url <- app$url
  
  return(to_app_url)
}

#' @export
dir_bundle <- function(path = ".") {
  before_wd <- getwd()
  setwd(directory)
  on.exit(expr = setwd(before_wd), add = TRUE)
  
  utils::tar(tarfile = "bundle.tar.gz", files = ".", compression = "gzip", tar = "internal")
  
  return(fs::path_abs("bundle.tar.gz"))
}

#' @export
dir_manifest <- function(path = ".", ...) {
  if (!requireNamespace("rsconnect", quitely = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.", call. = FALSE)
  }
  rsconnect::writeManifest(
    appDir = directory,
    appFiles = fs::path_rel(
      fs::dir_ls(directory, recursive = TRUE),
      start = directory
    ),
    ...
  )
}

#' @export
deploy_bundle <- function(connect, bundle, app_id){
  #upload bundle
  new_bundle_id <- connect$upload_bundle(bundle, app_id)
  
  #activate bundle
  task_id <- connect$activate_bundle(app_id, new_bundle_id)
  
  return(task_id)
}

#' @export
poll_task <- function(connect, task_id) {
  start <- 0
  while (task_id > 0) {
    Sys.sleep(2)
    status <- connect$get_task(task_id, start)
    if (length(status$status) > 0) {
      lapply(status$status, print)
      start <- status$last_status
    }
    if (status$finished) {
      task_id = 0
    }
  }
  invisible()
}