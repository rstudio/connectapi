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
  to_app <- content_ensure(connect = to_client, name = app_name)

  bundle_id <- connect$content_upload(bundle_path = bundle, guid = to_app[["guid"]])
  task_id <- connect$content_deploy(guid = to_app[["guid"]], bundle_id = bundle_id)
  
  poll_task(connect = to_client, task_id = task_id)
  
  to_app_url <- app$url
  
  return(to_app_url)
}

#' @export
content_ensure <- function(connect, name = random_name(), title = name, guid = NULL, ...) {
  
  if (!is.null(guid)) {
    # guid-based deployment
    # just in case we get a 404 back...
    content <- tryCatch(connect$get_content(guid = guid), error = function(e){return(NULL)})
    if (is.null(content)) {
      warning(glue::glue(
        "guid {guid} was not found on {connect$host}.",
        "Creating new content with name {name}"))
      content <- connect$content_create(
        name = name,
        title = title,
        ...
      )
    }
  } else {
    # name-based deployment
    content <- connect$get_apps(list(name = name))
    if (length(content) > 1) {
      stop(glue::glue("Found {length(to_content)} content items ",
                "matching {name} on {connect$host}",
                ", content must have a unique name."))
    } else if (length(content) == 0) {
      # create app
      content <- connect$content_create(
        name = name,
        title = title,
        ...
      )
      message(glue::glue("Creating NEW content {content$guid} ",
                   "with name {name} on {connect$host}"))
    } else {
      content <- content[[1]]
      message(glue::glue("Found EXISTING content {content$guid} with ",
      "name {name} on {connect$host}"))
    }
  }
  return(content)
}

random_name <- function(length = 25) {
  tolower(paste(sample(LETTERS, length, replace = TRUE), collapse = ""))
}

#' @export
dir_bundle <- function(path = ".", filename = "bundle.tar.gz") {
  before_wd <- getwd()
  setwd(path)
  on.exit(expr = setwd(before_wd), add = TRUE)
  
  utils::tar(tarfile = filename, files = ".", compression = "gzip", tar = "internal")
  
  return(fs::path_abs(filename))
}

#' @export
deploy_bundle <- function(connect, bundle_path, guid){
  #upload bundle
  new_bundle_id <- connect$content_upload(bundle_path = bundle_path, guid = guid)
  
  #activate bundle
  task_id <- connect$content_deploy(guid = guid, bundle_id = new_bundle_id)
  
  return(task_id)
}

#' @export
poll_task <- function(connect, task_id, wait = 1) {
  finished <- FALSE
  code <- -1
  first <- 0
  while (!finished) {
    task_data <- connect$get_task(task_id, wait = wait, first = first)
    finished <- task_data[["finished"]]
    code <- task_data[["code"]]
    first <- task_data[["last"]]
    
    lapply(task_data[["output"]], message)
  }
  
  if (code != 0) {
    msg <- task_data[["error"]]
    stop(msg)
  }
  invisible()
}
