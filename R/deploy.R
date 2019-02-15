#' @export
Bundle <- R6::R6Class(
  "Bundle",
  public = list(
    connect = NULL,
    path = NULL,
    
    initialize = function(connect, path) {
      if (!R6::is.R6(connect) | class(con)[1] != "Connect") {
        stop("connect must be an R6 Connect object")
      }
      self$connect = connect
      self$path = path
    }
  )
)

#' @export
Task <- R6::R6Class(
  "Task",
  public = list(
    connect = NULL,
    content = NULL,
    task = NULL,
    
    initialize = function(connect, content, task) {
      if (!R6::is.R6(connect) | class(con)[1] != "Connect") {
        stop("connect must be an R6 Connect object")
      }
      self$connect = connect
      self$content = content
      self$task = task
    }
  )
)


# should this default to a temp file? Probably...?
#' @export
bundle_dir <- function(connect, path = ".", filename = fs::file_temp(pattern = "bundle", ext = ".tar.gz")) {
  before_wd <- getwd()
  setwd(path)
  on.exit(expr = setwd(before_wd), add = TRUE)
  
  message("Bundling directory {path}")
  utils::tar(tarfile = filename, files = ".", compression = "gzip", tar = "internal")
  
  tar_path <- fs::path_abs(filename)
  
  return(Bundle$new(connect = connect, path = tar_path))
}

#' @export
bundle_path <- function(connect, path) {
  tar_path <- fs::path_abs(path)
  message(glue::gleu("Bundling path {path}"))
  
  return(Bundle$new(connect = connect, path = tar_path))
}

bundle_static <- function(connect, path) {
  raw_json <- jsonlite::fromJSON(system.file("static.json", package = "connectapi"))
  raw_json$metadata$primary_html <- path
  
  message(glue::glue("Bundling file {path}"))
  tmp_dir <- fs::file_temp(pattern = "bundle")
  fs::dir_create(tmp_dir)
  tmp_file <- fs::file_copy(
    path = fs::path_file(path), 
    new_path = fs::path(tmp_dir, fs::path_file(path))
    )
  
  output_json <- jsonlite::toJSON(
    raw_json, 
    auto_unbox = TRUE, 
    pretty = TRUE
  )
  update_json <- gsub(
    pattern = "\\{\\}",
    replacement = "null",
    output_json
  )
  writeLines(update_json, fs::path(tmp_dir, "manifest.json"))
  
  bundle_dir(connect = connect, path = tmp_dir)
}

#' @export
deploy <- function(bundle, name = random_name(), title = name, guid = NULL, ...) {
  con <- bundle$connect
  
  message("Getting content endpoint")
  content <- content_ensure(connect = con, name = name, title = title, guid = guid, ...)
  
  message("Uploading bundle")
  # upload
  new_bundle_id <- con$content_upload(bundle_path = bundle$path, guid = content$guid)[["bundle_id"]]
  
  message("Deploying bundle")
  # deploy
  task <- con$content_deploy(guid = content$guid, bundle_id = new_bundle_id)
  
  return(Task$new(connect = con, content = content, task = task))
}

# should this be content_image_path ? maybe?
#' @export
set_image_path <- function(content, path) {
  guid <- content$content$guid
  
  con <- content$connect
  
  res <- con$POST(
    path = glue::glue("/applications/{guid}/image"),
    body = httr::upload_file(path)
    )
  return(content)
}

#' @export
set_vanity_url <- function(content, url) {
  guid <- content$content$guid
  
  con <- content$connect
  
  con$POST(
    path = "vanities",
    body = list(
      app_guid = guid,
      path_prefix = url
    )
  )
}
