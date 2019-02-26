#' Bundle
#' 
#' An S6 class that represents a bundle
#' 
#' @family deploy
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

#' Task
#' 
#' An S6 class that represents a Task
#' 
#' @family deploy
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


#' Define a bundle from a Directory
#' 
#' Creates a bundle from a target directory. 
#' 
#' @param connect A Connect object
#' @param path The path to the directory to be bundled
#' @param filename The output bundle path
#' 
#' @return Bundle A bundle object
#' 
#' @family deploy
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

#' Define a bundle from a path (a tar.gz file)
#' 
#' @param connect A Connect object
#' @param path The path to a .tar.gz file
#' 
#' @return Bundle A bundle object
#' 
#' @family deploy
#' @export
bundle_path <- function(connect, path) {
  # need a check on filetype
  tar_path <- fs::path_abs(path)
  message(glue::glue("Bundling path {path}"))
  
  return(Bundle$new(connect = connect, path = tar_path))
}

#' Deploy a bundle
#' 
#' @param bundle A bundle object
#' @param name The unique name for the content on the server
#' @param title optional The title to be used for the content on the server
#' @param guid optional The GUID if the content already exists on the server
#' @param ... Additional arguments passed along to the content creation
#' 
#' @return Task A task object
#' 
#' @family deploy
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

#' Set the Image from a Path
#' 
#' @param content A content object
#' @param path The path to an image on disk
#' @param url The url for an image
#' @param ... Additional arguments passed on to methods
#' 
#' @rdname set_image
#' @family content
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

#' @rdname set_image
#' @export
set_image_url <- function(content, url) {
  parsed_url <- httr::parse_url(url)
  imgfile <- fs::file_temp(pattern = "image", ext = fs::path_ext(parsed_url[["path"]]))
  httr::GET(url, httr::write_disk(imgfile))
  
  set_image_path(content = content, path = imgfile)
}

#' @rdname set_image
#' @export
set_image_webshot <- function(content, ...) {
  imgfile <- fs::file_temp(pattern = "image", ext = ".png")
  webshot::webshot(content$content$url,
            file = imgfile,
            vwidth = 800,
            vheight = 600,
            cliprect = "viewport",
            key = server_key, 
            ...
            )
  
  set_image_path(content = content, path = imgfile)
}

#' Set the Vanity URL
#' 
#' Sets the Vanity URL for a piece of content.
#' 
#' @param content A content object
#' @param url The vanity URL to set
#' 
#' @family content
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
