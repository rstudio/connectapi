#' Bundle
#' 
#' An R6 class that represents a bundle
#' 
#' @family deployR6 functions
#' @export
Bundle <- R6::R6Class(
  "Bundle",
  public = list(
    path = NULL,
    
    initialize = function(path) {
      self$path <- path
    },
    
    print = function(...) {
      cat("RStudio Connect Bundle: \n")
      cat("  Path: ", self$path, "\n", sep = "")
      cat("\n")
      cat('bundle_path("', self$path, '")', "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

#' Content
#' 
#' An R6 class that represents content
#' 
#' @family deployR6 functions
#' @export
Content <- R6::R6Class(
  "Content",
  public = list(
    connect = NULL,
    content = NULL,
    
    initialize = function(connect, content) {
      validate_R6_class("Connect", connect)
      self$connect <- connect
      # TODO: need to check that content has
      # at least guid, url, title to be functional
      self$content <- content
    },
    get_connect = function(){self$connect},
    get_content = function(){self$content},
    get_dashboard_url = function(pane = ""){
      dashboard_url_chr(self$connect$host, self$content$guid, pane = pane)
    },
    print = function(...) {
      cat("RStudio Connect Content: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  Content URL: ", self$get_content()$url, "\n", sep = "")
      cat("  Content Title: ", self$get_content()$title, "\n", sep = "")
      cat("\n")
      cat('content_item(client, guid = "', self$get_content()$guid, '")', "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

#' Task
#' 
#' An R6 class that represents a Task
#' 
#' @family deployR6 functions
#' @export
Task <- R6::R6Class(
  "Task",
  inherit = Content,
  public = list(
    task = NULL,
    initialize = function(connect, content, task) {
      validate_R6_class("Connect", connect)
      self$connect <- connect
      # TODO: need to validate content
      self$content <- content
      # TODO: need to validate task (needs id)
      self$task <- task
    },
    get_task = function(){self$task},
    
    print = function(...) {
      cat("RStudio Connect Task: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  Task ID: ", self$get_task()$task_id, "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

#' Vanity
#' 
#' An R6 class that represents a Vanity URL
#' 
#' @family deployR6 functions
#' @export
Vanity <- R6::R6Class(
  "Vanity",
  inherit = Content,
  public = list(
    vanity = NULL,
    initialize = function(connect, content, vanity) {
      validate_R6_class("Connect", connect)
      self$connect <- connect
      # TODO: validate content
      self$content <- content
      # TODO: validate vanity (needs path_prefix)
      self$vanity <- vanity
    },
    get_vanity = function(){self$vanity},
    
    print = function(...) {
      cat("RStudio Connect Content Vanity URL: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  Vanity URL: ", self$get_vanity()$path_prefix, "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)


#' Define a bundle from a Directory
#' 
#' Creates a bundle from a target directory. 
#' 
#' @param path The path to the directory to be bundled
#' @param filename The output bundle path
#' 
#' @return Bundle A bundle object
#' 
#' @family deployment functions
#' @export
bundle_dir <- function(path = ".", filename = fs::file_temp(pattern = "bundle", ext = ".tar.gz")) {
  
  # TODO: check for manifest.json
  
  before_wd <- getwd()
  setwd(path)
  on.exit(expr = setwd(before_wd), add = TRUE)
  
  message(glue::glue("Bundling directory {path}"))
  utils::tar(tarfile = filename, files = ".", compression = "gzip", tar = "internal")
  
  tar_path <- fs::path_abs(filename)
  
  Bundle$new(path = tar_path)
}

#' Define a bundle from a path (a tar.gz file)
#' 
#' @param path The path to a .tar.gz file
#' 
#' @return Bundle A bundle object
#' 
#' @family deployment functions
#' @export
bundle_path <- function(path) {
  # need a check on filetype
  tar_path <- fs::path_abs(path)
  message(glue::glue("Bundling path {path}"))
  
  Bundle$new(path = tar_path)
}

#' Download a Bundle from Deployed Connect Content
#' 
#' Downloads a Content item's active bundle
#' 
#' @param content A Content object
#' @param filename The output bundle path
#' 
#' @return Bundle A bundle object
#' 
#' @family deployment functions
#' @export
download_bundle <- function(content, filename = fs::file_temp(pattern = "bundle", ext = ".tar.gz")) {
  validate_R6_class("Content", content)
  
  from_connect <- content$get_connect()
  from_content <- content$get_content()
  
  if (is.null(from_content$bundle_id)) {
    stop(
    glue::glue(
      "This content has no bundle_id.",
      "It has never been successfully deployed.",
      "See {content$get_dashboard_url()} for more information.",
      .sep = " "
      )
    )
  }
  
  message("Downloading bundle")
  from_connect$download_bundle(bundle_id = from_content$bundle_id, to_path = filename)
  
  Bundle$new(path = filename)
}

#' Deploy a bundle
#' 
#' @param connect A Connect object
#' @param bundle A Bundle object
#' @param name The unique name for the content on the server
#' @param title optional The title to be used for the content on the server
#' @param guid optional The GUID if the content already exists on the server
#' @param ... Additional arguments passed along to the content creation
#' 
#' @return Task A task object
#' 
#' @family deployment functions
#' @export
deploy <- function(connect, bundle, name = random_name(), title = name, guid = NULL, ...) {
  validate_R6_class("Bundle", bundle)
  validate_R6_class("Connect", connect)
  
  con <- connect
  
  message("Getting content endpoint")
  content <- content_ensure(connect = con, name = name, title = title, guid = guid, ...)
  
  message("Uploading bundle")
  # upload
  new_bundle_id <- con$content_upload(bundle_path = bundle$path, guid = content$guid)[["bundle_id"]]
  
  message("Deploying bundle")
  # deploy
  task <- con$content_deploy(guid = content$guid, bundle_id = new_bundle_id)
  
  Task$new(connect = con, content = content, task = task)
}

#' Get the Image
#' 
#' \lifecycle{experimental}
#' `get_image` saves the content image to the given path (default: temp file).
#' `delete_image` removes the image (optionally saving to the given path)
#' `has_image` returns whether the content has an image
#' 
#' @param content A content object
#' @param path optional. The path to the image on disk
#' 
#' @rdname get_image
#' @family deployment functions
#' @export
get_image <- function(content, path = NULL) {
  warn_experimental("get_image")
  validate_R6_class("Content", content)
  guid <- content$get_content()$guid
  
  con <- content$get_connect()
  
  res <- con$GET_RESULT(
    path = glue::glue("applications/{guid}/image"), 
    writer = httr::write_memory()
    )
  
  if (httr::status_code(res) == 204) {
    return(NA)
  }
  
  # guess file extension
  if (is.null(path)) {
    ct <- httr::headers(res)$`content-type`
    if (grepl("^image/", ct)) {
      # just strip off 'image/'
      ext <- substr(ct, 7, nchar(ct))
      path <- fs::file_temp(pattern = "content_image_", ext = ext)
    } else {
      # try png
      warning(glue::glue("Could not infer file extension from content type: {ct}. Using '.png'"))
      path <- fs::file_temp(pattern = "content_image_", ext = ".png")
    }
  }
  
  writeBin(httr::content(res, as = "raw"), path)
  
  return(fs::as_fs_path(path))
}

#' @rdname get_image
#' @family content
#' @export
delete_image <- function(content, path = NULL) {
  warn_experimental("delete_image")
  validate_R6_class("Content", content)
  guid <- content$get_content()$guid
  
  con <- content$get_connect()
  
  if (!is.null(path)) {
    scoped_experimental_silence()
    img_path <- get_image(content, path)
  }
  
  res <- con$DELETE(
    glue::glue("applications/{guid}/image")
  )
  
  return(content)
}

#' @rdname get_image
#' @family content
#' @export
has_image <- function(content) {
  warn_experimental("has_image")
  validate_R6_class("Content", content)
  guid <- content$get_content()$guid
  
  con <- content$get_connect()
  
  res <- con$GET_RESULT(
    glue::glue("applications/{guid}/image")
    )
  
  if (httr::status_code(res) == 204) {
    FALSE
  } else {
    TRUE
  }
}

#' Set the Image from a Path
#' 
#' @param content A content object
#' @param path The path to an image on disk
#' @param url The url for an image
#' @param ... Additional arguments passed on to methods
#' 
#' @rdname set_image
#' @family content functions
#' @export
set_image_path <- function(content, path) {
  warn_experimental("set_image_path")
  validate_R6_class("Content", content)
  guid <- content$get_content()$guid
  
  con <- content$get_connect()
  
  res <- con$POST(
    path = glue::glue("applications/{guid}/image"),
    body = httr::upload_file(path)
    )
  
  # return the input (in case it inherits more than just Content)
  content
}

#' @rdname set_image
#' @export
set_image_url <- function(content, url) {
  warn_experimental("set_image_url")
  validate_R6_class("Content", content)
  parsed_url <- httr::parse_url(url)
  imgfile <- fs::file_temp(pattern = "image", ext = fs::path_ext(parsed_url[["path"]]))
  httr::GET(url, httr::write_disk(imgfile))
  
  set_image_path(content = content, path = imgfile)
}

# #' @rdname set_image
# #' @export
set_image_webshot <- function(content, ...) {
  warn_experimental("set_image_webshot")
  validate_R6_class("Content", content)
  imgfile <- fs::file_temp(pattern = "image", ext = ".png")
  webshot::webshot(content$get_content()$url,
            file = imgfile,
            vwidth = 800,
            vheight = 600,
            cliprect = "viewport",
            key = content$get_connect()$api_key, 
            ...
            )
  
  set_image_path(content = content, path = imgfile)
}

#' Set the Vanity URL
#' 
#' Sets the Vanity URL for a piece of content.
#' 
#' @param content A Content object
#' @param url The path component of the URL
#' 
#' @return An updated Content object
#' 
#' @examples
#' \dontrun{
#' bnd <- bundle_dir("~/my/directory")
#' connect() %>% 
#'   deploy(bnd) %>% 
#'   set_vanity_url("a/vanity/url")
#' }
#' 
#' @family content functions
#' @export
set_vanity_url <- function(content, url) {
  warn_experimental("set_vanity_url")
  validate_R6_class("Content", content)
  guid <- content$get_content()$guid
  
  scoped_experimental_silence()
  # TODO: Check that the URL provided is appropriate
  
  current_vanity <- get_vanity_url(content)

  con <- content$get_connect()
  
  if (!inherits(current_vanity, "Vanity")) {
    # new
    res <- con$POST(
      path = "vanities",
      body = list(
        app_guid = guid,
        path_prefix = url
      )
  )
  } else {
    # update
    res <- con$PUT(
      path = glue::glue("vanities/{current_vanity$get_vanity()$id}"),
      body = list(
        path_prefix = url
      )
    )
  }
  
  # update content/vanity definition
  updated_content <- con$content(guid = guid)
  updated_van_res <- con$GET(glue::glue("/applications/{guid}"))
  updated_van <- updated_van_res$vanities[[1]]
  updated_van$app_id <- NULL
  updated_van$app_guid <- guid
  
  van <- Vanity$new(connect = con, content = updated_content, vanity = updated_van)
  
  van
}


#' Get the Vanity URL
#' 
#' Gets the Vanity URL for a piece of content.
#' 
#' @param content A Content object
#' 
#' @family content functions
#' @export
get_vanity_url <- function(content) {
  warn_experimental("get_vanity_url")
  con <- content$get_connect()
  guid <- content$get_content()$guid
  
  res <- con$GET(glue::glue("/applications/{guid}"))
  
  # just grab the first?
  van <- res$vanities[[1]]
  
  if (is.null(van)) {
    content
  } else {
    van$app_id <- NULL
    van$app_guid <- guid
    Vanity$new(connect = con, content = content$get_content(), vanity = van)
  }
}

#' Swap the Vanity URL
#' 
#' \lifecycle{experimental}
#' Swaps the Vanity URLs between two pieces of content
#' 
#' @param from_content A Content object
#' @param to_content A Content object
#' 
#' @family content
#' @export
swap_vanity_url <- function(from_content, to_content) {
  warn_experimental("swap_vanity_url")
  scoped_experimental_silence()
  # TODO: Add prompt if in an interactive session
  # TODO: Add pretty print output of what is happening
  # TODO: Test error cases super thoroughly!!
  # TODO: Do a "dry run" of sorts...? Check privileges... etc...
  # TODO: Do the changes within a TryCatch so we can undo...?
  # TODO: Need a way to "unset" a vanity URL
  
  from_vanity <- get_vanity_url(from_content)
  to_vanity <- get_vanity_url(to_content)
  
  if (!inherits(from_vanity, "Vanity") && !inherits(to_vanity, "Vanity")) {
    warning("Neither content has a Vanity URL. Exiting")
  } else {
    # swapping vanity URLs
    tmp_vanity <- paste0("vanity-url-swap-", random_name(length = 50))
    
    if (inherits(from_vanity, "Vanity")) {
      from_vanity_url <- from_vanity$get_vanity()$path_prefix
    } else {
      from_vanity_url <- NA
    }
    
    if (inherits(to_vanity, "Vanity")) {
      to_vanity_url <- to_vanity$get_vanity()$path_prefix
    } else {
      to_vanity_url <- NA
    }
    
    if (!is.na(from_vanity_url)) {
      set_vanity_url(from_vanity, tmp_vanity)
    } else {
      set_vanity_url(to_vanity, tmp_vanity)
    }
    
    if (!is.na(from_vanity_url)) {
      set_vanity_url(to_vanity, from_vanity_url)
    }
    if (!is.na(to_vanity_url)) {
      set_vanity_url(from_vanity, to_vanity_url)
    }
    
    from_vanity <- get_vanity_url(from_content)
    to_vanity <- get_vanity_url(to_content)
  }
  
  return(
    list(
      from = from_vanity,
      to = to_vanity
    )
  )
}

#' Poll Task
#' 
#' Polls a task, waiting for information about a deployment
#' 
#' @param task A Task object
#' @param wait The interval to wait between polling
#' 
#' @return Task The Task object that was input
#' 
#' @family deployment functions
#' @export
poll_task <- function(task, wait = 1) {
  validate_R6_class("Task", task)
  con <- task$get_connect()
  
  finished <- FALSE
  code <- -1
  first <- 0
  while (!finished) {
    task_data <- con$task(task$get_task()$task_id, wait = wait, first = first)
    finished <- task_data[["finished"]]
    code <- task_data[["code"]]
    first <- task_data[["last"]]
    
    lapply(task_data[["output"]], message)
  }
  
  if (code != 0) {
    msg <- task_data[["error"]]
    stop(msg)
  }
  task
}

#' Get Content Item
#' 
#' Returns a single content item based on guid
#' 
#' @param connect A Connect object
#' @param guid The GUID for the content item to be retrieved
#' 
#' @return A Content object for use with other content endpoints
#' 
#' @family content functions
#' @export
content_item <- function(connect, guid) {
  # TODO : think about how to handle if GUID does not exist
  validate_R6_class("Connect", connect)
  
  res <- connect$get_connect()$content(guid)
  
  Content$new(connect = connect, content = res)
}

#' Build a Dashboard URL from a Content Item
#' 
#' Returns the URL for the content dashboard (opened to the selected pane).
#' 
#' @param content [R6:Content] A Content object
#' @param pane [character] The pane in the dashboard to link to
#' 
#' @return [character] The dashboard URL for the content provided
#' 
#' @family content functions
#' @export
dashboard_url <- function(content, pane = "") {
  content$get_dashboard_url(pane = pane)
}

#' Build a Dashboard URL from Character Vectors
#' 
#' Returns the URL for the content dashboard (opened to the selected pane).
#' NOTE: this takes a character object for performance optimization.
#' 
#' @param connect_url [character] The base URL of the Connect server
#' @param content_guid [character] The guid for the content item in question
#' @param pane [character] The pane in the dashboard to link to
#' 
#' @return [character] The dashboard URL for the content provided
#' 
#' @family content functions
#' @export
dashboard_url_chr <- function(connect_url, content_guid, pane = "") {
  purrr::pmap_chr(
    list(x = connect_url, y = content_guid, z = pane),
    function(x,y,z) {
      paste(
        x,
        "connect",
        "#",
        "apps",
        y,
        z,
        sep = "/"
      )
    }
  )
}
