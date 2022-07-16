# Today set to 100MB. Should see if we can get this from Connect
max_bundle_size <- "100M"

#' Bundle
#'
#' An R6 class that represents a bundle
#'
#' @family R6 classes
#' @export
Bundle <- R6::R6Class(
  "Bundle",
  public = list(
    path = NULL,
    size = NULL,

    initialize = function(path) {
      self$path <- path
      self$size <- fs::file_size(path = path)
      if (fs::file_exists(path) && self$size > fs::as_fs_bytes(max_bundle_size)) {
        warning(glue::glue("Bundle size is greater than {max_bundle_size}. Please ensure your bundle is not including too much."))
      }
    },

    print = function(...) {
      cat("RStudio Connect Bundle: \n")
      cat("  Path: ", self$path, "\n", sep = "")
      cat("  Size: ", capture.output(self$size), "\n", sep = "")
      cat("\n")
      cat('bundle_path("', self$path, '")', "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

#' Task
#'
#' An R6 class that represents a Task
#'
#' @family R6 classes
#' @export
Task <- R6::R6Class(
  "Task",
  public = list(
    connect = NULL,
    task = NULL,
    data = NULL,
    initialize = function(connect, task) {
      validate_R6_class(connect, "Connect")
      self$connect <- connect
      # TODO: need to validate task (needs task_id)
      if ("id" %in% names(task) && ! "task_id" %in% names(task)) {
        # deal with different task interfaces on Connect
        task$task_id <- task$id
      }
      self$task <- task
    },
    get_connect = function() {
      self$connect
    },
    get_task = function() {
      self$task
    },
    add_data = function(data) {
      self$data <- data
      invisible(self)
    },
    get_data = function() {
      self$data
    },
    print = function(...) {
      cat("RStudio Connect Task: \n")
      cat("  Task ID: ", self$get_task()$task_id, "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

#' ContentTask
#'
#' An R6 class that represents a Task for a piece of Content
#'
#' @family R6 classes
#' @export
ContentTask <- R6::R6Class(
  "ContentTask",
  inherit = Content,
  # implements the "Task" interface too
  public = list(
    task = NULL,
    data = NULL,
    initialize = function(connect, content, task) {
      validate_R6_class(connect, "Connect")
      self$connect <- connect
      # TODO: need to validate content
      self$content <- content
      # TODO: need to validate task (needs task_id)
      self$task <- task
    },
    get_task = function() {
      self$task
    },
    add_data = function(data) {
      self$data <- data
      invisible(self)
    },
    get_data = function() {
      self$data
    },
    print = function(...) {
      cat("RStudio Connect Content Task: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  URL: ", dashboard_url_chr(self$get_connect()$server, self$get_content()$guid), "\n", sep = "")
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
#' @family R6 classes
#' @export
Vanity <- R6::R6Class(
  "Vanity",
  inherit = Content,
  public = list(
    vanity = NULL,
    initialize = function(connect, content, vanity) {
      validate_R6_class(connect, "Connect")
      self$connect <- connect
      # TODO: validate content
      self$content <- content
      # TODO: validate vanity (needs path_prefix)
      self$vanity <- vanity
    },
    get_vanity = function() {
      self$vanity
    },

    print = function(...) {
      cat("RStudio Connect Content Vanity URL: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  Vanity URL: ", self$get_vanity()$path, "\n", sep = "")
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
  stopifnot(fs::dir_exists(path))
  message(glue::glue("Bundling directory ({path})"))

  before_wd <- getwd()
  setwd(path)
  on.exit(expr = setwd(before_wd), add = TRUE)

  check_bundle_contents(".")
  utils::tar(tarfile = filename, files = ".", compression = "gzip", tar = "internal")

  tar_path <- fs::path_abs(filename)

  Bundle$new(path = tar_path)
}

check_bundle_contents <- function(dir) {
  all_contents <- fs::path_file(fs::dir_ls(dir))
  if (! "manifest.json" %in% all_contents) {
    stop(glue::glue("ERROR: no `manifest.json` file found in {dir}. Please generate with `rsconnect::writeManifest()`"))
  }
  if ("packrat.lock" %in% all_contents) {
    warning(glue::glue("WARNING: `packrat.lock` file found in {dir}. This can have unexpected consequences."))
  }
  if ("packrat" %in% all_contents) {
    warning(glue::glue("WARNING: `packrat` directory found in {dir}. This can have unexpected consequences"))
  }
}

#' Define a bundle from a static file (or files)
#'
#' Defines a bundle from static files. It copies all files to a temporary
#' directory, generates a basic manifest file (using the first file as the
#' "primary"), and bundles the directory.
#'
#' NOTE: the `rsconnect` package is required for this function to work properly.
#'
#' @param path The path to a file (or files) that will be used for the static bundle
#' @param filename The output bundle path
#'
#' @return Bundle A bundle object
#'
#' @family deployment functions
#' @export
bundle_static <- function(path, filename = fs::file_temp(pattern = "bundle", ext = ".tar.gz")) {
  tmpdir <- fs::file_temp("bundledir")
  dir.create(tmpdir, recursive = TRUE)
  all_files <- fs::file_copy(path = path, new_path = paste0(tmpdir, "/"))
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("ERROR: the `rsconnect` package needs to be installed to use this function")
  }
  # TODO: error if these files are not static?
  # TODO: a better way to get the primary document besides `all_files[[1]]`?
  rsconnect::writeManifest(appDir = tmpdir, appPrimaryDoc = fs::path_file(all_files[[1]]))
  bundle_dir(tmpdir, filename = filename)
}

#' Define a bundle from a path (a path directly to a tar.gz file)
#'
#' @param path The path to a .tar.gz file
#'
#' @return Bundle A bundle object
#'
#' @family deployment functions
#' @export
bundle_path <- function(path) {
  # TODO: need a check on filetype
  # TODO: a way to check that the .tar.gz has a manifest.json?
  # TODO: err if the file path does not exist
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
#' @param overwrite Optional. Default FALSE. Whether to overwrite the target location if it already exists
#'
#' @return Bundle A bundle object
#'
#' @family deployment functions
#' @export
download_bundle <- function(content, filename = fs::file_temp(pattern = "bundle", ext = ".tar.gz"), overwrite=FALSE) {
  validate_R6_class(content, "Content")

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
  from_connect$download_bundle(bundle_id = from_content$bundle_id, to_path = filename, overwrite=overwrite)

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
#' @param .pre_deploy An expression to execute before deploying the new bundle. The variables `content` and `bundle_id` are supplied
#'
#' @return Task A task object
#'
#' @family deployment functions
#' @export
deploy <- function(connect, bundle, name = create_random_name(), title = name, guid = NULL, ..., .pre_deploy = {}) {
  validate_R6_class(bundle, "Bundle")
  validate_R6_class(connect, "Connect")

  con <- connect

  message("Getting content endpoint")
  content <- content_ensure(connect = con, name = name, title = title, guid = guid, ...)

  message("Uploading bundle")
  # upload
  new_bundle_id <- con$content_upload(bundle_path = bundle$path, guid = content$guid)[["id"]]

  pre_deploy_expr <- rlang::enexpr(.pre_deploy)
  rlang::eval_bare(pre_deploy_expr, env = rlang::env(content = content_item(con, content$guid), bundle_id = new_bundle_id))

  message("Deploying bundle")
  # deploy
  task <- con$content_deploy(guid = content$guid, bundle_id = new_bundle_id)

  ContentTask$new(connect = con, content = content, task = task)
}

#' Get the Content Image
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
#' @family content functions
#' @export
get_image <- function(content, path = NULL) {
  warn_experimental("get_image")
  validate_R6_class(content, "Content")
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
#' @export
delete_image <- function(content, path = NULL) {
  warn_experimental("delete_image")
  validate_R6_class(content, "Content")
  guid <- content$get_content()$guid

  con <- content$get_connect()

  if (!is.null(path)) {
    scoped_experimental_silence()
    get_image(content, path)
  }

  res <- con$DELETE(
    glue::glue("applications/{guid}/image")
  )

  return(content)
}

#' @rdname get_image
#' @export
has_image <- function(content) {
  warn_experimental("has_image")
  validate_R6_class(content, "Content")
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

#' Set the Content Image
#'
#' \lifecycle{experimental}
#'
#' Set the Content Image using a variety of methods.
#'
#' NOTE: `set_image_webshot()` requires [`webshot2`](webshot2::webshot) package, but
#' currently skips and warns for any content that requires authentication until
#' the [`webshot2`](webshot2::webshot) package supports authentication.
#'
#' @param content A content object
#' @param path The path to an image on disk
#' @param url The url for an image
#' @param ... Additional arguments passed on to [`webshot2`](webshot2::webshot)
#'
#' @rdname set_image
#' @family content functions
#' @export
set_image_path <- function(content, path) {
  warn_experimental("set_image_path")
  validate_R6_class(content, "Content")
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
  validate_R6_class(content, "Content")
  parsed_url <- httr::parse_url(url)
  imgfile <- fs::file_temp(pattern = "image", ext = fs::path_ext(parsed_url[["path"]]))
  httr::GET(url, httr::write_disk(imgfile))

  set_image_path(content = content, path = imgfile)
}

#' @rdname set_image
#' @export
set_image_webshot <- function(content, ...) {
  warn_experimental("set_image_webshot")
  validate_R6_class(content, "Content")
  imgfile <- fs::file_temp(pattern = "webshot", ext = ".png")

  check_webshot()
  content_details <- content$get_content_remote()

  # check if it is possible to take the webshot
  if (content_details$access_type != "all") {
    warning(glue::glue(
      "WARNING: unable to take webshot for content ",
      "'{content_details$guid}' because authentication is not possible yet. ",
      "Set access_type='all' to proceed."))
    return(content)
  }

  # default args
  args <- rlang::list2(...)

  if(!"cliprect" %in% names(args)) {args["cliprect"] <- "viewport"}


  rlang::inject(webshot2::webshot(
    url = content_details$content_url,
    file = imgfile,
    !!!args
  ))

  set_image_path(content = content, path = imgfile)
}



check_webshot <- function() {
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("ERROR: the webshot2 package must be installed to use screenshots. Install using `install.packages('webshot2')`")
  }
}

#' Set the Vanity URL
#'
#' Sets the Vanity URL for a piece of content.
#'
#' @param content A Content object
#' @param url The path component of the URL
#' @param force optional. Default FALSE. Whether to force-reassign a vanity URL that already exists
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
set_vanity_url <- function(content, url, force = FALSE) {
  validate_R6_class(content, "Content")
  con <- content$get_connect()
  error_if_less_than(con, "1.8.6")
  guid <- content$get_content()$guid

  scoped_experimental_silence()
  # TODO: Check that the URL provided is appropriate

  res <- con$PUT(
    path = glue::glue("v1/content/{guid}/vanity"),
    body = list(
      path = url,
      force = force
    )
  )

  Vanity$new(connect = con, content = content$get_content_remote(), vanity = res)
}

#' Delete the Vanity URL
#'
#' Deletes the Vanity URL for a piece of content.
#'
#' @param content A Content object
#'
#' @family content functions
#' @export
delete_vanity_url <- function(content) {
  con <- content$get_connect()
  error_if_less_than(con, "1.8.6")
  guid <- content$get_content()$guid

  con$DELETE(glue::glue("/v1/content/{guid}/vanity"))

  content
}

#' Get the Vanity URL
#'
#' Gets the Vanity URL for a piece of content.
#'
#' @param content A Content object
#'
#' @return A character string (or NULL if not defined)
#'
#' @family content functions
#' @export
get_vanity_url <- function(content) {
  validate_R6_class(content, "Content")
  con <- content$get_connect()
  error_if_less_than(con, "1.8.6")
  guid <- content$get_content()$guid

  van <- tryCatch({
    con$GET(glue::glue("/v1/content/{guid}/vanity"))
  }, error = function(e) {
    # TODO: check to ensure that this error was expected
    return(NULL)
  })
  if (is.null(van)) {
    return(NULL)
  }

  return(van$path)
}

#' Swap the Vanity URL
#'
#' Swaps the Vanity URLs between two pieces of content
#'
#' @param from_content A Content object
#' @param to_content A Content object
#'
#' @family content functions
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

  if (is.null(from_vanity) && is.null(to_vanity)) {
    warning("Neither content has a Vanity URL. Exiting")
  } else {
    # swapping vanity URLs
    tmp_vanity <- paste0("vanity-url-swap-", create_random_name(length = 50))

    if (!is.null(from_vanity)) {
      set_vanity_url(from_content, tmp_vanity)
    } else {
      set_vanity_url(to_content, tmp_vanity)
    }

    if (!is.null(from_vanity)) {
      set_vanity_url(to_content, from_vanity)
    }
    if (!is.null(to_vanity)) {
      set_vanity_url(from_content, to_vanity)
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
#' Polls a task, waiting for information about a deployment. If the task has
#' results, the output will be a modified "Task" object with `task$get_data()`
#' available to retrieve the results.
#'
#' For a simple way to silence messages, set `callback = NULL`
#'
#' @param task A Task object
#' @param wait The interval to wait between polling
#' @param callback A function to be called for each message received. Set to NULL for no callback
#'
#' @return Task The Task object that was input
#'
#' @family deployment functions
#' @export
poll_task <- function(task, wait = 1, callback = message) {
  validate_R6_class(task, c("Task", "ContentTask", "VariantTask"))
  con <- task$get_connect()

  all_task_data <- list()

  finished <- FALSE
  code <- -1
  first <- 0
  while (!finished) {
    task_data <- con$task(task$get_task()$task_id, wait = wait, first = first)
    finished <- task_data[["finished"]]
    code <- task_data[["code"]]
    first <- task_data[["last"]]

    if (!is.null(callback)) {
      lapply(task_data[["output"]], callback)
    }
    all_task_data <- c(all_task_data, task_data[["output"]])
  }

  if (code != 0) {
    msg <- task_data[["error"]]
    # print the logs if there is no callback
    if (is.null(callback)) {
      lapply(all_task_data, message)
    }
    stop(msg)
  }
  if (!is.null(task_data[["result"]])) {
    task$add_data(task_data[["result"]])
  }
  task
}


#' Build a Dashboard URL from a Content Item
#'
#' Returns the URL for the content dashboard (opened to the selected pane).
#'
#' @param content [Content] A Content object
#' @param pane character The pane in the dashboard to link to
#'
#' @return character The dashboard URL for the content provided
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
#' @param connect_url character The base URL of the Connect server
#' @param content_guid character The guid for the content item in question
#' @param pane character The pane in the dashboard to link to
#'
#' @return character The dashboard URL for the content provided
#'
#' @family content functions
#' @export
dashboard_url_chr <- function(connect_url, content_guid, pane = "") {
  purrr::pmap_chr(
    list(x = connect_url, y = content_guid, z = pane),
    function(x, y, z) {
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
