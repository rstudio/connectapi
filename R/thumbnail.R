#' Get content item thumbnail
#'
#' Download the thumbnail for a content item on Connect to a file on your
#' computer.
#'
#' @param content A content item.
#' @param path Optional. A path to a file used to write the thumbnail image. If
#' no path is provided, a temporary file with the correct file extension is
#' created. If the content item does not have a thumbnail, returns `NA`.
#' 
#' @returns The path to the image file.
#' 
#' @examples
#' \dontrun{
#' client <- connect()
#' item <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
#' thumbnail <- get_thumbnail(item)
#' }
#'
#' @family thumbnail functions
#' @family content functions
#' @export
get_thumbnail <- function(content, path = NULL) {
  validate_R6_class(content, "Content")
  guid <- content$content$guid
  con <- content$connect

  # Connect 2024.09.0 introduced public endpoints for content thumbnails. We
  # prefer those, falling back to the unversioned endpoints if unavailable.
  res <- con$GET(
    url = con$server_url("content", guid, "__thumbnail__"),
    parser = NULL
  )
  if (httr::status_code(res) == 404) {
    res <- con$GET(
      unversioned_url("applications", guid, "image"),
      parser = NULL
    )
  }
  con$raise_error(res)

  if (httr::status_code(res) == 204) {
    return(NA_character_)
  }

  # Guess file extension
  if (is.null(path)) {
    ct <- httr::headers(res)$`content-type`
    if (grepl("^image/", ct)) {
      # Just strip off 'image/'
      ext <- substr(ct, 7, nchar(ct))
      path <- tempfile(pattern = "content_image_", fileext = paste0(".", ext))
    } else {
      # Try png
      warning(glue::glue("Could not infer file extension from content type: {ct}. Using '.png'"))
      path <- tempfile(pattern = "content_image_", fileext = ".png")
    }
  }

  writeBin(httr::content(res, as = "raw"), path)

  return(path)
}


#' Delete content item thumbnail
#'
#' Delete the thumbnail from a content item on Connect.
#'
#' @param content A content item.
#' 
#' @returns The content item (invisibly).
#' 
#' @examples
#' \dontrun{
#' client <- connect()
#' item <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
#' thumbnail <- get_thumbnail(item)
#' }
#'
#' @family thumbnail functions
#' @family content functions
#' @export
delete_thumbnail <- function(content) {
  validate_R6_class(content, "Content")
  guid <- content$content$guid
  con <- content$connect

  # Connect 2024.09.0 introduced public endpoints for content thumbnails. We
  # prefer those, falling back to the unversioned endpoints if unavailable.
  res <- con$DELETE(
    v1_url("content", guid, "thumbnail"),
    parser = NULL
  )
  if (httr::status_code(res) == 404) {
    res <- con$DELETE(
      unversioned_url("applications", guid, "image"),
      parser = NULL
    )
  }
  con$raise_error(res)

  invisible(content)
}

#' Check content item thumbnail
#'
#' Check whether a content item has a thumbnail.
#'
#' @param content A content item.
#' 
#' @returns `TRUE` if the content item has a thumbnail, otherwise `FALSE`.
#' 
#' @examples
#' \dontrun{
#' client <- connect()
#' item <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
#' has_thumbnail(item)
#' }
#'
#' @family thumbnail functions
#' @family content functions
#' @export
has_thumbnail <- function(content) {
  validate_R6_class(content, "Content")
  guid <- content$content$guid
  con <- content$connect

  # Connect 2024.09.0 introduced public endpoints for content thumbnails. We
  # prefer those, falling back to the unversioned endpoints if unavailable.
  res <- con$GET(
    url = con$server_url("content", guid, "__thumbnail__"),
    parser = NULL
  )
  if (httr::status_code(res) == 404) {
    res <- con$GET(
      unversioned_url("applications", guid, "image"),
      parser = NULL
    )
  }
  con$raise_error(res)
  httr::status_code(res) != 204
}

#' Set content item thumbnail
#'
#' Set the thumbnail for a content item.
#'
#' @param content A content item.
#' @param path Either a path to a local file or a URL to an image available over
#' HTTP/HTTPS. If `path` is an HTTP or HTTPS URL, the image will first
#' be downloaded.
#' 
#' @returns The content item (invisibly).
#' 
#' @examples
#' \dontrun{
#' client <- connect()
#' item <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
#' set_thumbnail(item, "resources/image.png")
#' }
#'
#' @family thumbnail functions
#' @family content functions
#' @export
set_thumbnail <- function(content, path) {
  validate_R6_class(content, "Content")

  valid_path <- NULL
  if (file.exists(path)) {
    valid_path <- path
  } else {
    parsed <- httr::parse_url(path)
    print(parsed)
    if (!is.null(parsed$scheme) && parsed$scheme %in% c("http", "https")) {
      valid_path <- fs::file_temp(pattern = "image", ext = fs::path_ext(parsed[["path"]]))
      res <- httr::GET(path, httr::write_disk(valid_path))
      print(res)
      on.exit(unlink(valid_path))
      print(readBin(valid_path, "raw", n = 999))
      content$connect$raise_error(res)
    }
  }
  if (is.null(valid_path)) {
    stop(glue::glue("Could not locate image at path: {path}"))
  }

  guid <- content$content$guid
  con <- content$connect

  # Connect 2024.09.0 introduced public endpoints for content thumbnails. We
  # prefer those, falling back to the unversioned endpoints if unavailable.
  res <- con$PUT(
    path = v1_url("content", guid, "thumbnail"),
    body = httr::upload_file(valid_path),
    parser = NULL
  )
  if (httr::status_code(res) == 404) {
    res <- con$POST(
      path = unversioned_url("applications", guid, "image"),
      body = httr::upload_file(valid_path),
      parser = NULL
    )
  }
  con$raise_error(res)
  
  # return the input (in case it inherits more than just Content)
  invisible(content)
}


#' Get the Content Image
#' 
#' @description
#' 
#' \lifecycle{deprecated}
#' 
#' Please use [`get_thumbnail`],
#' [`delete_thumbnail`], and [`has_thumbnail`] instead.
#' 
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
  lifecycle::deprecate_warn("0.3.1", "get_image()", "get_thumbnail()")
  
  get_thumbnail(content, path)
}

#' @rdname get_image
#' @export
delete_image <- function(content, path = NULL) {
  lifecycle::deprecate_warn("0.3.1", "delete_image()", "delete_thumbnail()")

  if (!is.null(path)) {
    get_thumbnail(content, path)
  }
  
  delete_thumbnail(content, path)
}

#' @rdname get_image
#' @export
has_image <- function(content) {
  lifecycle::deprecate_warn("0.3.1", "has_image()", "has_thumbnail()")
  has_thumbnail(content)
}


#' Set the Content Image
#'
#' @description
#' 
#' \lifecycle{deprecated}
#' 
#' Please use [`set_thumbnail`] instead.
#' 
#' Set the Content Image using a variety of methods.
#'
#' @details NOTE: `set_image_webshot()` requires [webshot2::webshot()], but currently
#' skips and warns for any content that requires authentication until the
#' `webshot2` package supports authentication.
#'
#' @param content A content object
#' @param path The path to an image on disk
#' @param url The url for an image
#' @param ... Additional arguments passed on to [webshot2::webshot()]
#'
#' @rdname set_image
#' @family content functions
#' @export
set_image_path <- function(content, path) {
  lifecycle::deprecate_warn("0.3.1", "set_image_path()", "set_thumbnail()")
  set_thumbnail(content, path)
}


#' @rdname set_image
#' @export
set_image_url <- function(content, path) {
  lifecycle::deprecate_warn("0.3.1", "set_image_url()", "set_thumbnail()")
  set_thumbnail(content, path)
}


#' @rdname set_image
#' @export
set_image_webshot <- function(content, ...) {
  lifecycle::deprecate_warn("0.3.1", "set_image_webshot()", "set_thumbnail()")
  validate_R6_class(content, "Content")
  imgfile <- fs::file_temp(pattern = "webshot", ext = ".png")

  rlang::check_installed("webshot2", "to take screenshots of applications")
  content_details <- content$get_content_remote()

  # check if it is possible to take the webshot
  if (content_details$access_type != "all") {
    warning(glue::glue(
      "WARNING: unable to take webshot for content ",
      "'{content_details$guid}' because authentication is not possible yet. ",
      "Set access_type='all' to proceed."
    ))
    return(content)
  }

  # default args
  args <- rlang::list2(...)

  if (!"cliprect" %in% names(args)) {
    args["cliprect"] <- "viewport"
  }


  rlang::inject(webshot2::webshot(
    url = content_details$content_url,
    file = imgfile,
    !!!args
  ))

  set_thumbnail(content = content, path = imgfile)
}
