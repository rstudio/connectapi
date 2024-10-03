#' Get the Content Thumbnail
#'
#' `get_thumbnail` saves the content thumbnail to the given path (default: temp file).
#' `delete_thumbnail` removes the thumbnail.
#' `has_thumbnail` returns whether the content has an thumbnail
#'
#' @param content A content object.
#' @param path Optional. The path to the image on disk.
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

  contents <- httr::content(res, as = "raw")
  writeBin(contents, path)

  return(path)
}

#' @rdname get_thumbnail
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

#' @rdname get_thumbnail
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

#' @rdname get_thumbnail
#' @export
get_image <- function(content, path = NULL) {
  lifecycle::deprecate_warn("0.3.1", "get_image()", "get_thumbnail()")
  
  get_thumbnail(content, path)
}

#' @rdname get_thumbnail
#' @export
delete_image <- function(content, path = NULL) {
  lifecycle::deprecate_warn("0.3.1", "delete_image()", "delete_thumbnail()")

  if (!is.null(path)) {
    get_thumbnail(content, path)
  }
  
  delete_thumbnail(content, path)
}

#' @rdname get_thumbnail
#' @export
has_image <- function(content) {
  lifecycle::deprecate_warn("0.3.1", "has_image()", "has_thumbnail()")
  has_thumbnail(content)
}

#' Set the Content Thumbnail
#'
#' Set the Content thumbnail using a variety of methods.
#'
#' @param content A content object
#' @param path A file path or URL to an image
#'
#' @family content functions
#' @rdname set_thumbnail
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
  content
}


#' @rdname set_thumbnail
#' @export
set_image_path <- function(content, path) {
  lifecycle::deprecate_warn("0.3.1", "set_image_path()", "set_thumbnail()")
  set_thumbnail(content, path)
}


#' @rdname set_thumbnail
#' @export
set_image_url <- function(content, path) {
  lifecycle::deprecate_warn("0.3.1", "set_image_url()", "set_thumbnail()")
  set_thumbnail(content, path)
}


#' @rdname set_thumbnail
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
