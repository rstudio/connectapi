# because format(NULL, "%Y-%m") == "NULL"
safe_format <- function(expr, ...) {
  if (is.null(expr)) {
    return(NULL)
  } else {
    return(format(expr, ...))
  }
}

datetime_to_rfc3339 <- function(input) {
  tmp <- format(input, format = "%Y-%m-%dT%H:%M:%OS5%z")
  ln <- nchar(tmp)
  paste0(substr(tmp, 0, ln - 2), ":", substr(tmp, ln - 1, ln))
}

make_timestamp <- function(input) {
  if (is.character(input)) {
    # TODO: make sure this is the right timestamp format
    return(input)
  }
  safe_format(input, "%Y-%m-%dT%H:%M:%SZ")
}

swap_timestamp_format <- function(.col) {
  if (is.character(.col)) {
    gsub("([0-9]{4}-[0-9]{2}-[0-9]{2})T([0-9]{2}:[0-9]{2}:[0-9]{2}\\.*[0-9]*Z)", "\\1 \\2", .col)
  } else {
    .col
  }
}

ensure_columns <- function(.data, ...) {
  defaults <- rlang::list2(...)
  names <- names(defaults)
  for (i in seq_along(defaults)) {
    .data <- ensure_column(.data, defaults[[i]], names[[i]])
  }
  .data
}

ensure_column <- function(data, default, name) {
  stopifnot(length(default) == 1)
  col <- data[[name]]
  scoped_experimental_silence()
  if (rlang::is_null(col)) {
    col <- vctrs::vec_rep(default, nrow(data))
    col <- vctrs::vec_cast(col, default)
  } else {
    col <- swap_timestamp_format(col)
    if (vctrs::vec_is(default, NA_datetime_) && !vctrs::vec_is(col, NA_datetime_)) {
      # manual fix because vctrs::vec_cast cannot cast double -> datetime or char -> datetime
      col <- coerce_datetime(col, default, name = name)
    }
    if (inherits(default, "fs_bytes") && !inherits(col, "fs_bytes")) {
      col <- coerce_fsbytes(col, default)
    }
    if (inherits(default, "integer64") && !inherits(col, "integer64")) {
      col <- bit64::as.integer64(col)
    }
    if (inherits(default, "list") && !inherits(col, "list")) {
      col <- list(col)
    }
    col <- vctrs::vec_cast(col, default)
  }
  data[[name]] <- col
  data
}

parse_connectapi_typed <- function(data, ...) {
  ensure_columns(parse_connectapi(data), ...)
}

parse_connectapi <- function(data) {
  tibble::as_tibble(purrr::map_df(
    data,
    function(x) {
      purrr::map(
        .x = x,
        .f = function(y) {
          if (is.list(y)) {
            # empty list object gets null
            prep <- purrr::pluck(y, .default = NULL)
          } else {
            # otherwise NA
            prep <- purrr::pluck(y, .default = NA)
          }
          if (length(prep) > 1) {
            prep <- list(prep)
          }
          return(prep)
        }
      )
    }
  ))
}

#' @export
vec_cast.fs_bytes.integer <- function(x, to, ...) {
  warn_experimental("vec_cast.fs_bytes")
  fs::as_fs_bytes(x)
}

#' @export
vec_cast.fs_bytes.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x = x, to = to)
}

#' Cast to fs_bytes
#'
#' \lifecycle{deprecated}
#' This is a temporary placeholder because the functionality
#' does not exist yet in the `fs` package. Do not build dependencies
#' on `connectapi::vec-cast.fs_bytes`, as it will be removed without
#' warning in a future release.
#'
#' @param x Vectors to cast
#' @param to Type to cast to. If `NULL`, `x` will be returned as is
#' @param ... Dots for future extensions and should be empty
#'
#' @return A vector the same length as `x` with the same type as `to`, or an
#'   error if the cast is not possible.
#'
#' @export
vec_cast.fs_bytes <- function(x, to, ...) {
  warn_experimental("vec_cast.fs_bytes")
  UseMethod("vec_cast.fs_bytes")
}

coerce_fsbytes <- function(x, to, ...) {
  if (is.numeric(x)) {
    fs::as_fs_bytes(x)
  } else {
    vctrs::stop_incompatible_cast(x = x, to = to, x_arg = "x", to_arg = "to")
  }
}

# name - optional. Must be named, the name of the variable / column being converted
coerce_datetime <- function(x, to, ...) {
  tmp_name <- rlang::dots_list(...)[["name"]]
  if (is.null(tmp_name) || is.na(tmp_name) || !is.character(tmp_name)) {
    tmp_name <- "x"
  }
  if (is.numeric(x)) {
    vctrs::new_datetime(as.double(x), tzone = tzone(to))
  } else if (is.character(x)) {
    as.POSIXct(x, tz = tzone(to))
  } else if (inherits(x, "POSIXct")) {
    x
  } else if (all(is.logical(x) & is.na(x)) && length(is.logical(x) & is.na(x)) > 0) {
    NA_datetime_
  } else {
    vctrs::stop_incompatible_cast(x = x, to = to, x_arg = tmp_name, to_arg = "to")
  }
}

vec_cast.POSIXct.double <- function(x, to, ...) {
  warn_experimental("vec_cast.POSIXct.double")
  vctrs::new_datetime(x, tzone = tzone(to))
}

vec_cast.POSIXct.character <- function(x, to, ...) {
  as.POSIXct(x, tz = tzone(to))
}

tzone <- function(x) {
  attr(x, "tzone")[[1]] %||% ""
}


new_datetime <- function(x = double(), tzone = "") {
  tzone <- tzone %||% ""
  if (is.integer(x)) {
    x <- as.double(x)
  }
  stopifnot(is.double(x))
  stopifnot(is.character(tzone))
  structure(x, tzone = tzone, class = c("POSIXct", "POSIXt"))
}
