# because format(NULL, "%Y-%m") == "NULL"
safe_format <- function(expr, ...) {
  if (is.null(expr)) {
    return(NULL)
  } else {
    return(format(expr, ...))
  }
}

make_timestamp <- function(input) {
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
      col <- coerce_datetime(col, default)
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
  purrr::map_df(
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
  )
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

coerce_datetime <- function(x, to, ...) {
  if (is.double(x)) {
    vctrs::new_datetime(x, tzone = tzone(to))
  } else if (is.character(x)) {
    as.POSIXct(x, tz = tzone(to))
  } else {
    vctrs::stop_incompatible_cast(x = x, to = to, x_arg = "x", to_arg = "to")
  }
}

vec_cast.POSIXct.double <- function(x, to, ...) {
  warn_experimental("vec_cast.POSIXct.double")
  vctrs::new_datetime(x, tzone = tzone(to))
}

vec_cast.POSIXct.character <- function (x, to, ...) 
{
  as.POSIXct(x, tz = tzone(to))
}

tzone <- function (x) 
{
  attr(x, "tzone")[[1]] %||% ""
}


new_datetime <- function (x = double(), tzone = "") 
{
  tzone <- tzone %||% ""
  if (is.integer(x)) {
    x <- as.double(x)
  }
  stopifnot(is.double(x))
  stopifnot(is.character(tzone))
  structure(x, tzone = tzone, class = c("POSIXct", "POSIXt"))
}
