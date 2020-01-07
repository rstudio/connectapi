# because format(NULL, "%Y-%m") == "NULL"
safe_format <- function(expr, ...) {
  if (is.null(expr)) {
    return(NULL)
  } else {
    return(format(expr, ...))
  }
}

make_timestamp <- function(input) {
  safe_format(input, '%Y-%m-%dT%H:%M:%SZ')
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
    col <- rep_len(default, nrow(data))
    col <- vctrs::vec_cast(col, default)
  } else {
    col <- swap_timestamp_format(col)
    col <- vctrs::vec_cast(col, default)
  }
  data[[name]] <- col
  data
}

parse_connectapi_typed <- function(data, ...) {
  ensure_columns(parse_connectapi(data), ...)
}

parse_connectapi <- function(data){
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
          if (length(prep) > 1)
            prep <- list(prep)
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

#' @export
vec_cast.fs_bytes <- function(x, to, ...) {
  warn_experimental("vec_cast.fs_bytes")
  UseMethod("vec_cast.fs_bytes")
}
