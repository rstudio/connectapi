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

  # In the call to `safe_format`:
  # - The format specifier adds a literal "Z" to the end of the timestamp, which
  #   tells Connect "This is UTC".
  # - The `tz` argument tells R to produce times in the UTC time zone.
  # - The `usetz` argument says "Don't concatenate ' UTC' to the end of the string".
  safe_format(input, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC", usetz = FALSE)
}

ensure_columns <- function(.data, ptype, strict = FALSE) {
  # Given a prototype, ensure that all columns are present and cast to the correct type.
  # If a column is missing in .data, it will be created with all missing values of the correct type.
  # If a column is present in both, it will be cast to the correct type.
  # If a column is present in .data but not in ptype, it will be left as is.
  # If `strict == TRUE`, include only columns present in the ptype, in the order they occur.
  for (i in names(ptype)) {
    .data <- ensure_column(.data, ptype[[i]], i)
  }

  if (strict) {
    .data <- .data[, names(ptype), drop = FALSE]
  }

  .data
}

ensure_column <- function(data, default, name) {
  stopifnot(length(default) == 1)
  col <- data[[name]]
  if (rlang::is_null(col)) {
    col <- vctrs::vec_rep(default, nrow(data))
    col <- vctrs::vec_cast(col, default)
  } else {
    if (
      inherits(default, "POSIXct") && !inherits(col, "POSIXct")
    ) {
      # manual fix because vctrs::vec_cast cannot cast double -> datetime or char -> datetime
      col <- coerce_datetime(col, default, name = name)
    }

    if (inherits(default, "fs_bytes") && !inherits(col, "fs_bytes")) {
      col <- coerce_fsbytes(col, default)
    }

    if (inherits(default, "integer64") && !inherits(col, "integer64")) {
      col <- bit64::as.integer64(col)
    }

    if (is.character(default) && (is.integer(col) || is.double(col))) {
      if (is.double(col)) {
        col <- format(col, scientific = FALSE, trim = TRUE)
      } else {
        col <- as.character(col)
      }
    }

    if (inherits(default, "list") && !inherits(col, "list")) {
      col <- list(col)
    }

    col <- vctrs::vec_cast(col, default, x_arg = name)
  }
  data[[name]] <- col
  data
}

parse_connectapi_typed <- function(data, ptype, strict = FALSE) {
  ensure_columns(parse_connectapi(data), ptype, strict)
}

# Build a tibble column-by-column instead of row-by-row (via list_rbind).
# This avoids type conflicts when the same field is NULL in some rows and
# non-NULL in others: NULL -> NA, and unlist() coerces that NA to match the
# type of the non-null values in the same column. ensure_columns() handles
# any further type coercion (e.g. character -> POSIXct) after this step.
parse_connectapi <- function(data) {
  if (length(data) == 0) return(tibble::tibble())

  all_names <- unique(unlist(lapply(data, names), use.names = FALSE))
  n <- length(data)

  cols <- lapply(all_names, function(nm) {
    # .subset2 is the internal no-dispatch version of `[[`
    values <- lapply(data, .subset2, nm)
    nulls <- vapply(values, is.null, logical(1))

    # Determine column type from first non-NULL value
    is_list_col <- FALSE
    if (!all(nulls)) {
      first_val <- values[[which.min(nulls)]]
      is_list_col <- is.list(first_val) || length(first_val) > 1L
    }

    values[nulls] <- list(NA)

    if (is_list_col) {
      lapply(values, function(v) if (is.list(v)) v else list(v))
    } else {
      unlist(values, use.names = FALSE)
    }
  })
  names(cols) <- all_names
  tibble::new_tibble(cols, nrow = n)
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

  if (is.null(x)) {
    as.POSIXct(character(), tz = tzone(to))
  } else if (is.numeric(x)) {
    vctrs::new_datetime(as.double(x), tzone = tzone(to))
  } else if (is.character(x)) {
    parse_connect_rfc3339(x)
  } else if (inherits(x, "POSIXct")) {
    x
  } else if (
    all(is.logical(x) & is.na(x)) && length(is.logical(x) & is.na(x)) > 0
  ) {
    NA_datetime_
  } else {
    vctrs::stop_incompatible_cast(
      x = x,
      to = to,
      x_arg = tmp_name,
      to_arg = "to"
    )
  }
}

# nolint start: commented_code_linter
# Parses a character vector of dates received from Connect, using use RFC 3339,
# returning a vector of POSIXct datetimes.
#
# R parses character timestamps as ISO 8601. When specifying %z, it expects time
# zones to be specified as `-1400` to `+1400`.
#
# Connect's API sends times in a specific RFC 3339 format: indicating time zone
# offsets with `-14:00` to `+14:00`, and zero offset with `Z`.
# https://github.com/golang/go/blob/54fe0fd43fcf8609666c16ae6d15ed92873b1564/src/time/format.go#L86
# For example:
# - "2023-08-22T14:13:14Z"
# - "2023-08-22T15:13:14+01:00"
# - "2020-01-01T00:02:03-01:00"
# nolint end
parse_connect_rfc3339 <- function(x) {
  if (length(x) == 0) return(.POSIXct(double(), tz = Sys.timezone()))

  # The date portion is always at fixed positions: YYYY-MM-DDTHH:MM:SS
  dates <- as.Date(substr(x, 1, 10))
  hour <- as.integer(substr(x, 12, 13))
  min <- as.integer(substr(x, 15, 16))

  # Seconds (with optional fractional part) run from position 18 to just before
  # the timezone suffix. The suffix is either "Z" (1 char) or "+HH:MM" (6 chars).
  nc <- nchar(x)
  is_utc <- endsWith(x, "Z")
  tz_len <- ifelse(is_utc, 1L, 6L)
  sec <- as.double(substr(x, 18, nc - tz_len))

  # Compute timezone offset in seconds for non-UTC timestamps
  tz_offset <- rep(0, length(x))
  non_utc <- which(!is_utc)
  if (length(non_utc) > 0) {
    tz_str <- substr(x[non_utc], nc[non_utc] - 5, nc[non_utc])
    tz_sign <- ifelse(substr(tz_str, 1, 1) == "+", 1, -1)
    tz_h <- as.integer(substr(tz_str, 2, 3))
    tz_m <- as.integer(substr(tz_str, 5, 6))
    tz_offset[non_utc] <- tz_sign * (tz_h * 3600L + tz_m * 60L)
  }

  # Build epoch seconds directly: date days * 86400 + time of day - tz offset
  epoch <- as.double(dates) * 86400 + hour * 3600 + min * 60 + sec - tz_offset
  .POSIXct(epoch, tz = Sys.timezone())
}

vec_cast.POSIXct.double <- # nolint: object_name_linter
  function(x, to, ...) {
    warn_experimental("vec_cast.POSIXct.double")
    vctrs::new_datetime(x, tzone = tzone(to))
  }

vec_cast.POSIXct.character <- # nolint: object_name_linter
  function(x, to, ...) {
    as.POSIXct(x, tz = tzone(to))
  }

tzone <- function(x) {
  attr(x, "tzone")[[1]] %||% ""
}

vec_cast.character.integer <- # nolint: object_name_linter
  function(x, to, ...) {
    as.character(x)
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
