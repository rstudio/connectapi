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

# Post-parse helpers for special column types. These are used by individual
# getter functions to coerce columns that jsonlite cannot infer automatically
# (e.g. byte sizes, 64-bit integers, epoch timestamps).

coerce_fs_bytes <- function(df, col) {
  if (col %in% names(df)) {
    df[[col]] <- fs::as_fs_bytes(df[[col]])
  }
  df
}

coerce_integer64 <- function(df, col) {
  if (col %in% names(df)) {
    df[[col]] <- bit64::as.integer64(df[[col]])
  }
  df
}

coerce_epoch_to_posixct <- function(df, cols) {
  for (col in intersect(cols, names(df))) {
    if (is.numeric(df[[col]])) {
      df[[col]] <- .POSIXct(as.double(df[[col]]), tz = Sys.timezone())
    }
  }
  df
}

coerce_to_character <- function(df, cols) {
  for (col in intersect(cols, names(df))) {
    if (is.numeric(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  df
}

parse_connectapi_typed <- function(data, datetime_cols = character()) {
  if (inherits(data, "data.frame")) {
    # Strip custom S3 classes to avoid dispatch loops (e.g., connect_list_hits
    # defines as_tibble which calls parse_connectapi_typed, causing recursion)
    class(data) <- "data.frame"
    df <- tibble::as_tibble(data)
  } else {
    # Fallback for list-of-lists (backward compat, non-simplified responses)
    df <- parse_connectapi(data)
  }
  for (col in intersect(datetime_cols, names(df))) {
    df[[col]] <- coerce_datetime(df[[col]])
  }
  df
}

# Coerce a column to POSIXct. Handles character (RFC 3339), numeric (epoch
# seconds), POSIXct (pass-through), and all-NA logical vectors.
coerce_datetime <- function(x) {
  if (is.null(x)) {
    .POSIXct(double(), tz = Sys.timezone())
  } else if (is.character(x)) {
    parse_connect_rfc3339(x)
  } else if (is.numeric(x)) {
    .POSIXct(as.double(x), tz = Sys.timezone())
  } else if (inherits(x, "POSIXct")) {
    x
  } else if (is.logical(x) && all(is.na(x))) {
    .POSIXct(rep(NA_real_, length(x)), tz = Sys.timezone())
  } else {
    stop("Cannot coerce ", class(x)[[1]], " to POSIXct", call. = FALSE)
  }
}

# Build a tibble column-by-column instead of row-by-row (via list_rbind).
# This avoids type conflicts when the same field is NULL in some rows and
# non-NULL in others: NULL -> NA, and unlist() coerces that NA to match the
# type of the non-null values in the same column.
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

  na_mask <- is.na(x)
  if (all(na_mask)) {
    return(.POSIXct(rep(NA_real_, length(x)), tz = Sys.timezone()))
  }

  result <- rep(NA_real_, length(x))
  xn <- x[!na_mask]

  # The date portion is always at fixed positions: YYYY-MM-DDTHH:MM:SS
  dates <- as.Date(substr(xn, 1, 10))
  hour <- as.integer(substr(xn, 12, 13))
  min <- as.integer(substr(xn, 15, 16))

  # Seconds (with optional fractional part) run from position 18 to just before
  # the timezone suffix. The suffix is either "Z" (1 char) or "+HH:MM" (6 chars).
  nc <- nchar(xn)
  is_utc <- endsWith(xn, "Z")
  tz_len <- ifelse(is_utc, 1L, 6L)
  sec <- as.double(substr(xn, 18, nc - tz_len))

  # Compute timezone offset in seconds for non-UTC timestamps
  tz_offset <- rep(0, length(xn))
  non_utc <- which(!is_utc)
  if (length(non_utc) > 0) {
    tz_str <- substr(xn[non_utc], nc[non_utc] - 5, nc[non_utc])
    tz_sign <- ifelse(substr(tz_str, 1, 1) == "+", 1, -1)
    tz_h <- as.integer(substr(tz_str, 2, 3))
    tz_m <- as.integer(substr(tz_str, 5, 6))
    tz_offset[non_utc] <- tz_sign * (tz_h * 3600L + tz_m * 60L)
  }

  # Build epoch seconds directly: date days * 86400 + time of day - tz offset
  epoch <- as.double(dates) * 86400 + hour * 3600 + min * 60 + sec - tz_offset
  result[!na_mask] <- epoch
  .POSIXct(result, tz = Sys.timezone())
}
