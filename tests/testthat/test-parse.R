test_that("parse_connect_rfc3339() parses timestamps with offsets as expected", {
  x_mixed <- c(
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z",
    "2023-08-22T15:13:14+01:00",
    "2020-01-01T00:02:03-01:00"
  )

  x_zero_offset <- c(
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z"
  )

  x_plus_one <- c(
    "2023-08-22T15:13:14+01:00",
    "2020-01-01T02:02:03+01:00"
  )

  x_minus_one <- c(
    "2023-08-22T13:13:14-01:00",
    "2020-01-01T00:02:03-01:00"
  )

  single_zero_offset <- "2023-08-22T14:13:14Z"

  single_offset <- "2023-08-22T15:13:14+01:00"

  withr::local_envvar(TZ = "America/New_York")
  expected <- as.POSIXct(strptime(
    c(
      "2023-08-22T14:13:14+0000",
      "2020-01-01T01:02:03+0000"
    ),
    format = "%Y-%m-%dT%H:%M:%S%z",
    tz = Sys.timezone()
  ))
  expect_identical(parse_connect_rfc3339(x_mixed), rep(expected, 2))
  expect_identical(parse_connect_rfc3339(x_zero_offset), expected)
  expect_identical(parse_connect_rfc3339(x_plus_one), expected)
  expect_identical(parse_connect_rfc3339(x_minus_one), expected)
  expect_identical(parse_connect_rfc3339(single_zero_offset), expected[1])
  expect_identical(parse_connect_rfc3339(single_offset), expected[1])

  withr::local_envvar(TZ = "UTC")
  expected <- as.POSIXct(strptime(
    c(
      "2023-08-22T14:13:14+0000",
      "2020-01-01T01:02:03+0000"
    ),
    format = "%Y-%m-%dT%H:%M:%S%z",
    tz = Sys.timezone()
  ))
  expect_identical(parse_connect_rfc3339(x_mixed), rep(expected, 2))
  expect_identical(parse_connect_rfc3339(x_zero_offset), expected)
  expect_identical(parse_connect_rfc3339(x_plus_one), expected)
  expect_identical(parse_connect_rfc3339(x_minus_one), expected)
  expect_identical(parse_connect_rfc3339(single_zero_offset), expected[1])
  expect_identical(parse_connect_rfc3339(single_offset), expected[1])

  withr::local_envvar(TZ = "Asia/Tokyo")
  expected <- as.POSIXct(strptime(
    c(
      "2023-08-22T14:13:14+0000",
      "2020-01-01T01:02:03+0000"
    ),
    format = "%Y-%m-%dT%H:%M:%S%z",
    tz = Sys.timezone()
  ))
  expect_identical(parse_connect_rfc3339(x_mixed), rep(expected, 2))
  expect_identical(parse_connect_rfc3339(x_zero_offset), expected)
  expect_identical(parse_connect_rfc3339(x_plus_one), expected)
  expect_identical(parse_connect_rfc3339(x_minus_one), expected)
  expect_identical(parse_connect_rfc3339(single_zero_offset), expected[1])
  expect_identical(parse_connect_rfc3339(single_offset), expected[1])
})


test_that("parse_connect_rfc3339() handles fractional seconds", {
  withr::local_envvar(TZ = "UTC")
  expected <- as.POSIXct(strptime(
    c(
      "2024-12-06T19:09:29.948016766+0000",
      "2024-12-06T19:09:29.948070345+0000"
    ),
    format = "%Y-%m-%dT%H:%M:%OS%z",
    tz = Sys.timezone()
  ))

  x <- c("2024-12-06T19:09:29.948016766Z", "2024-12-06T19:09:29.948070345Z")

  expect_identical(parse_connect_rfc3339(x), expected)
})

test_that("parse_connect_rfc3339() handles NA values", {
  withr::local_envvar(TZ = "UTC")
  result <- parse_connect_rfc3339(c("2023-08-22T14:13:14Z", NA))
  expect_s3_class(result, "POSIXct")
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))

  # All NA
  result <- parse_connect_rfc3339(c(NA_character_, NA_character_))
  expect_s3_class(result, "POSIXct")
  expect_true(all(is.na(result)))
})

test_that("make_timestamp produces expected output", {
  x_mixed <- c(
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z",
    "2023-08-22T15:13:14+01:00",
    "2020-01-01T00:02:03-01:00"
  )

  x_zero_offset <- c(
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z"
  )

  x_plus_one <- c(
    "2023-08-22T15:13:14+01:00",
    "2020-01-01T02:02:03+01:00"
  )

  x_minus_one <- c(
    "2023-08-22T13:13:14-01:00",
    "2020-01-01T00:02:03-01:00"
  )

  single_zero_offset <- "2023-08-22T14:13:14Z"

  single_offset <- "2023-08-22T15:13:14+01:00"

  outcome <- c(
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z"
  )

  withr::local_envvar(TZ = "America/New_York")
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_mixed)),
    rep(outcome, 2)
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_zero_offset)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_plus_one)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_minus_one)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(single_zero_offset)),
    outcome[1]
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(single_offset)),
    outcome[1]
  )
  expect_equal(make_timestamp(outcome), outcome)

  withr::local_envvar(TZ = "UTC")
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_mixed)),
    rep(outcome, 2)
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_zero_offset)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_plus_one)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_minus_one)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(single_zero_offset)),
    outcome[1]
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(single_offset)),
    outcome[1]
  )
  expect_equal(make_timestamp(outcome), outcome)

  withr::local_envvar(TZ = "Asia/Tokyo")
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_mixed)),
    rep(outcome, 2)
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_zero_offset)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_plus_one)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(x_minus_one)),
    outcome
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(single_zero_offset)),
    outcome[1]
  )
  expect_equal(
    make_timestamp(parse_connect_rfc3339(single_offset)),
    outcome[1]
  )
  expect_equal(make_timestamp(outcome), outcome)
})

test_that("make_timestamp is safe for strings", {
  expect_equal(make_timestamp("hello"), "hello")
  expect_equal(make_timestamp(rep("hello", 5)), rep("hello", 5))

  expect_equal(make_timestamp(NA_character_), NA_character_)
})

test_that("make_timestamp converts to character", {
  ts <- .POSIXct(NA_real_, tz = Sys.timezone())
  expect_type(make_timestamp(ts), "character")
})

test_that("parse_connectapi handles mixed null/non-null character values", {
  data <- list(
    list(guid = "aaa", bundle_id = NULL, name = "first"),
    list(guid = "bbb", bundle_id = "123", name = "second")
  )

  result <- parse_connectapi(data)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_type(result$bundle_id, "character")
  expect_identical(result$bundle_id, c(NA_character_, "123"))
})

test_that("parse_connectapi handles mixed null/non-null datetime strings", {
  data <- list(
    list(guid = "aaa", active_time = NULL),
    list(guid = "bbb", active_time = "2023-08-22T14:13:14Z")
  )

  result <- parse_connectapi(data)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_type(result$active_time, "character")
  expect_identical(result$active_time, c(NA_character_, "2023-08-22T14:13:14Z"))
})

test_that("parse_connectapi handles mixed null/non-null integer timestamps", {
  data <- list(
    list(key = "abc", start_time = 1732573574, end_time = NULL),
    list(key = "def", start_time = 1732553145, end_time = 1732556770)
  )

  result <- parse_connectapi(data)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_type(result$end_time, "double")
  expect_identical(result$end_time, c(NA_real_, 1732556770))
})

test_that("coerce_datetime handles character (RFC 3339)", {
  withr::local_envvar(TZ = "UTC")
  result <- coerce_datetime(c("2023-08-22T14:13:14Z", "2020-01-01T01:02:03Z"))
  expect_s3_class(result, "POSIXct")
  expect_equal(length(result), 2)
})

test_that("coerce_datetime handles numeric (epoch seconds)", {
  result <- coerce_datetime(1692713594)
  expect_s3_class(result, "POSIXct")
  expect_equal(as.double(result), 1692713594, tolerance = 1)
})

test_that("coerce_datetime handles POSIXct pass-through", {
  ts <- as.POSIXct("2023-08-22 14:13:14", tz = "UTC")
  result <- coerce_datetime(ts)
  expect_identical(result, ts)
})

test_that("coerce_datetime handles NULL", {
  result <- coerce_datetime(NULL)
  expect_s3_class(result, "POSIXct")
  expect_equal(length(result), 0)
})

test_that("coerce_datetime handles all-NA logical", {
  result <- coerce_datetime(c(NA, NA))
  expect_s3_class(result, "POSIXct")
  expect_true(all(is.na(result)))
  expect_equal(length(result), 2)
})

test_that("coerce_datetime rejects unsupported types", {
  expect_error(coerce_datetime(data.frame()), "Cannot coerce")
  expect_error(coerce_datetime(NA_complex_), "Cannot coerce")
})

test_that("parse_connectapi_typed converts specified datetime columns", {
  data <- list(
    list(guid = "aaa", created_time = "2023-08-22T14:13:14Z"),
    list(guid = "bbb", created_time = "2020-01-01T01:02:03Z")
  )

  result <- parse_connectapi_typed(data, datetime_cols = "created_time")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result$created_time, "POSIXct")
  expect_type(result$guid, "character")
})

test_that("parse_connectapi_typed leaves columns alone without datetime_cols", {
  data <- list(
    list(guid = "aaa", created_time = "2023-08-22T14:13:14Z"),
    list(guid = "bbb", created_time = "2020-01-01T01:02:03Z")
  )

  result <- parse_connectapi_typed(data)
  expect_s3_class(result, "tbl_df")
  # Without datetime_cols, timestamps stay as character
  expect_type(result$created_time, "character")
})

test_that("parse_connectapi_typed handles data frame input (fast path)", {
  df <- data.frame(
    guid = c("aaa", "bbb"),
    created_time = c("2023-08-22T14:13:14Z", "2020-01-01T01:02:03Z"),
    stringsAsFactors = FALSE
  )

  result <- parse_connectapi_typed(df, datetime_cols = "created_time")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result$created_time, "POSIXct")
})

test_that("parse_connectapi_typed handles empty input", {
  result <- parse_connectapi_typed(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

  result <- parse_connectapi_typed(tibble::tibble())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("parse_connectapi_typed handles all-NA datetime column", {
  data <- list(
    list(guid = "aaa", active_time = NULL),
    list(guid = "bbb", active_time = NULL)
  )

  result <- parse_connectapi_typed(data, datetime_cols = "active_time")
  expect_s3_class(result$active_time, "POSIXct")
  expect_true(all(is.na(result$active_time)))
})
