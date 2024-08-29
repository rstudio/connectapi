test_that("coerce_fsbytes fills the void", {
  expect_s3_class(coerce_fsbytes(1L, fs::as_fs_bytes(NA_integer_)), "fs_bytes")
  expect_s3_class(coerce_fsbytes(1, fs::as_fs_bytes(NA_integer_)), "fs_bytes")
  expect_error(coerce_fsbytes(data.frame(), fs::as_fs_bytes(NA_integer_)), class = "vctrs_error_incompatible_type")
})

test_that("coerce_datetime fills the void", {
  chardate <- "2023-10-25T17:04:08Z"
  numdate <- as.double(Sys.time())
  expect_s3_class(coerce_datetime(chardate, NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(c(chardate, chardate), NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(numdate, NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(c(numdate, numdate), NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(NA_datetime_, NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(c(NA_datetime_, NA_datetime_), NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(NA_integer_, NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(c(NA_integer_, NA_integer_), NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(NA, NA_datetime_), "POSIXct")
  expect_s3_class(coerce_datetime(c(NA, NA), NA), "POSIXct")
  expect_s3_class(coerce_datetime(NULL, NA), "POSIXct")

  expect_error(coerce_datetime(data.frame(), NA_datetime_), class = "vctrs_error_incompatible_type")
  expect_error(coerce_datetime(list(), NA_datetime_, name = "list"), class = "vctrs_error_incompatible_type")

  expect_error(coerce_datetime(NA_complex_, NA_datetime_, name = "complexity"), class = "vctrs_error_incompatible_type")
})

test_that("parse_connect_rfc3339 parses timestamps we expect from Connect", {
  original_tz <- Sys.getenv("TZ")
  withr::defer(Sys.setenv(TZ = original_tz))

  xs <- c(
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z",
    "2023-08-22T15:13:14+01:00",
    "2020-01-01T00:02:03-01:00"
  )

  expected <- as.POSIXct(strptime(c(
    "2023-08-22T14:13:14+0000",
    "2020-01-01T01:02:03+0000",
    "2023-08-22T15:13:14+0100",
    "2020-01-01T00:02:03-0100"
  ), format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))

  Sys.setenv(TZ = "America/New_York")
  expect_identical(parse_connect_rfc3339(xs), expected)

  Sys.setenv(TZ = "UTC")
  expect_identical(parse_connect_rfc3339(xs), expected)
})

test_that("make_timestamp produces expected output", {
  original_tz <- Sys.getenv("TZ")
  withr::defer(Sys.setenv(TZ = original_tz))

  inputs <- c(
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z",
    "2023-08-22T15:13:14+01:00",
    "2020-01-01T00:02:03-01:00"
  )
  outcome <- c(
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z",
    "2023-08-22T14:13:14Z",
    "2020-01-01T01:02:03Z"
  )
  Sys.setenv(TZ = "America/New_York")
  
  ts <- coerce_datetime(outcome, NA_datetime_)
  expect_equal(make_timestamp(ts), outcome)

  # Works on a single item
  expect_equal(make_timestamp(ts[1]), outcome[1])

  # Idempotent
  expect_equal(make_timestamp(make_timestamp(ts)), outcome)

  Sys.setenv(TZ = "UTC")
  
  ts <- coerce_datetime(outcome, NA_datetime_)
  expect_equal(make_timestamp(ts), outcome)

  # Works on a single item
  expect_equal(make_timestamp(ts[1]), outcome[1])

  # Idempotent
  expect_equal(make_timestamp(make_timestamp(ts)), outcome)
})

test_that("make_timestamp is safe for strings", {
  expect_equal(make_timestamp("hello"), "hello")
  expect_equal(make_timestamp(rep("hello", 5)), rep("hello", 5))

  expect_equal(make_timestamp(NA_character_), NA_character_)
})

test_that("make_timestamp converts to character", {
  expect_type(make_timestamp(NA_datetime_), "character")
})

test_that("ensure_column works with lists", {
  list_chk_null <- ensure_column(tibble::tibble(), NA_list_, "hello")
  expect_s3_class(list_chk_null, "tbl_df")
  expect_type(list_chk_null$hello, "list")

  list_chk_same <- ensure_column(tibble::tibble(hello = list(list(1, 2, 3), list(1, 2, 3, 4))), NA_list_, "hello")
  expect_s3_class(list_chk_same, "tbl_df")
  expect_type(list_chk_same$hello, "list")
})
test_that("ensure_column works with POSIXct", {
  time_chk_null <- ensure_column(tibble::tibble(), NA_datetime_, "hello")
  expect_s3_class(time_chk_null, "tbl_df")
  expect_s3_class(time_chk_null$hello, "POSIXct")

  time_chk_some <- ensure_column(tibble::tibble(one = c(1, 2, 3)), NA_datetime_, "hello")
  expect_s3_class(time_chk_some, "tbl_df")
  expect_s3_class(time_chk_some$hello, "POSIXct")

  skip("Ahh! this fails presently. Are double -> POSIXct conversions allowed?")
  time_chk_convert <- ensure_column(tibble::tibble(hello = c(1, 2, 3)), NA_datetime_, "hello")
  expect_s3_class(time_chk_convert, "tbl_df")
  expect_s3_class(time_chk_convert$hello, "POSIXct")
})

test_that("converts length one list", {
  hm <- ensure_column(tibble::tibble(one = "hi"), NA_list_, "one")
  expect_type(hm$one, "list")
})

# specific errors - PR 192
test_that("works for bad inputs", {
  job <- list(
    ppid = 12345,
    pid = 67890,
    key = "abckey",
    app_id = 1234,
    variant_id = 0,
    bundle_id = 1234,
    tag = "run_app",
    finalized = TRUE,
    hostname = "host",
    origin = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ"),
    stdout = "one-entry",
    stderr = c("one-entry", "two-entry"),
    logged_error = NULL,
    exit_code = 0,
    start_time = as.numeric(format(Sys.time(), format = "%s")),
    end_time = NULL,
    app_guid = uuid::UUIDgenerate()
  )
  res <- connectapi:::parse_connectapi_typed(list(job), connectapi:::connectapi_ptypes$job)
  expect_type(res$stdout, "list")
  expect_type(res$origin, "character")
  expect_s3_class(res$start_time, "POSIXct")
  expect_s3_class(res$end_time, "POSIXct")
})
