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

test_that("make_timestamp works with POSIXct", {
  outcome <- "2020-01-01T01:02:03Z"
  ts <- coerce_datetime(outcome, NA_datetime_)
  expect_equal(make_timestamp(ts), outcome)
  expect_equal(make_timestamp(rep(ts, 10)), rep(outcome, 10))

  # idempotent
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
