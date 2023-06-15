context("vec_cast")

test_that("vec_cast.fs_bytes warns", {
  skip("failing currently")
  expect_warning(vctrs::vec_cast(1L, fs::as_fs_bytes(1L)), "experimental")
  expect_is(vctrs::vec_cast(1L, fs::as_fs_bytes(NA_integer_)), "fs_bytes")
})

test_that("vec_cast.POSIXct.character works", {
  skip("failing currently")
  vctrs::vec_cast("2020-05-19 01:36:27Z", NA_datetime_)
})

test_that("coerce_fsbytes fills the void", {
  expect_is(coerce_fsbytes(1L, fs::as_fs_bytes(NA_integer_)), "fs_bytes")
  expect_is(coerce_fsbytes(1, fs::as_fs_bytes(NA_integer_)), "fs_bytes")
  expect_error(coerce_fsbytes(data.frame(), fs::as_fs_bytes(NA_integer_)), class = "vctrs_error_incompatible_type")
})

test_that("coerce_datetime fills the void", {
  chardate <- "2020-05-19 01:36:27Z"
  numdate <- as.double(Sys.time())
  expect_is(coerce_datetime(chardate, NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(c(chardate, chardate), NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(numdate, NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(c(numdate, numdate), NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(NA_datetime_, NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(c(NA_datetime_, NA_datetime_), NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(NA_integer_, NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(c(NA_integer_, NA_integer_), NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(NA, NA_datetime_), "POSIXct")
  expect_is(coerce_datetime(c(NA, NA), NA), "POSIXct")

  expect_error(coerce_datetime(data.frame(), NA_datetime_), class = "vctrs_error_incompatible_type")
  expect_error(coerce_datetime(list(), NA_datetime_, name = "list"), class = "vctrs_error_incompatible_type")

  expect_error(coerce_datetime(NA_complex_, NA_datetime_, name = "complexity"), class = "vctrs_error_incompatible_type")
})

context("make_timestamp")

test_that("works with POSIXct", {
  ts <- as.POSIXct("2020-01-01 01:02:03Z")
  outcome <- "2020-01-01T01:02:03Z"
  expect_equal(make_timestamp(ts), outcome)
  expect_equal(make_timestamp(rep(ts, 10)), rep(outcome, 10))

  # idempotent
  expect_equal(make_timestamp(make_timestamp(ts)), outcome)
})

test_that("safe for strings", {
  expect_equal(make_timestamp("hello"), "hello")
  expect_equal(make_timestamp(rep("hello", 5)), rep("hello", 5))

  expect_equal(make_timestamp(NA_character_), NA_character_)
})

test_that("converts to character", {
  expect_is(make_timestamp(NA_datetime_), "character")
})

context("swap_timestamp_format")

test_that("works with expected case", {
  expect_match(swap_timestamp_format("2020-01-07T11:21:07Z"), "([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}\\.*[0-9]*Z)")
  expect_match(swap_timestamp_format(rep("2020-01-07T11:21:07Z", 10)), "([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}\\.*[0-9]*Z)")

  # decimals
  expect_match(swap_timestamp_format("2020-01-07T11:21:07.123456Z"), "([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}\\.*[0-9]*Z)")
  expect_match(swap_timestamp_format(rep("2020-01-07T11:21:07.123456Z", 10)), "([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}\\.*[0-9]*Z)")
})

test_that("safe for NA", {
  expect_identical(swap_timestamp_format(NA_character_), NA_character_)
})

test_that("safe for other strings", {
  expect_identical(swap_timestamp_format("my string"), "my string")
  expect_identical(swap_timestamp_format("132352523153151"), "132352523153151")
})

context("ensure_column")

test_that("works with lists", {
  list_chk_null <- ensure_column(tibble::tibble(), NA_list_, "hello")
  expect_is(list_chk_null, "tbl_df")
  expect_is(list_chk_null$hello, "list")

  list_chk_same <- ensure_column(tibble::tibble(hello = list(list(1, 2, 3), list(1, 2, 3, 4))), NA_list_, "hello")
  expect_is(list_chk_same, "tbl_df")
  expect_is(list_chk_same$hello, "list")
})
test_that("works with POSIXct", {
  time_chk_null <- ensure_column(tibble::tibble(), NA_datetime_, "hello")
  expect_is(time_chk_null, "tbl_df")
  expect_is(time_chk_null$hello, "POSIXct")

  time_chk_some <- ensure_column(tibble::tibble(one = c(1, 2, 3)), NA_datetime_, "hello")
  expect_is(time_chk_some, "tbl_df")
  expect_is(time_chk_some$hello, "POSIXct")

  skip("Ahh! this fails presently. Are double -> POSIXct conversions allowed?")
  time_chk_convert <- ensure_column(tibble::tibble(hello = c(1, 2, 3)), NA_datetime_, "hello")
  expect_is(time_chk_convert, "tbl_df")
  expect_is(time_chk_convert$hello, "POSIXct")
})

test_that("converts length one list", {
  hm <- ensure_column(tibble::tibble(one = "hi"), NA_list_, "one")
  expect_is(hm$one, "list")
})

# specific errors - PR 192
test_that("works for bad inputs", {
  job <- list(
    ppid=12345,
    pid=67890,
    key="abckey",
    app_id=1234,
    variant_id=0,
    bundle_id=1234,
    tag="run_app",
    finalized=TRUE,
    hostname="host",
    origin=format(Sys.time(), format="%Y-%m-%dT%H:%M:%SZ"),
    stdout="one-entry",
    stderr=c("one-entry", "two-entry"),
    logged_error=NULL,
    exit_code=0,
    start_time=as.numeric(format(Sys.time(), format="%s")),
    end_time=NULL,
    app_guid=uuid::UUIDgenerate()
  )
  res <- connectapi:::parse_connectapi_typed(list(job), !!!connectapi:::connectapi_ptypes$job)
  expect_is(res$stdout, "list")
  expect_is(res$origin, "character")
  expect_is(res$start_time, "POSIXct")
  expect_is(res$end_time, "POSIXct")
})



