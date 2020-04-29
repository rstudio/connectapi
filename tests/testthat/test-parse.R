context("vec_cast")

test_that("vec_cast.fs_bytes warns", {
  skip("fail - broken right now...?")
  expect_warning(vctrs::vec_cast(1L, fs::as_fs_bytes(1L)), "experimental")
  expect_is(vctrs::vec_cast(1L, fs::as_fs_bytes(NA_integer_)), "fs_bytes")
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

  time_chk_convert <- ensure_column(tibble::tibble(hello = c(1, 2, 3)), NA_datetime_, "hello")
  expect_is(time_chk_convert, "tbl_df")
  expect_is(time_chk_convert$hello, "POSIXct")
})
