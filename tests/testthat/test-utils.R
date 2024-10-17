test_that("simplify_version works", {
  expect_identical(simplify_version("1.8.2-4"), "1.8.2")
  expect_identical(simplify_version("1.8.2.1-4"), "1.8.2")
  expect_identical(simplify_version("10.70.204.1-4"), "10.70.204")
  expect_identical(simplify_version("10.0.0.0-4"), "10.0.0")
})

test_that("error_if_less_than errors as expected", {
  expect_silent(error_if_less_than("2024.09.0", "1.8.6"))
  expect_error(
    error_if_less_than("2024.08.2", "2024.09"),
    "ERROR: This API requires Posit Connect version 2024.09"
  )
  expect_warning(
    error_if_less_than(NA, "2024.09"),
    "WARNING: This API requires Posit Connect version 2024.09"
  )
})

test_that("warn_untested_connect works", {
  # silent for patch version changes
  expect_silent(warn_untested_connect("1.8.2.1-10", "1.8.2-4"))

  # silent if newer
  expect_silent(warn_untested_connect("1.8.2-4", "1.8.0.5-1"))

  # warnings for minor version changes
  expect_warning(warn_untested_connect("1.8.2-4", "2.8.0.5-1"), "older")
  rlang::reset_warning_verbosity("old-connect")
})

test_that("warn_untested_connect warning snapshot", {
  # warning messages seem to cause issues in different environments based on color codes
  skip_on_cran()
  # No warning
  expect_snapshot(capture_warning(warn_untested_connect("2022.02", "2022.01")))
  expect_snapshot(capture_warning(warn_untested_connect("2022.01", "2022.02")))
  rlang::reset_warning_verbosity("old-connect")
})
