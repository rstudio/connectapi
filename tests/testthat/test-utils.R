test_that("safe_query handles values correctly", {
  pref <- "prefixed"
  nullval <- NULL
  expect_identical(safe_query(nullval, pref), "")

  oneval <- "blah"
  expect_identical(safe_query(oneval, pref), paste0(pref, oneval))

  moreval <- c("blah", "blah2")
  expect_identical(safe_query(moreval, pref), paste0(pref, paste(moreval, collapse = "|")))

  morenull <- c(NULL, NULL)
  expect_identical(safe_query(morenull, pref, "|"), "")
})

test_that("simplify_version works", {
  expect_identical(simplify_version("1.8.2-4"), "1.8.2")
  expect_identical(simplify_version("1.8.2.1-4"), "1.8.2")
  expect_identical(simplify_version("10.70.204.1-4"), "10.70.204")
  expect_identical(simplify_version("10.0.0.0-4"), "10.0.0")
})

test_that("error_if_less_than errors as expected", {
  with_mock_api({
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_silent(error_if_less_than(con, "1.8.6"))
    expect_error(
      error_if_less_than(con, "2024.09"),
      "ERROR: This API requires Posit Connect version 2024.09"
    )
  })
})

test_that("check_connect_version works", {
  # silent for patch version changes
  expect_silent(check_connect_version("1.8.2.1-10", "1.8.2-4"))

  # silent if newer
  expect_silent(check_connect_version("1.8.2-4", "1.8.0.5-1"))

  # warnings for minor version changes
  expect_warning(check_connect_version("1.8.2-4", "2.8.0.5-1"), "older")
  warn_clear("old-connect")
})

test_that("check_connect_version warning snapshot", {
  # warning messages seem to cause issues in different environments based on color codes
  skip_on_cran()
  # No warning
  expect_snapshot(capture_warning(check_connect_version("2022.02", "2022.01")))
  expect_snapshot(capture_warning(check_connect_version("2022.01", "2022.02")))
  warn_clear("old-connect")
})
