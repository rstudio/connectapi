context("utils")

test_that("safequery handles values correctly", {
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
  m <- mockery::mock("1.8.2.1-4", "1.8.6.9-14", "2.9.0.0.4")
  fake_client <- Connect$new("http://test", "api_key")
  with_mock(safe_server_version = m, {
    expect_error(error_if_less_than(fake_client, "1.8.6"))
    expect_silent(error_if_less_than(fake_client, "1.8.6"))
    expect_silent(error_if_less_than(fake_client, "1.8.6"))
  })
})

test_that("check_connect_version works", {
  # silent for patch version changes
  expect_silent(check_connect_version("1.8.2-4", "1.8.2.1-10"))

  # warnings for minor version changes
  expect_warning(check_connect_version("1.8.2-4", "1.8.0.5-1"), "newer")
  expect_warning(check_connect_version("1.8.2-4", "2.8.0.5-1"), "older")
})
