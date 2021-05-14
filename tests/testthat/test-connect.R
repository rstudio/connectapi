context("test Connect R6 class")

test_that("preserves provided values", {
  host <- "http://myhost.example.com"
  api_key <- "fake"

  con <- Connect$new(host = host, api_key = api_key)

  expect_identical(con$host, host)
  expect_identical(con$api_key, api_key)
})

test_that("trailing slash removed from host", {
  con <- Connect$new(host = "http://myhost.example.com/", api_key = "fake")
  con2 <- Connect$new(host = "http://myhost.example.com", api_key = "fake")

  expect_identical(con$host, con2$host)
})

test_that("error if protocol not defined", {
  con <- Connect$new(host = "https://myhost.example.com", api_key = "fake")

  expect_error(
    Connect$new("test.example.com", "fake"),
    "protocol"
  )

  expect_error(
    Connect$new("://test.example.com", "fake"),
    "protocol"
  )
})

context("connect")

test_that("version is validated", {
  skip("not implemented yet")
})
