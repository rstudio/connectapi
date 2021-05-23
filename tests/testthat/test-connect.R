context("test Connect R6 class")

test_that("preserves provided values", {
  server <- "http://myhost.example.com"
  api_key <- "fake"

  con <- Connect$new(server = server, api_key = api_key)

  expect_identical(con$server, server)
  expect_identical(con$api_key, api_key)
})

test_that("trailing slash removed from server", {
  con <- Connect$new(server = "http://myhost.example.com/", api_key = "fake")
  con2 <- Connect$new(server = "http://myhost.example.com", api_key = "fake")

  expect_identical(con$server, con2$server)
})

test_that("error if protocol not defined", {
  con <- Connect$new(server = "https://myhost.example.com", api_key = "fake")

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

test_that("warning if using the host parameter", {
  expect_warning(
    connect(host = "https://hello.example.com", api_key = "false", .check_is_fatal = FALSE),
    "host"
  )
})

test_that("version is validated", {
  skip("not implemented yet")
})
