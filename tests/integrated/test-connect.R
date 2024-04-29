test_that("connect works", {
  con <- connect(
    server = Sys.getenv("TEST_1_SERVER"),
    api_key = Sys.getenv("TEST_1_API_KEY")
  )
  expect_true(validate_R6_class(conn, "Connect"))
})

test_that("connect works with prefix only", {
  con <- connect(prefix = "TEST_1")
  expect_true(validate_R6_class(conn, "Connect"))
})

test_that("connect fails for nonexistent server", {
  expect_error({
    connect(server = "does-not-exist.rstudio.com", api_key = "bogus")
  })
})

test_that("connect fails for good server, bad api key", {
  expect_error({
    connect(
      server = Sys.getenv("TEST_1_SERVER"),
      api_key = "bogus"
    )
  })
})

test_that("error if API key is empty", {
  expect_error(
    connect(server = Sys.getenv("TEST_1_SERVER"), api_key = ""),
    "provide a valid API key"
  )

  expect_error(
    connect(server = Sys.getenv("TEST_1_SERVER"), api_key = NA_character_),
    "provide a valid API key"
  )

  expect_error(
    connect(server = Sys.getenv("TEST_1_SERVER"), api_key = NULL),
    "provide a valid API key"
  )
})

test_that(".check_is_fatal toggle works", {
  expect_error(
    connect(server = Sys.getenv("TEST_1_SERVER"), api_key = ""),
    "provide a valid API key"
  )

  rsc <- connect(server = Sys.getenv("TEST_1_SERVER"), api_key = "", .check_is_fatal = FALSE)
  expect_true(
    validate_R6_class(rsc, "Connect")
  )

  expect_error(
    suppressMessages(connect(server = "http://fake-value.example.com", api_key = "fake-value")),
    "Could not resolve host"
  )

  # TODO: suppressing the message in the tryCatch handler does not work...?
  rsc1 <- suppressMessages(connect(server = "http://fake-value.example.com", api_key = "fake-value", .check_is_fatal = FALSE))
  expect_true(
    validate_R6_class(rsc1, "Connect")
  )
})
