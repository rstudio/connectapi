context("test connect connection")

# should connect with env vars
test_conn_1 <- NULL
test_conn_2 <- NULL

test_that("connect works", {
  test_conn_1 <<- connect(
    host = Sys.getenv("TEST_1_SERVER"),
    api_key = Sys.getenv("TEST_1_API_KEY")
  )
  expect_true(validate_R6_class(test_conn_1, "Connect"))
})

test_that("connect works with prefix only", {
  test_conn_2 <<- connect(
    prefix = "TEST_2"
  )
  expect_true(validate_R6_class(test_conn_2, "Connect"))
})

test_that("connect fails for nonexistent server", {
  expect_error({
    connect(host = "does-not-exist.rstudio.com", api_key = "bogus")
  })
})

test_that("connect fails for good server, bad api key", {
  expect_error({
    connect(
      host = Sys.getenv("TEST_1_SERVER"),
      api_key = "bogus"
    )
  })
})

test_that("error if API key is empty", {
  expect_error(
    connect(host = Sys.getenv("TEST_1_SERVER"), api_key = ""),
    "provide a valid API key"
  )

  expect_error(
    connect(host = Sys.getenv("TEST_1_SERVER"), api_key = NA_character_),
    "provide a valid API key"
  )

  expect_error(
    connect(host = Sys.getenv("TEST_1_SERVER"), api_key = NULL),
    "provide a valid API key"
  )
})
