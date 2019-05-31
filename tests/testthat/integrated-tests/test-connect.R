context("test connect connection")

# should connect with env vars
test_conn_1 <- NULL
test_conn_2 <- NULL

test_that("connect works", {
  test_conn_1 <<- connect(
    host = Sys.getenv("TEST_SERVER_1"), 
    api_key = Sys.getenv("TEST_KEY_1")
    )
})

test_that("connect fails for nonexistent server", {
  expect_error({
    connect(host = "does-not-exist.example.com", api_key = "bogus")
  })
})

test_that("connect fails for good server, bad api key", {
  expect_error({
    connect(
      host = Sys.getenv("TEST_SERVER_1"),
      api_key = "bogus"
    )
  })
})
