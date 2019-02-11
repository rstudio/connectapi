context("test Connect R6 class")

test_that("preserves provided values", {
  host <- "myhost.example.com"
  api_key <- "fake"
  
  con <- Connect$new(host = host, api_key = api_key)
  
  expect_identical(con$host, host)
  expect_identical(con$api_key, api_key)
})

test_that("trailing slash removed from host", {
  con <- Connect$new(host = "myhost.example.com/", api_key = "fake")
  con2 <- Connect$new(host = "myhost.example.com", api_key = "fake")
  
  expect_identical(con$host, con2$host)
})
