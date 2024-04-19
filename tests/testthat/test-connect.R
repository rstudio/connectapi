test_that("Connect R6 class preserves provided values", {
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

test_that("version is validated", {
  skip("not implemented yet")
})

test_that("Handling error responses", {
  con <- Connect$new(server = "https://connect.example", api_key = "fake")
  resp <- fake_response("https://connect.example/__api__/", status_code = 400L)
  expect_error(con$raise_error(resp), "Bad Request")
})
