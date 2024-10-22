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

test_that("Handling deprecation warnings", {
  rlang::reset_warning_verbosity("X-Deprecated-Endpoint")
  on.exit(rlang::reset_warning_verbosity("X-Deprecated-Endpoint"))

  # No warning here
  resp <- fake_response("https://connect.example/__api__/", headers = list(
    `Content-Type` = "application/json"
  ))
  expect_warning(check_debug(resp), NA)

  # Yes warning here
  resp <- fake_response("https://connect.example/__api__/", headers = list(
    `X-Deprecated-Endpoint` = "/v1"
  ))
  expect_warning(
    check_debug(resp),
    paste(
      "https://connect.example/__api__/ is deprecated and will be removed in a",
      "future version of Connect. Please upgrade `connectapi` in order to use",
      "the new APIs."
    ),
    class = "deprecatedWarning"
  )

  # No warning if you do it again because we only warn the first time
  expect_warning(check_debug(resp), NA)
})

with_mock_api({
  test_that("browse URLs", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    # Inject into this function something other than utils::browseURL
    # so we can assert that it is being called without actually trying to open a browser
    suppressMessages(trace("browse_url", where = connectapi::browse_solo, tracer = quote({
      browseURL <- function(x) warning(paste("Opening", x))
    }), at = 1, print = FALSE))
    expect_warning(
      browse_connect(con),
      "Opening https://connect.example"
    )
    expect_warning(
      browse_api_docs(con),
      "Opening https://connect.example/__docs__/api"
    )
    suppressMessages(untrace("browse_url", where = connectapi::browse_solo))
  })

  test_that("httr_config", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    con$httr_config(httr::add_headers(MY_MAGIC_HEADER = "value"))
    expect_header(
      expect_GET(
        con$GET("v1/content"),
        "https://connect.example/__api__/v1/content"
      ),
      "MY_MAGIC_HEADER: value"
    )
  })

  
  test_that("client$version is NA when server settings lacks version info", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_message(v <- con$version, "Version information is not exposed by this Posit Connect instance")
    expect_true(is.na(v))
  })
})

test_that("client$version is returns version when server settings exposes it", {
  with_mock_dir("2024.09.0", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_equal(con$version, "2024.09.0")
  })
})
