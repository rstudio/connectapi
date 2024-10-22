test_that("get_timezones() gets timeszones from v1 url when available", {
  # With version available
  client <- MockConnect$new()

  client$mock_response(
    path = "v1/timezones",
    content = list(
      list(timezone = "Africa/Abidjan", offset = "+00:00"),
      list(timezone = "Africa/Accra", offset = "+00:00")
    )
  )
  client$mock_response(
    path = "timezones",
    content = list(
      list(timezone = "Africa/Abidjan", offset = "+00:00"),
      list(timezone = "Africa/Accra", offset = "+00:00")
    )
  )

  expect_equal(
    get_timezones(client),
    list(`Africa/Abidjan (+00:00)` = "Africa/Abidjan", `Africa/Accra (+00:00)` = "Africa/Accra")
  )
  expect_equal(
    client$call_log,
    "GET https://connect.example/__api__/v1/timezones"
  )
})

test_that("get_timezones() gets timeszones from unversioned url when v1 returns 404", {
  mock_get_2024.08.0 <- function(url, parser = "parsed", ...) {
    self$call_log(url)
    if (url == "v1/timezones") {
      res <- structure(
        list(
          url = "__api__/v1/timezones",
          status_code = 404L,
          request = structure(list(url = "__api__/v1/timezones"), class = "request"),
          headers = structure(
            list(
              "content-type" = "text/plain"
            ),
            class = c("insensitive", "list")
          ),
          content = charToRaw("404 page not found")
        ),
        class = "response"
      )
    } else if (url == "timezones") {
      res <- structure(
        list(
          url = "__api__/timezones",
          status_code = 200L,
          headers = structure(
            list(
              "content-type" = "application/json; charset=utf-8"
            ),
            class = c("insensitive", "list")
          ),
          content = charToRaw("[{\"timezone\":\"Africa/Abidjan\",\"offset\":\"+00:00\"},{\"timezone\":\"Africa/Accra\",\"offset\":\"+00:00\"}]")
        ),
        class = "response"
      )
    } else {
      stop("Unexpected URL called")
    }

    if (is.null(parser)) {
      res
    } else {
      self$raise_error(res)
      httr::content(res, as = parser)
    }
  }

  client <- MockConnect$new()

  client$mock_response(
    path = "v1/timezones",
    content = "404 page not found",
    status_code = 404L,
    headers = c("Content-Type" = "text/plain; charset=utf-8")
  )
  client$mock_response(
    path = "timezones",
    content = list(
      list(timezone = "Africa/Abidjan", offset = "+00:00"),
      list(timezone = "Africa/Accra", offset = "+00:00")
    )
  )

  expect_equal(
    get_timezones(client),
    list(`Africa/Abidjan (+00:00)` = "Africa/Abidjan", `Africa/Accra (+00:00)` = "Africa/Accra")
  )
  expect_equal(
    client$call_log,
    c(
      "GET https://connect.example/__api__/v1/timezones",
      "GET https://connect.example/__api__/timezones"
    )
  )
})

test_that("get_timezones() raises 404 error when v1 and unversioned return 404", {
  client <- MockConnect$new()

  client$mock_response(
    path = "v1/timezones",
    content = "404 page not found",
    status_code = 404L,
    headers = c("Content-Type" = "text/plain; charset=utf-8")
  )
  client$mock_response(
    path = "timezones",
    content = "404 page not found",
    status_code = 404L,
    headers = c("Content-Type" = "text/plain; charset=utf-8")
  )

  expect_error(
    get_timezones(client),
    regexp = "timezones request failed with Client error"
  )
  expect_equal(
    client$call_log,
    c(
      "GET https://connect.example/__api__/v1/timezones",
      "GET https://connect.example/__api__/timezones"
    )
  )
})
