library(R6)

MockConnect <- R6Class(
  "MockConnect",
  inherit = Connect,
  public = list(
    initialize = function(version = NA) {
      self$server <- "https://connect.example"
      self$api_key <- "fake"
      private$.version <- version
    },
    request = function(method, url, ..., parser = "parsed") {
      # Record call
      self$log_call(paste(method, url))

      # Look for response
      if (!(url %in% names(self$responses))) {
        stop("Unexpected URL")
      }
      res <- self$responses[[url]]

      if (is.null(parser)) {
        res
      } else {
        self$raise_error(res)
        httr::content(res, as = parser)
      }
    },
    responses = list(),
    add_mock_response = function(path, content, status_code = 200L, headers = c("Content-Type" = "application/json; charset=utf-8")) {
      url <- self$api_url(path)
      res <- mock_response(url, content, status_code, headers)
      self$responses[[url]] <- res
    },
    call_log = character(),
    log_call = function(call) {
      self$call_log <- c(self$call_log, call)
    }
  )
)

mock_response <- function(url, content, status_code, headers = character()) {
  # Headers in responses are case-insensitive lists.
  names(headers) <- tolower(names(headers))
  headers <- as.list(headers)
  headers <- structure(as.list(headers), class = c("insensitive", class(headers)))

  # Treat content similarly to httr::POST, with a subset of behaviors
  if (is.character(content) && length(content) == 1) {
    content <- charToRaw(content)
  } else if (is.list(content)) {
    content <- charToRaw(toJSON(content, auto_unbox = TRUE))
  }

  structure(
    list(
      url = url,
      status_code = status_code,
      request = structure(list(url = url), class = "request"),
      headers = headers,
      content = content
    ),
    class = "response"
  )
}

mock_response_404 <- function(url) {
  mock_response(url, content = "404 page not found", status_code = 404L, headers = c("Content-Type" = "text/plain"))
}


mock_response(
  url = "v1/timezones",
  body = list(
    list(timezone = "Africa/Abidjan", offset = "+00:00"),
    list(timezone = "Africa/Accra", offset = "+00:00")
  )
)

test_that("get_timezones() gets timeszones from v1 url when available", {
  # With version available
  client <- MockConnect$new()

  client$add_mock_response(
    path = "v1/timezones",
    content = list(
      list(timezone = "Africa/Abidjan", offset = "+00:00"),
      list(timezone = "Africa/Accra", offset = "+00:00")
    )
  )
  client$add_mock_response(
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

  client$add_mock_response(
    path = "v1/timezones",
    content = "404 page not found",
    status_code = 404L,
    headers = c("Content-Type" = "text/plain; charset=utf-8")
  )
  client$add_mock_response(
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

  client$add_mock_response(
    path = "v1/timezones",
    content = "404 page not found",
    status_code = 404L,
    headers = c("Content-Type" = "text/plain; charset=utf-8")
  )
  client$add_mock_response(
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
