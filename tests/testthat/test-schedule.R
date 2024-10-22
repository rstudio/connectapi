library(R6)

BaseMockConnect <- R6Class(
  "BaseMockConnect",
  inherit = Connect,
  private = list(
    urls = character()
  ),
  public = list(
    initialize = function(version = NA) {
      self$server <- "https://connect.example"
      self$api_key <- "fake"
      private$.version <- version
    },
    called_with = function(url = NULL) {
      if (is.null(url)) {
        return(private$urls)
      }
      private$urls <- c(private$urls, url)
    }
  )
)

test_that("get_timezones() gets timeszones from v1 url when available", {
  mock_get_2024.09.0 <- function(url, parser = "parsed", ...) {
    self$called_with(url)
    if (url == "v1/timezones") {
      res <- structure(
        list(
          url = "__api__/v1/timezones",
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
    }

    if (is.null(parser)) {
      res
    } else {
      self$raise_error(res)
      httr::content(res, as = parser)
    }
  }

  # With version available
  MockConnect <- R6Class(
    "MockConnect",
    inherit = BaseMockConnect,
    public = list(
      GET = mock_get_2024.09.0
    )
  )

  client <- MockConnect$new("2024.09.0")

  expect_equal(
    get_timezones(client),
    list(`Africa/Abidjan (+00:00)` = "Africa/Abidjan", `Africa/Accra (+00:00)` = "Africa/Accra")
  )
  expect_equal(
    client$called_with(),
    "v1/timezones"
  )
})

test_that("get_timezones() gets timeszones from unversioned url when v1 returns 404", {
  mock_get_2024.08.0 <- function(url, parser = "parsed", ...) {
    self$called_with(url)
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

  # With version available
  MockConnect <- R6Class(
    "MockConnect",
    inherit = BaseMockConnect,
    public = list(
      GET = mock_get_2024.08.0
    )
  )

  client <- MockConnect$new("2024.08.0")

  expect_equal(
    get_timezones(client),
    list(`Africa/Abidjan (+00:00)` = "Africa/Abidjan", `Africa/Accra (+00:00)` = "Africa/Accra")
  )
  expect_equal(
    client$called_with(),
    c("v1/timezones", "timezones")
  )
})

test_that("get_timezones() raises 404 error when v1 and unversioned return 404", {
  mock_get_both_404 <- function(url, parser = "parsed", ...) {
    self$called_with(url)
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
          status_code = 404L,
          request = structure(list(url = "__api__/timezones"), class = "request"),
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

  # With version available
  MockConnect <- R6Class(
    "MockConnect",
    inherit = BaseMockConnect,
    public = list(
      GET = mock_get_both_404
    )
  )

  client <- MockConnect$new()

  expect_error(
    get_timezones(client),
    regexp = "timezones request failed with Client error"
  )
  expect_equal(
    client$called_with(),
    c("v1/timezones", "timezones")
  )
})
