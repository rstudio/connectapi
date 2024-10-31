library(httptest)
library(R6)

set_requester(
  function(r) {
    gsub_request(
      gsub_request(
        r,
        # Prune the domain
        "^https?://[^/]+/",
        ""
      ),
      # Truncate GUIDs to 8 characters
      "([0-9a-f]{8})\\-[0-9a-f\\-]{27}",
      "\\1"
    )
  }
)

set_redactor(
  function(r) {
    gsub_response(
      gsub_response(
        r,
        # Prune the domain
        "^https?://[^/]+/",
        ""
      ),
      # Truncate GUIDs to 8 characters
      "([0-9a-f]{8})\\-[0-9a-f\\-]{27}",
      "\\1"
    )
  }
)

# Mocks are in directories by Connect version. 2024.08.0 contains all mocks
# created before 2024.09.0, and is the default mock path.
.mockPaths("2024.08.0")

MockConnect <- R6Class(
  "MockConnect",
  inherit = Connect,
  public = list(
    initialize = function(version = NA) {
      self$server <- "https://connect.example"
      self$api_key <- "fake"
      private$.version <- version
    },
    # The request function matches the route against the routes in the names of
    # the response list. When a response is selected, it is removed from the
    # list.
    request = function(method, url, ..., parser = "parsed") {
      route <- paste(method, url)

      # Record call
      self$log_call(route)

      # Look for response
      if (!(route %in% names(self$responses))) {
        stop(glue::glue("Unexpected route: {route}"))
      }

      idx <- match(route, names(self$responses))
      res <- self$responses[[idx]]
      self$responses <- self$responses[-idx]

      if (is.null(parser)) {
        res
      } else {
        self$raise_error(res)
        httr::content(res, as = parser)
      }
    },
    responses = list(),
    # Add a response to a list of responses. The response is keyed according to
    # its route, represented as `{VERB} {URL}`. The URL is constructed as an API
    # URL for the server (this will probably have to change in the future). Each
    # response can only be used once. You can supply multiple responses for the
    # same URL.
    mock_response = function(method, path, content, status_code = 200L, headers = c("Content-Type" = "application/json; charset=utf-8")) {
      url <- self$api_url(path)
      route <- paste(method, url)

      res <- new_mock_response(url, content, status_code, headers)

      route <- paste(method, url)
      new_response <- list(res)
      new_response <- setNames(new_response, route)

      self$responses <- append(self$responses, new_response)

    },
    call_log = character(),
    log_call = function(route) {
      self$call_log <- c(self$call_log, route)
    }
  )
)

new_mock_response <- function(url, content, status_code, headers = character()) {
  # Headers in responses are case-insensitive lists.
  names(headers) <- tolower(names(headers))
  headers <- as.list(headers)
  headers <- structure(as.list(headers), class = c("insensitive", class(headers)))

  # Treat content similarly to httr::POST, with a subset of behaviors
  if (is.character(content) && length(content) == 1) {
    content <- charToRaw(content)
  } else if (is.list(content)) {
    content <- charToRaw(jsonlite::toJSON(content, auto_unbox = TRUE, null = "null"))
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
