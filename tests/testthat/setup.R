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
    request = function(method, url, ..., parser = "parsed") {
      route <- paste(method, url)
      print(route)

      # Record call
      self$log_call(route)

      # Look for response
      if (!(route %in% names(self$responses))) {
        stop("Unexpected route")
      }
      res <- self$responses[[route]]

      if (is.null(parser)) {
        res
      } else {
        self$raise_error(res)
        httr::content(res, as = parser)
      }
    },
    responses = list(),
    mock_response = function(method, path, content, status_code = 200L, headers = c("Content-Type" = "application/json; charset=utf-8")) {
      url <- self$api_url(path)
      route <- paste(method, url)
      print(route)
      res <- new_mock_response(url, content, status_code, headers)
      self$responses[[route]] <- res
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
    content <- charToRaw(jsonlite::toJSON(content, auto_unbox = TRUE))
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
