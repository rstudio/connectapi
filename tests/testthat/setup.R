library(httptest)

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
# created before 2024.09.0.
.mockPaths("tests/testthat/_mocks/2024.08.0")
