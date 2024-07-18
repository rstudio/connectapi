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