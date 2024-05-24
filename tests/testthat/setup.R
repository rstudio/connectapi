library(httptest)

set_requester(
  function(r) {
    gsub_request(
      gsub_request(
        r,
        # Prune the fake domain
        "https\\://connect.example/",
        ""
      ),
      # Truncate GUIDs to 8 characters
      "([0-9a-f]{8})\\-[0-9a-f\\-]{27}",
      "\\1"
    )
  }
)
