test_that("audit_vanity_urs fails on missing protocol", {
  apps <- list(
    list(url = "http://myserver.com/hello"),
    list(url = "http://myserver.com/goodbye/")
  )
  expect_error(
    audit_vanity_urls(apps, "myserver.com"),
    "protocol"
  )
})
test_that("audit_vanity_urls handles slashes", {
  apps <- list(
    list(url = "http://myserver.com/hello"),
    list(url = "http://myserver.com/goodbye/"),
    list(url = "http://myserver.com/hello/goodbye/")
  )

  expect_equal(
    audit_vanity_urls(apps, "http://myserver.com"),
    c("/hello/", "/goodbye/", "/hello/goodbye/")
  )

  apps2 <- list(
    list(url = "https://myserver.com/rstudio/hello"),
    list(url = "https://myserver.com/rstudio/goodbye/"),
    list(url = "https://myserver.com/rstudio/hello/goodbye/")
  )

  expect_equal(
    audit_vanity_urls(apps2, "https://myserver.com/rstudio/"),
    c("/hello/", "/goodbye/", "/hello/goodbye/")
  )
})

without_internet({
  test_that("audit_logs query params", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      con$audit_logs(),
      "https://connect.example/__api__/v1/audit_logs?limit=500&ascOrder=true"
    )

    expect_GET(
      con$audit_logs(limit = 1000, previous = "asdf", nxt = "qwer"),
      "https://connect.example/__api__/v1/audit_logs?limit=500&previous=asdf&ascOrder=true&next=qwer"
    )
  })
})
