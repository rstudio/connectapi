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

with_mock_api({
  test_that("audit_r_versions does not error", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("gridExtra")
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    content <- get_content(con)

    expect_s3_class(audit_r_versions(content), "gtable")
  })

  test_that("audit_access_open", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    content <- get_content(con)

    expect_identical(audit_access_open(content, "all"), "My-Streamlit-app")
    expect_identical(
      audit_access_open(content, "logged_in"),
      c("team-admin-dashboard", "Performance-Data-1671216053560")
    )
  })

  test_that("audit_runas", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    content <- get_content(con)
    expect_equal(
      audit_runas(content),
      tibble::tibble(
        app_name = "My-Streamlit-app",
        run_as_user = "current user"
      )
    )
  })
})
