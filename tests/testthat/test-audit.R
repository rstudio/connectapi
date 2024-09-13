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

  test_that("vanity_is_available", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_true(vanity_is_available(con, "not-currently/used"))
    expect_false(vanity_is_available(con, "/streamlit/my-app/"))
    expect_false(vanity_is_available(con, "streamlit/my-app"))
    expect_false(vanity_is_available(con, "https://connect.example/streamlit/my-app"))
  })
})
