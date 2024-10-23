test_that("get_timezones() gets timeszones from v1 url when available", {
  # With version available
  client <- MockConnect$new()

  client$mock_response(
    path = "v1/timezones",
    content = list(
      list(timezone = "Africa/Abidjan", offset = "+00:00"),
      list(timezone = "Africa/Accra", offset = "+00:00")
    )
  )
  client$mock_response(
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
  client <- MockConnect$new()

  client$mock_response(
    path = "v1/timezones",
    content = "404 page not found",
    status_code = 404L,
    headers = c("Content-Type" = "text/plain; charset=utf-8")
  )
  client$mock_response(
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

  client$mock_response(
    path = "v1/timezones",
    content = "404 page not found",
    status_code = 404L,
    headers = c("Content-Type" = "text/plain; charset=utf-8")
  )
  client$mock_response(
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
