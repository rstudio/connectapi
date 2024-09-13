without_internet({
  test_that("Query params to get task", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      con$task("asdf"),
      "https://connect.example/__api__/v1/tasks/asdf?first=0&wait=5"
    )
  })
})
