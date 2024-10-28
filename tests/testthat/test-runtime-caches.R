with_mock_api({
  client <- connect(server = "https://connect.example", api_key = "fake")

  test_that("get_runtime_caches() gets runtime caches", {
    caches <- get_runtime_caches(client)
    expect_equal(
      get_runtime_caches(client),
      tibble::tibble(
        language = c("R", "Python"),
        version = c("4.3.0", "3.11.3"),
        image_name = c("Local", "Local")
      )
    )
  })

  test_that("delete_runtime_cache() prints message and returns NULL when dry_run == TRUE", {
    expect_message(
      res <- delete_runtime_cache(client, "Python", "3.11.3", dry_run = TRUE),
      paste0(
        "Runtime cache deletion dry run finished; ",
        "language = Python, version = 3.11.3, image_name = Local"
      )
    )
    expect_null(res)
  })

  test_that("delete_runtime_cache() returns a task object when dry_run == FALSE", {
    res <- delete_runtime_cache(client, "Python", "3.11.3")
    expect_true(validate_R6_class(res, "Task"))
    expect_equal(res$task$id, "eKm0RFxyzIKR3jnI")
  })

  test_that("delete_runtime_cache() on a nonexistent cache throws a 404 error", {
    expect_error(
      delete_runtime_cache(client, "Python", "3.11.2"),
      "request failed with Client error: \\(404\\) Not Found"
    )
  })
})
