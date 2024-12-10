test_that("verify_content_name works with valid names", {
  short_name <- "123"
  expect_equal(verify_content_name(short_name), short_name)

  long_name <- "abcdefghijklmnopqrstuvwxyabcdefghijklmnopqrstuvwxyabcdefghijklmn"
  expect_equal(verify_content_name(long_name), long_name)

  uuid <- uuid::UUIDgenerate()
  expect_equal(verify_content_name(uuid), uuid)
})

test_that("verify_content_name fails for invalid names", {
  expect_error(verify_content_name("a"))
  expect_error(verify_content_name(NA))
  # 65 characters
  expect_error(verify_content_name("abcdefghijklmnopqrstuvwxyabcdefghijklmnopqrstuvwxyabcdefghijklmno"))
  expect_error(verify_content_name(NULL))
  expect_error(verify_content_name("abc!@#$"))
  expect_error(verify_content_name("123 abc"))
})

test_that("create_random_name works with no length", {
  expect_type(create_random_name(), "character")
})

test_that("create_random_name works with length", {
  expect_equal(nchar(create_random_name(200)), 200)
  expect_equal(nchar(create_random_name(1)), 1)
})

with_mock_api({
  test_that("we can retrieve the content list", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_true(validate_R6_class(con, "Connect"))
    content_list <- get_content(con)
    expect_s3_class(content_list, "data.frame")
    expect_equal(nrow(content_list), 3)
  })

  test_that("we can retrieve a content item", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    item <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")
    expect_true(validate_R6_class(item, "Content"))
    expect_equal(
      item$get_url(),
      "https://connect.example/content/f2f37341-e21d-3d80-c698-a935ad614066/"
    )

    expect_snapshot(print(item))
  })

  test_that("browse URLs", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    item <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")
    # Inject into this function something other than utils::browseURL
    # so we can assert that it is being called without actually trying to open a browser
    suppressMessages(trace("browse_url", where = connectapi::browse_solo, tracer = quote({
      browseURL <- # nolint: object_name_linter
        function(x) {
          warning(paste("Opening", x))
        }
    }), at = 1, print = FALSE))
    expect_warning(
      browse_solo(item),
      "Opening https://connect.example/content/f2f37341-e21d-3d80-c698-a935ad614066/"
    )
    expect_warning(
      browse_dashboard(item),
      "Opening https://connect.example/connect/#/apps/f2f37341-e21d-3d80-c698-a935ad614066"
    )
    suppressMessages(untrace("browse_url", where = connectapi::browse_solo))
  })

  test_that("we can modify a content item", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    item <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")
    expect_PATCH(
      item$update(description = "new description"),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066",
      '{"description":"new description"}'
    )
  })

  test_that("we can delete a content item", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    item <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")
    expect_DELETE(item$danger_delete())
  })

  test_that("we can get the content item's permissions", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    item <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")
    perms <- item$permissions()
    expect_type(perms, "list")
    expect_equal(perms[[1]]$id, 94)

    # Now get perms with id specified
    one_perm <- item$permissions(id = 94)
    expect_equal(one_perm, perms[[1]])
  })

  test_that("we can modify the content item's permissions", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    item <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")

    # Add one
    expect_POST(
      item$permissions_add("a01792e3-2e67-402e-99af-be04a48da074", "user", "viewer"),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/permissions",
      '{"principal_guid":"a01792e3-2e67-402e-99af-be04a48da074","principal_type":"user","role":"viewer"}'
    )

    # Update one
    expect_PUT(
      item$permissions_update(94, "a01792e3-2e67-402e-99af-be04a48da074", "user", "editor"),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/permissions/94",
      '{"principal_guid":"a01792e3-2e67-402e-99af-be04a48da074","principal_type":"user","role":"editor"}'
    )

    expect_DELETE(
      item$permissions_delete("a01792e3-2e67-402e-99af-be04a48da074"),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/permissions"
    )
  })

  test_that("content environment vars", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    item <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")

    expect_GET(
      item$environment(),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/environment"
    )
    expect_PATCH(
      item$environment_set(VAR_NAME = "new_value"),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/environment",
      '[{"name":"VAR_NAME","value":"new_value"}]'
    )
    expect_PATCH(
      item$environment_set(VAR_NAME = NA),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/environment",
      '[{"name":"VAR_NAME","value":null}]'
    )
    expect_PUT(
      item$environment_all(VAR_NAME = "new_value"),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/environment",
      '[{"name":"VAR_NAME","value":"new_value"}]'
    )
    expect_PUT(
      item$environment_all(),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/environment",
      "[]"
    )
  })
})

without_internet({
  test_that("Query params to connect$content()", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      con$content(),
      "https://connect.example/__api__/v1/content?include=tags%2Cowner"
    )

    expect_GET(
      con$content("a01792e3-2e67-402e-99af-be04a48da074"),
      "https://connect.example/__api__/v1/content/a01792e3-2e67-402e-99af-be04a48da074"
    )

    expect_GET(
      con$content(include = NULL),
      "https://connect.example/__api__/v1/content"
    )

    expect_GET(
      con$content(owner_guid = "a01792e3-2e67-402e-99af-be04a48da074"),
      "https://connect.example/__api__/v1/content?owner_guid=a01792e3-2e67-402e-99af-be04a48da074&include=tags%2Cowner"
    )

    expect_GET(
      con$content(name = "A name for content"),
      "https://connect.example/__api__/v1/content?name=A%20name%20for%20content&include=tags%2Cowner"
    )
  })
})

with_mock_api({
  test_that("content_render() calls the correct endpoint, returns task on success", {
    client <- Connect$new(server = "https://connect.example", api_key = "not-a-key")
    x <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
    render_task <- content_render(x)
    expect_equal(render_task$task[["id"]], "v9XYo7OKkAQJPraI")
    expect_equal(render_task$connect, client)
    # TODO think about how to get variant key into response
  })

  test_that("content_render() can render a non-default variant", {
    client <- Connect$new(server = "https://connect.example", api_key = "not-a-key")
    x <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
    render_task <- content_render(x, variant_key = "SECOND")
    expect_equal(render_task$task[["id"]], "variant2_task_id")
    expect_equal(render_task$connect, client)
    # TODO think about how to get variant key into response
  })


  test_that("content_render() raises an error when called on interactive content", {
    client <- Connect$new(server = "http://connect.example", api_key = "not-a-key")
    x <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
    expect_error(
      content_render(x),
      "Render not supported for application mode: shiny. Did you mean content_restart()?",
      fixed = TRUE
    )
  })
})

with_mock_api({
  test_that("content_restart() calls the correct endpoint", {
    client <- Connect$new(server = "https://connect.example", api_key = "not-a-key")
    x <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
    expect_PATCH(content_restart(x), url = "https://connect.example/__api__/v1/content/8f37d6e0/environment")
  })

  test_that("content_restart() raises an error when called on interactive content", {
    client <- Connect$new(server = "http://connect.example", api_key = "not-a-key")
    x <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
    expect_error(
      content_restart(x),
      "Restart not supported for application mode: quarto-static. Did you mean content_render()?",
      fixed = TRUE
    )
  })
})

with_mock_api({
  test_that("content$default_variant gets the default variant", {
    client <- Connect$new(server = "http://connect.example", api_key = "not-a-key")
    x <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
    v <- x$default_variant
    expect_identical(v$key, "WrEKKa77")
  })
})

# jobs -----

test_that("get_jobs() using the old and new endpoints returns sensible results", {
  with_mock_api({
    client <- Connect$new(server = "http://connect.example", api_key = "not-a-key")
    item <- content_item(client, "8f37d6e0")
    jobs_v1 <- get_jobs(item)
  })

  with_mock_dir("2024.07.0", {
    jobs_v0 <- get_jobs(item)
  })

  # Columns we expect to be identical
  common_cols <- c(
    "id", "pid", "key", "app_id", "variant_id", "bundle_id", "start_time",
    "end_time", "tag", "exit_code", "hostname"
  )
  expect_identical(
    jobs_v1[common_cols],
    jobs_v0[common_cols]
  )

  # Status columns line up as expected
  expect_equal(jobs_v1$status, c(0L, 0L, 2L, 2L, 2L, 2L))
  expect_equal(jobs_v0$status, c(0L, 0L, NA, NA, NA, NA))
})

with_mock_api({
  client <- Connect$new(server = "http://connect.example", api_key = "not-a-key")

  test_that("terminate_jobs() returns expected data when active jobs exist", {
    item <- content_item(client, "8f37d6e0")
    expect_equal(
      terminate_jobs(item),
      tibble::tibble(
        app_id = c(NA, 52389L),
        app_guid = c(NA, "8f37d6e0"),
        job_key = c("waaTO7v75I84S1hQ", "k3sHkEoWJNwQim7g"),
        job_id = c(NA, "40669829"),
        result = c(NA, "Order to kill job registered"),
        code = c(163L, NA),
        error = c(
          "The specified job cannot be terminated because it is not active",
          NA
        )
      )
    )
  })

  test_that("terminate_jobs() functions as expected with no active jobs", {
    item <- content_item(client, "01234567")
    expect_message(
      expect_equal(
        terminate_jobs(item),
        tibble::tibble(
          app_id = integer(),
          app_guid = character(),
          job_key = character(),
          job_id = character(),
          result = character(),
          code = integer(),
          error = character()
        )
      ),
      "No active jobs found."
    )
  })
})

test_that("an error is raised when terminate_jobs() calls a bad URL", {
  with_mock_api({
    client <- Connect$new(server = "http://connect.example", api_key = "not-a-key")
    item <- content_item(client, "8f37d6e0")
  })

  with_mock_dir("2024.07.0", {
    expect_error(
      terminate_jobs(item, "waaTO7v75I84S1hQ")
    )
  })
})
