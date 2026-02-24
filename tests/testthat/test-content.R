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
  expect_error(verify_content_name(
    "abcdefghijklmnopqrstuvwxyabcdefghijklmnopqrstuvwxyabcdefghijklmno"
  ))
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
    suppressMessages(trace(
      "browse_url",
      where = connectapi::browse_solo,
      tracer = quote({
        browseURL <- # nolint: object_name_linter
          function(x) {
            warning(paste("Opening", x))
          }
      }),
      at = 1,
      print = FALSE
    ))
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
    withr::local_options(list(rlib_warning_verbosity = "verbose"))

    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    item <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")
    expect_warning(
      expect_PATCH(
        item$update(description = "new description"),
        "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066",
        '{"description":"new description"}'
      ),
      "the server version is not exposed by this Posit Connect instance"
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
      item$permissions_add(
        "a01792e3-2e67-402e-99af-be04a48da074",
        "user",
        "viewer"
      ),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/permissions",
      '{"principal_guid":"a01792e3-2e67-402e-99af-be04a48da074","principal_type":"user","role":"viewer"}'
    )

    # Update one
    expect_PUT(
      item$permissions_update(
        94,
        "a01792e3-2e67-402e-99af-be04a48da074",
        "user",
        "editor"
      ),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/permissions/94",
      '{"principal_guid":"a01792e3-2e67-402e-99af-be04a48da074","principal_type":"user","role":"editor"}'
    )

    expect_DELETE(
      item$permissions_delete("a01792e3-2e67-402e-99af-be04a48da074"),
      "https://connect.example/__api__/v1/content/f2f37341-e21d-3d80-c698-a935ad614066/permissions"
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

    expect_GET(
      con$content(guid = 1),
      "https://connect.example/__api__/v1/content/1?include=tags%2Cowner"
    )

    expect_GET(
      con$content(guid = 1, include = "owner,tags"),
      "https://connect.example/__api__/v1/content/1?include=owner%2Ctags"
    )
  })
})

with_mock_api({
  test_that("content_render() calls the correct endpoint, returns task on success", {
    client <- Connect$new(
      server = "https://connect.example",
      api_key = "not-a-key"
    )
    x <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
    render_task <- content_render(x)
    expect_equal(render_task$task[["id"]], "v9XYo7OKkAQJPraI")
    expect_equal(render_task$connect, client)
    # TODO think about how to get variant key into response
  })

  test_that("content_render() can render a non-default variant", {
    client <- Connect$new(
      server = "https://connect.example",
      api_key = "not-a-key"
    )
    x <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
    render_task <- content_render(x, variant_key = "SECOND")
    expect_equal(render_task$task[["id"]], "variant2_task_id")
    expect_equal(render_task$connect, client)
    # TODO think about how to get variant key into response
  })

  test_that("content_render() raises an error when called on interactive content", {
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    x <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
    expect_error(
      content_render(x),
      "Render not supported for application mode: shiny. Did you mean content_restart()?",
      fixed = TRUE
    )
  })

  test_that("content_restart() calls the correct endpoint", {
    client <- Connect$new(
      server = "https://connect.example",
      api_key = "not-a-key"
    )
    x <- content_item(client, "8f37d6e0-3395-4a2c-aa6a-d7f2fe1babd0")
    expect_PATCH(
      content_restart(x),
      url = "https://connect.example/__api__/v1/content/8f37d6e0/environment"
    )
  })

  test_that("content_restart() raises an error when called on interactive content", {
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    x <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
    expect_error(
      content_restart(x),
      "Restart not supported for application mode: quarto-static. Did you mean content_render()?",
      fixed = TRUE
    )
  })

  test_that("content$default_variant gets the default variant", {
    withr::local_options(list(rlib_warning_verbosity = "verbose"))
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    x <- content_item(client, "951bf3ad-82d0-4bca-bba8-9b27e35c49fa")
    expect_warning(
      v <- x$default_variant,
      "experimental"
    )
    expect_identical(v$key, "WrEKKa77")
  })
})

# jobs -----

test_that("get_jobs() using the old and new endpoints returns sensible results", {
  with_mock_api({
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )

    item <- content_item(client, "8f37d6e0")
    jobs_v1 <- get_jobs(item)
  })

  with_mock_dir("2024.07.0", {
    jobs_v0 <- get_jobs(item)
  })

  # Connect 2025.01.0 also provides `content_id` and `content_guid` columns.
  # For older versions, `connectapi` adds them, so objects should all be identical.
  with_mock_dir("2025.01.0", {
    jobs_v1_2025_01_0 <- get_jobs(item)
  })

  # Columns we expect to be identical
  common_cols <- c(
    "id",
    "pid",
    "key",
    "app_id",
    "app_guid",
    "content_id",
    "content_guid",
    "variant_id",
    "bundle_id",
    "start_time",
    "end_time",
    "tag",
    "exit_code",
    "hostname"
  )
  expect_identical(
    jobs_v1[common_cols],
    jobs_v0[common_cols]
  )
  expect_identical(
    jobs_v1[common_cols],
    jobs_v1_2025_01_0[common_cols]
  )

  # Status columns line up as expected
  expect_equal(jobs_v1$status, c(0L, 0L, 2L, 2L, 2L, 2L))
  expect_equal(jobs_v0$status, c(0L, 0L, NA, NA, NA, NA))
  expect_equal(jobs_v1_2025_01_0$status, c(0L, 0L, 2L, 2L, 2L, 2L))
})

with_mock_api({
  client <- Connect$new(
    server = "http://connect.example",
    api_key = "not-a-key"
  )
  test_that("get_job_list() returns expected data", {
    item <- content_item(client, "8f37d6e0")
    job_list <- get_job_list(item)

    expect_equal(
      purrr::map_chr(job_list, "id"),
      c("40793542", "40669829", "40097386", "40096649", "40080413", "39368207")
    )

    expect_equal(
      purrr::map_chr(job_list, "app_guid"),
      rep("8f37d6e0", 6)
    )

    expect_equal(
      purrr::map(job_list, "client"),
      list(client, client, client, client, client, client)
    )
  })

  test_that("terminate_jobs() returns expected data when active jobs exist", {
    item <- content_item(client, "8f37d6e0")
    expect_equal(
      terminate_jobs(item),
      tibble::tibble(
        app_id = c(NA, 52389L),
        app_guid = c(NA, "8f37d6e0"),
        job_key = c("waaTO7v75I84S1hQ", "k3sHkEoWJNwQim7g"),
        job_id = c(NA_integer_, 40669829L),
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
      expect_equal(terminate_jobs(item), tibble::tibble()),
      "No active jobs found."
    )
  })
})

test_that("an error is raised when terminate_jobs() calls a bad URL", {
  with_mock_api({
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    item <- content_item(client, "8f37d6e0")
  })

  with_mock_dir("2024.07.0", {
    expect_error(
      terminate_jobs(item, "waaTO7v75I84S1hQ")
    )
  })
})

test_that("get_log() gets job logs", {
  with_mock_api({
    withr::local_options(list(rlib_warning_verbosity = "verbose"))

    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    item <- content_item(client, "8f37d6e0")
    job_list <- get_job_list(item)
    # This job's log is present at {mock_dir}/v1/content/8f37d6e0/jobs/mxPGVOMVk6f8dso2/log.json.
    job <- purrr::keep(job_list, ~ .x$key == "mxPGVOMVk6f8dso2")[[1]]
    expect_warning(
      log <- get_log(job),
      "the server version is not exposed by this Posit Connect instance"
    )
    expect_identical(
      log,
      tibble::tibble(
        source = c("stderr", "stderr", "stderr"),
        timestamp = structure(
          c(1733512169.9480169, 1733512169.9480703, 1733512169.9480758),
          tzone = Sys.timezone(),
          class = c("POSIXct", "POSIXt")
        ),
        data = c(
          "[rsc-session] Content GUID: 8f37d6e0",
          "[rsc-session] Content ID: 52389",
          "[rsc-session] Bundle ID: 127015"
        )
      )
    )

    expect_GET(
      expect_warning(get_log(job, max_log_lines = 10)),
      "http://connect.example/__api__/v1/content/8f37d6e0/jobs/mxPGVOMVk6f8dso2/log?maxLogLines=10"
    )
  })
})

test_that("get_content_packages() gets packages", {
  with_mock_api({
    withr::local_options(list(rlib_warning_verbosity = "verbose"))
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    item <- content_item(client, "8f37d6e0")
    expect_warning(
      expect_identical(
        get_content_packages(item),

        tibble::tibble(
          language = c("r", "r", "r"),
          name = c("askpass", "backports", "base64enc"),
          version = c("1.2.1", "1.5.0", "0.1-3"),
          hash = c(
            "449d6f179d9b3b5df1dc44fad2930b83",
            "6c5e6997f9a6836358ea459b93f2bc39",
            "c590d29e555926af053055e23ee79efb"
          )
        )
      ),
      "the server version is not exposed by this Posit Connect instance"
    )
  })
})

# content search ----

with_mock_dir("2025.09.0", {
  client <- Connect$new(
    server = "https://connect.example",
    api_key = "not-a-key"
  )

  test_that("content search returns the expected list of content", {
    res <- search_content(client, q = "sea bream")
    expect_equal(
      class(res[[1]]),
      c("Content", "R6")
    )
    expect_equal(
      purrr::map_chr(res, list("content", "guid")),
      c("c9f68287", "53032a0e")
    )
    expect_equal(
      purrr::map_chr(res, list("content", "title")),
      c("sea bream report", "sea bream dashboard")
    )
  })

  test_that("content search fetches multiple pages correctly", {
    res <- search_content(client, q = "blobfish")
    expect_equal(length(res), 4)
    expect_equal(
      purrr::map_chr(res, list("content", "title")),
      c(
        "blobfish dashboard",
        "blobfish api",
        "blobfish report",
        "blobfish report (v2)"
      )
    )
  })

  test_that("search_content() can be converted to a data frame correctly", {
    content_df <- search_content(client, q = "sea bream") |>
      as_tibble()
    expect_true(all(c("guid", "name", "title") %in% names(content_df)))
    expect_equal(
      content_df$title,
      c("sea bream report", "sea bream dashboard")
    )
    expect_equal(
      content_df$guid,
      c("c9f68287", "53032a0e")
    )
  })
})

test_that("content search uses default page_size of 500 and page_number of 1", {
  without_internet(
    expect_GET(
      search_content(client, q = "bream"),
      "https://connect.example/__api__/v1/search/content?q=bream&page_number=1&page_size=500&include=owner%2Cvanity_url" #nolint
    )
  )
})

test_that("content search passes arbitrary parameters through ... to query string", {
  without_internet(
    expect_GET(
      search_content(
        client,
        q = "bream",
        future_param = "value"
      ),
      "https://connect.example/__api__/v1/search/content?q=bream&page_number=1&page_size=500&include=owner%2Cvanity_url&future_param=value" #nolint
    )
  )
})

test_that("the inner .search_content() func calls the endpoint correctly", {
  without_internet(
    expect_GET(
      .search_content(
        client,
        q = "bream",
        page_number = 2,
        page_size = 20,
        include = "owner"
      ),
      "https://connect.example/__api__/v1/search/content?q=bream&page_number=2&page_size=20&include=owner"
    )
  )
})

test_that("content search errors on Connect < 2024.04.0", {
  client <- MockConnect$new("2024.04.0")
  client$mock_response(
    "GET",
    "v1/search/content",
    content = list()
  )
  expect_no_error(search_content(client))

  client <- MockConnect$new("2024.03.0")
  expect_error(
    search_content(client),
    "ERROR: This feature requires Posit Connect version 2024.04.0 but you are using 2024.03.0."
  )
})

# lock content ----

with_mock_dir("2025.09.0", {
  test_that("lock_content() and unlock_content() make the expected requests", {
    client <- Connect$new(
      server = "https://connect.example",
      api_key = "not-a-key"
    )
    client$version # Hydrate version
    item <- content_item(client, "6632a162")
    without_internet({
      expect_PATCH(
        lock_content(item),
        "https://connect.example/__api__/v1/content/6632a162",
        '{"locked":true,"locked_message":""}'
      )
      expect_PATCH(
        lock_content(item, "ACCESS DENIED"),
        "https://connect.example/__api__/v1/content/6632a162",
        '{"locked":true,"locked_message":"ACCESS DENIED"}'
      )
      expect_PATCH(
        unlock_content(item),
        "https://connect.example/__api__/v1/content/6632a162",
        '{"locked":false,"locked_message":""}'
      )
    })
  })
})

test_that("lock_content() and unlock_content() error on Connect < 2024.08.0", {
  client <- MockConnect$new("2024.07.0")
  client$mock_response(
    "GET",
    "v1/content/test-guid",
    content = list(
      guid = "test-guid",
      title = "Test Content",
      app_mode = "static"
    )
  )
  item <- content_item(client, "test-guid")
  expect_error(
    lock_content(item),
    "ERROR: This feature requires Posit Connect version 2024.08.0 but you are using 2024.07.0."
  )
  expect_error(
    unlock_content(item),
    "ERROR: This feature requires Posit Connect version 2024.08.0 but you are using 2024.07.0."
  )
})

test_that("content_restart retries if delete fails", {
  mock_connect <- MockConnect$new("2025.09.0")
  mock_connect$mock_response(
    "GET",
    "v1/content/test-guid",
    content = list(
      guid = "test-guid",
      name = "test-content",
      title = "Test Content",
      app_mode = "shiny",
      content_url = "https://connect.example/content/test-guid/",
      dashboard_url = "https://connect.example/connect/#/apps/test-guid"
    )
  )

  # First PATCH to set the env var (succeeds)
  mock_connect$mock_response(
    "PATCH",
    "v1/content/test-guid/environment",
    content = list(),
    status_code = 200L
  )

  # Second PATCH to delete the env var (fails with 500)
  mock_connect$mock_response(
    "PATCH",
    "v1/content/test-guid/environment",
    content = list(error = "Internal server error"),
    status_code = 500L
  )

  # Third PATCH to delete the env var (retry succeeds)
  mock_connect$mock_response(
    "PATCH",
    "v1/content/test-guid/environment",
    content = list(),
    status_code = 200L
  )

  # Get the content item
  item <- content_item(mock_connect, "test-guid")

  expect_no_error(content_restart(item))
  expect_equal(
    mock_connect$call_log,
    c(
      "GET https://connect.example/__api__/v1/content/test-guid",
      "PATCH https://connect.example/__api__/v1/content/test-guid/environment",
      "PATCH https://connect.example/__api__/v1/content/test-guid/environment",
      "PATCH https://connect.example/__api__/v1/content/test-guid/environment"
    )
  )
})

test_that("content_restart doesn't retry if error isn't a 500", {
  mock_connect <- MockConnect$new("2025.09.0")
  mock_connect$mock_response(
    "GET",
    "v1/content/test-guid",
    content = list(
      guid = "test-guid",
      name = "test-content",
      title = "Test Content",
      app_mode = "shiny",
      content_url = "https://connect.example/content/test-guid/",
      dashboard_url = "https://connect.example/connect/#/apps/test-guid"
    )
  )

  mock_connect$mock_response(
    "PATCH",
    "v1/content/test-guid/environment",
    content = list(),
    status_code = 200L
  )

  mock_connect$mock_response(
    "PATCH",
    "v1/content/test-guid/environment",
    content = list(error = "Bad request"),
    status_code = 400L
  )

  item <- content_item(mock_connect, "test-guid")
  expect_warning(content_restart(item))
  # only 2 PATCHes
  expect_equal(
    mock_connect$call_log,
    c(
      "GET https://connect.example/__api__/v1/content/test-guid",
      "PATCH https://connect.example/__api__/v1/content/test-guid/environment",
      "PATCH https://connect.example/__api__/v1/content/test-guid/environment"
    )
  )
})
