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
    trace("browse_url", where = connectapi::browse_solo, tracer = quote({
      browseURL <- function(x) warning(paste("Opening", x))
    }), at = 1)
    expect_warning(
      browse_solo(item),
      "Opening https://connect.example/content/f2f37341-e21d-3d80-c698-a935ad614066/"
    )
    expect_warning(
      browse_dashboard(item),
      "Opening https://connect.example/connect/#/apps/f2f37341-e21d-3d80-c698-a935ad614066"
    )
    untrace("browse_url", where = connectapi::browse_solo)
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
})
