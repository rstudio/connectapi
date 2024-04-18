# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("can create content", {
  cont1 <- test_conn_1$content_create(name = cont1_name, title = cont1_title)
  expect_equal(cont1$name, cont1_name)
  expect_equal(cont1$title, cont1_title)

  get_cont1 <- test_conn_1$content(guid = cont1$guid)
  expect_identical(get_cont1, cont1)
  cont1_guid <<- cont1$guid
})

test_that("can upload and deploy content", {
  cont1_bundle <<- bundle_dir(
    rprojroot::find_package_root_file("tests/testthat/test-plot")
  )
  expect_true(fs::file_exists(cont1_bundle$path))

  res <- test_conn_1$content_upload(bundle_path = cont1_bundle$path, guid = cont1_guid)
  expect_false(is.null(res))
  expect_silent(as.integer(res[["bundle_id"]]))

  task <- test_conn_1$content_deploy(guid = cont1_guid, bundle_id = res[["bundle_id"]])
  expect_type(task[["task_id"]], "character")
})

test_that("can promote content to another server", {
  # TODO : Intermittent failures here... with a 404 response on GET
  # during the download_bundle... connect.R:154
  res <- promote(
    from = Sys.getenv("TEST_1_SERVER"),
    from_key = Sys.getenv("TEST_1_API_KEY"),
    to = Sys.getenv("TEST_2_SERVER"),
    to_key = Sys.getenv("TEST_2_API_KEY"),
    name = cont1_name
  )

  expect_type(res, "character")

  cont1_2 <- content_ensure(
    connect = test_conn_2,
    name = cont1_name
  )

  expect_identical(cont1_name, cont1_2[["name"]])
})

test_that("content_ensure works with guid", {
  c1 <- content_ensure(test_conn_1, guid = cont1_guid)
  expect_identical(c1[["guid"]], cont1_guid)

  fake_guid <- paste0(cont1_guid, "-does-not-exist")
  expect_warning({
    c2 <- content_ensure(test_conn_1, guid = fake_guid)
  })
  expect_false(identical(c2[["guid"]], cont1_guid))
})

test_that("content_ensure works with name", {
  expect_message(c_new <- content_ensure(test_conn_1))
  expect_type(c_new[["guid"]], "character")

  expect_message(
    c_same <- content_ensure(test_conn_1, name = c_new[["name"]])
  )

  expect_identical(c_new[["name"]], c_same[["name"]])
  expect_identical(c_new[["guid"]], c_same[["guid"]])

  c_newname <- paste0(c_new[["name"]], "-alternate")
  c_title <- "Some Title"
  c_desc <- "Some Description"
  expect_message(
    c_diff <- content_ensure(test_conn_1,
      name = c_newname,
      title = c_title, description = c_desc
    )
  )

  expect_false(identical(c_new[["name"]], c_diff[["name"]]))
  expect_false(identical(c_new[["guid"]], c_diff[["guid"]]))
  expect_identical(c_newname, c_diff[["name"]])
  expect_identical(c_title, c_diff[["title"]])
  expect_identical(c_desc, c_diff[["description"]])
})

test_that("content_ensure fails when not permitted", {
  permit_guid <- uuid::UUIDgenerate()
  permit_name <- create_random_name()

  # duplicates to ensure it does not create
  expect_error(content_ensure(test_conn_1, guid = permit_guid, .permitted = c("existing")), "not found on")
  expect_error(content_ensure(test_conn_1, guid = permit_guid, .permitted = c("existing")), "not found on")

  # duplicates to ensure it does not create
  expect_error(content_ensure(test_conn_1, name = permit_name, .permitted = c("existing")), "not found on")
  expect_error(content_ensure(test_conn_1, name = permit_name, .permitted = c("existing")), "not found on")

  # actually create
  invisible(content_ensure(test_conn_1, name = permit_name))

  # error because we expect new
  expect_error(content_ensure(test_conn_1, name = permit_name, .permitted = c("new")), "already exists")
})
