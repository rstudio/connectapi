context("test deployment pipelines")

# should connect with env vars
test_conn_1 <- Connect$new(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- Connect$new(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

target_repo <- Sys.getenv("TEST_REPO")
target_ref <- Sys.getenv("TEST_REF", "master")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("can create content", {
  cont1 <- test_conn_1$content_create(name = cont1_name, title = cont1_title)
  expect_equal(cont1$name, cont1_name)
  expect_equal(cont1$title, cont1_title)
  
  get_cont1 <- test_conn_1$get_content(guid = cont1$guid)
  expect_identical(get_cont1, cont1)
  cont1_guid <<- cont1$guid
})

test_that("can upload and deploy content", {
  cont1_bundle <<- dir_bundle(
      rprojroot::find_testthat_root_file("test-plot"), 
      "../test-ex-1.tar.gz"
  )
  expect_true(fs::file_exists(cont1_bundle))
  
  res <- test_conn_1$content_upload(bundle_path = cont1_bundle, guid = cont1_guid)
  expect_false(is.null(res))
  expect_silent(as.integer(res[["bundle_id"]]))
  
  task <- test_conn_1$content_deploy(guid = cont1_guid, bundle_id = res[["bundle_id"]])
  expect_is(task[["task_id"]], "character")
  
  res <- poll_task(test_conn_1, task_id = task[["task_id"]])
  expect_null(res)
})

test_that("can promote content to another server", {
  res <- promote(
    from = Sys.getenv("TEST_SERVER_1"),
    from_key = Sys.getenv("TEST_KEY_1"),
    to = Sys.getenv("TEST_SERVER_2"),
    to_key = Sys.getenv("TEST_KEY_2"),
    name = cont1_name
  )
  
  expect_is(res, "character")
  
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
  expect_warning({c2 <- content_ensure(test_conn_1, guid = fake_guid)})
  expect_false(identical(c2[["guid"]], cont1_guid))
})

test_that("content_ensure works with name", {
  expect_message(c_new <- content_ensure(test_conn_1))
  expect_is(c_new[["guid"]], "character")
  
  expect_message(
    c_same <- content_ensure(test_conn_1, name = c_new[["name"]])
  )
  
  expect_identical(c_new[["name"]], c_same[["name"]])
  expect_identical(c_new[["guid"]], c_same[["guid"]])
  
  c_newname <- paste0(c_new[["name"]], "-alternate")
  c_title <- "Some Title"
  c_desc <- "Some Description"
  expect_message(
    c_diff <- content_ensure(test_conn_1, name = c_newname, 
                             title = c_title, description = c_desc)
  )
  
  expect_false(identical(c_new[["name"]], c_diff[["name"]]))
  expect_false(identical(c_new[["guid"]], c_diff[["guid"]]))
  expect_identical(c_newname, c_diff[["name"]])
  expect_identical(c_title, c_diff[["title"]])
  expect_identical(c_desc, c_diff[["description"]])
  
})

test_that("download_github works", {
  download_dir <- download_github(target_repo, target_ref)
  expect_true(fs::dir_exists(download_dir))
})

test_that("download_github fails when it gets a bad HTTP response", {
  expect_error(
    download_dir_fail <- download_github(
      paste0(target_repo, "-almost-definitely-does-not-exist..."),
      target_ref
    )
    , regexp = "request failed with"
  )
})

test_that("deploy_github works", {
  res <- deploy_github(
    connect = test_conn_1,
    repo = target_repo,
    ref = target_ref,
    filename = ".connect.yml"
  )
  expect_gt(length(res), 0)
  expect_is(res[[1]][["content"]][["guid"]], "character")
  expect_is(res[[1]][["content"]][["name"]], "character")
  expect_is(res[[1]][["task"]][["task_id"]], "character")
})

test_that("deploy_github fails when the file does not exist", {
  # need to mock the `download_github` function
  skip("not currently implemented")
})
