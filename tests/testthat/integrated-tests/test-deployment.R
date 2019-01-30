context("test deployment pipelines")

# should connect with env vars
test_conn_1 <- Connect$new(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- Connect$new(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL

test_that("can create content", {
  cont1 <- test_conn_1$content_create(name = cont1_name, title = cont1_title)
  expect_equal(cont1$name, cont1_name)
  expect_equal(cont1$title, cont1_title)
  
  get_cont1 <- test_conn_1$get_content(guid = cont1$guid)
  expect_identical(get_cont1, cont1)
  cont1_guid <<- cont1$guid
})

test_that("can upload and deploy content", {
  bundle <- dir_bundle(
      rprojroot::find_testthat_root_file("test-plot"), 
      "../test-ex-1.tar.gz"
  )
  expect_true(fs::file_exists(bundle))
  
  res <- test_conn_1$content_upload(bundle_path = bundle, guid = cont1_guid)
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
