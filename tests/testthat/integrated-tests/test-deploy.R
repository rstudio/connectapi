context("deploy")

# should connect with env vars
test_conn_1 <- Connect$new(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- Connect$new(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("bundle_dir deploys", {
  dir_path <- rprojroot::find_testthat_root_file("examples/static")
  tmp_file <- fs::file_temp(pattern = "bundle", ext = ".tar.gz")
  bund <- bundle_dir(test_conn_1, path = dir_path, filename = tmp_file)
  
  expect_equal(tmp_file, bund$path)
  
  tsk <- deploy(bundle = bund, name = cont1_name, title = cont1_title)
  
  cont1_guid <<- tsk$get_content()$guid
  
  expect_equal(tsk$get_content()$name, cont1_name)
  expect_equal(tsk$get_content()$title, cont1_title)
})

test_that("bundle_path deploys", {
  skip("not implemented yet")
})

test_that("set_image_path works", {
  skip("not implemented yet")
})

test_that("set_image_url works", {
  skip("not implemented yet")
})

test_that("set_vanity_url works", {
  skip("not implemented yet")
})
