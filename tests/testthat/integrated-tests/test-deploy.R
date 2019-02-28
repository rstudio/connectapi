context("deploy")

# should connect with env vars
test_conn_1 <- Connect$new(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- Connect$new(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

test_that("bundle_dir deploys", {
  dir_path <- rprojroot::find_testthat_root_file("examples/static")
  tmp_file <- fs::file_temp(pattern = "bundle", ext = ".tar.gz")
  bund <- bundle_dir(test_conn_1, path = dir_path, filename = tmp_file)
  
  expect_equal(tmp_file, bund$path)
  
  tsk <- deploy(bundle = bund, name = cont1_name, title = cont1_title)
  
  cont1_guid <<- tsk$get_content()$guid
  cont1_content <<- tsk
  
  # how should we test that deployment happened?
  expect_true(validate_R6_class("Content", tsk))
  expect_equal(tsk$get_content()$name, cont1_name)
  expect_equal(tsk$get_content()$title, cont1_title)
})

test_that("bundle_path deploys", {
  tar_path <- rprojroot::find_testthat_root_file("examples/static.tar.gz")
  bund <- bundle_path(test_conn_1, path = tar_path)
  
  expect_equal(tar_path, as.character(bund$path))
  
  # deploy to a new endpoint
  tsk <- deploy(bundle = bund)
  
  # how should we test that deployment happened?
  expect_true(validate_R6_class("Content", tsk))
})

test_that("set_image_path works", {
  img_path <- rprojroot::find_testthat_root_file("examples/logo.png")
  
  res <- set_image_path(cont1_content, img_path)
  
  expect_true(validate_R6_class("Content", res))
})

test_that("set_image_url works", {
  # need to find a reliable image URL that is small
  skip("not implemented yet")
})

test_that("set_image_webshot works", {
  res <- set_image_webshot(cont1_content, delay = 5)
  
  expect_true(validate_R6_class("Content", res))
})

test_that("set_vanity_url works", {
  res <- set_vanity_url(cont1_content, "/test-vanity-url")
  
  expect_true(validate_R6_class("Content", res))
})
