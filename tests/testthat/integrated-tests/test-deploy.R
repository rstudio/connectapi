context("deploy")

# should connect with env vars
test_conn_1 <- Connect$new(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- Connect$new(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("bundle_dir deploys", {
  skip("not implemented yet")
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
