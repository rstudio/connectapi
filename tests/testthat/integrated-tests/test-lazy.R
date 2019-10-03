context("tbl_connect")

# should connect with env vars
test_conn_1 <- connect(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- connect(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

test_that("error on bad 'src' object", {
  expect_error(
    tbl_connect("bad_src", "users"),
    "src.*Connect"
  )
})

test_that("error on bad 'from' value", {
  skip("expect_error is not picking up the failure")
  expect_error(
    tbl_connect(test_conn_1, "bad_from")
  )
})

test_that("users works", {
  users <- tbl_connect(test_conn_1, "users")
  expect_is(users, c("tbl_connect", "tbl_lazy", "tbl"))
  
  users_local <- users %>% dplyr::collect()
  expect_is(users_local, c("tbl_df", "tbl", "data.frame"))
})

test_that("content_visits works", {
  content_visits <- tbl_connect(test_conn_1, "content_visits")
  expect_is(content_visits, c("tbl_connect", "tbl_lazy", "tbl"))
  
  content_visits_local <- content_visits %>% dplyr::collect()
  expect_is(content_visits, c("tbl_df", "tbl", "data.frame"))
})

test_that("shiny_usage works", {
  shiny_usage <- tbl_connect(test_conn_1, "shiny_usage")
  expect_is(shiny_usage, c("tbl_connect", "tbl_lazy", "tbl"))
  
  shiny_usage_local <- shiny_usage %>% dplyr::collect()
  expect_is(shiny_usage, c("tbl_df", "tbl", "data.frame"))
})

test_that("content works", {
  content_list <- tbl_connect(test_conn_1, "content")
  expect_is(content_list, c("tbl_connect", "tbl_lazy", "tbl"))
  
  content_list_local <- content_list %>% dplyr::collect()
  expect_is(content_list_local, c("tbl_df", "tbl", "data.frame"))
})
