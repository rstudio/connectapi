context("get")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

test_that("get_users works", {
  users <- get_users(test_conn_1)
  
  expect_is(users, c("tbl_df", "tbl", "data.frame"))
  expect_identical(vctrs::vec_ptype(users), vctrs::vec_ptype(connectapi_ptypes$users))
})

test_that("get_groups works", {
  groups_list <- get_groups(test_conn_1)
  expect_is(groups_list, c("tbl_df", "tbl", "data.frame"))
  
  expect_identical(vctrs::vec_ptype(groups_list), vctrs::vec_ptype(connectapi_ptypes$groups))
})

test_that("get_content works", {
  scoped_experimental_silence()
  content_list <- get_content(test_conn_1)
  expect_is(content_list, c("tbl_df", "tbl", "data.frame"))
  
  expect_identical(vctrs::vec_ptype(content_list), vctrs::vec_ptype(connectapi_ptypes$content))
})

test_that("get_usage_shiny works", {
  shiny_usage <- get_usage_shiny(test_conn_1)
  expect_is(shiny_usage, c("tbl_df", "tbl", "data.frame"))
  
  expect_identical(vctrs::vec_ptype(shiny_usage), vctrs::vec_ptype(connectapi_ptypes$usage_shiny))
})

test_that("get_usage_static works", {
  content_visits <- get_usage_static(test_conn_1)
  expect_is(content_visits, c("tbl_df", "tbl", "data.frame"))
  
  expect_identical(vctrs::vec_ptype(content_visits), vctrs::vec_ptype(connectapi_ptypes$usage_static))
})

test_that("get_audit_logs works", {
  audit_list <- get_audit_logs(test_conn_1)
  expect_is(audit_list, c("tbl_df", "tbl", "data.frame"))
  
  expect_identical(vctrs::vec_ptype(audit_list), vctrs::vec_ptype(connectapi_ptypes$audit_logs))
})

test_that("get_procs works", {
  scoped_experimental_silence()
  proc_data <- get_procs(test_conn_1)
  
  # TODO: This is not a great test, since no processes are running
  # we could always start a content restoration...
  expect_is(proc_data, "tbl_df")
  expect_identical(vctrs::vec_ptype(proc_data), vctrs::vec_ptype(connectapi_ptypes$procs))
})
