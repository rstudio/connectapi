context("tbl_connect")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

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
  expect_error(
    tbl_connect(test_conn_1, "bad_from")
  )
})

test_that("users works", {
  users <- tbl_connect(test_conn_1, "users")
  expect_is(users, c("tbl_connect", "tbl_lazy", "tbl"))
  
  users_local <- users %>% dplyr::collect()
  expect_is(users_local, c("tbl_df", "tbl", "data.frame"))
  
  expect_true(is.na(nrow(users)))
  expect_is(colnames(users), "character")
  expect_gt(length(colnames(users)), 1)
  
  expect_equal(vctrs::vec_ptype(users_local), vctrs::vec_ptype(connectapi_ptypes$users))
})

test_that("usage_static works", {
  content_visits <- tbl_connect(test_conn_1, "usage_static")
  expect_is(content_visits, c("tbl_connect", "tbl_lazy", "tbl"))
  
  content_visits_local <- content_visits %>% dplyr::collect()
  expect_is(content_visits_local, c("tbl_df", "tbl", "data.frame"))
  
  expect_true(is.na(nrow(content_visits)))
  expect_is(colnames(content_visits), "character")
  expect_gt(length(colnames(content_visits)), 1)
  
  expect_equal(vctrs::vec_ptype(content_visits_local), vctrs::vec_ptype(connectapi_ptypes$usage_static))
})

test_that("usage_shiny works", {
  shiny_usage <- tbl_connect(test_conn_1, "usage_shiny")
  expect_is(shiny_usage, c("tbl_connect", "tbl_lazy", "tbl"))
  
  shiny_usage_local <- shiny_usage %>% dplyr::collect()
  expect_is(shiny_usage_local, c("tbl_df", "tbl", "data.frame"))
  
  expect_true(is.na(nrow(shiny_usage)))
  expect_is(colnames(shiny_usage), "character")
  expect_gt(length(colnames(shiny_usage)), 1)
  
  expect_equal(vctrs::vec_ptype(shiny_usage_local), vctrs::vec_ptype(connectapi_ptypes$usage_shiny))
})

test_that("content works", {
  scoped_experimental_silence()
  content_list <- tbl_connect(test_conn_1, "content")
  expect_is(content_list, c("tbl_connect", "tbl_lazy", "tbl"))
  
  content_list_local <- content_list %>% dplyr::collect()
  expect_is(content_list_local, c("tbl_df", "tbl", "data.frame"))
  
  expect_true(is.na(nrow(content_list)))
  expect_is(colnames(content_list), "character")
  expect_gt(length(colnames(content_list)), 1)
  
  # https://github.com/r-lib/testthat/issues/985
  #skip("currently segfaults")
  expect_equal(vctrs::vec_ptype(content_list_local), vctrs::vec_ptype(connectapi_ptypes$content))
})

test_that("groups works", {
  scoped_experimental_silence()
  groups_list <- tbl_connect(test_conn_1, "groups")
  expect_is(groups_list, c("tbl_connect", "tbl_lazy", "tbl"))
  
  groups_list_local <- groups_list %>% dplyr::collect()
  expect_is(groups_list_local, c("tbl_df", "tbl", "data.frame"))
  
  expect_true(is.na(nrow(groups_list)))
  expect_is(colnames(groups_list), "character")
  expect_gt(length(colnames(groups_list)), 1)
  
  expect_equal(vctrs::vec_ptype(groups_list_local), vctrs::vec_ptype(connectapi_ptypes$groups))
})

test_that("audit_logs works", {
  scoped_experimental_silence()
  audit_list <- tbl_connect(test_conn_1, "audit_logs")
  expect_is(audit_list, c("tbl_connect", "tbl_lazy", "tbl"))
  
  audit_list_local <- audit_list %>% dplyr::collect()
  expect_is(audit_list_local, c("tbl_df", "tbl", "data.frame"))
  
  expect_true(is.na(nrow(audit_list)))
  expect_is(colnames(audit_list), "character")
  expect_gt(length(colnames(audit_list)), 1)
  
  expect_equal(vctrs::vec_ptype(audit_list_local), vctrs::vec_ptype(connectapi_ptypes$audit_logs))
})
