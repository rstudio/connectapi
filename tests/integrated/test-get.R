context("get")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

# get --------------------------------------------

test_that("get_users works", {
  users <- get_users(test_conn_1)

  expect_is(users, c("tbl_df", "tbl", "data.frame"))
  expect_equal(purrr::map_chr(vctrs::vec_ptype(users), typeof), purrr::map_chr(vctrs::vec_ptype(connectapi_ptypes$users), typeof))
})

test_that("get_groups works", {
  groups_list <- get_groups(test_conn_1)
  expect_is(groups_list, c("tbl_df", "tbl", "data.frame"))

  expect_equal(vctrs::vec_ptype(groups_list), vctrs::vec_ptype(connectapi_ptypes$groups))
})

test_that("get_content works", {
  scoped_experimental_silence()
  content_list <- get_content(test_conn_1)
  expect_is(content_list, c("tbl_df", "tbl", "data.frame"))

  # https://github.com/r-lib/testthat/issues/985
  skip("currently segfaults")
  expect_equal(vctrs::vec_ptype(content_list), vctrs::vec_ptype(connectapi_ptypes$content))
})

test_that("get_usage_shiny works", {
  shiny_usage <- get_usage_shiny(test_conn_1)
  expect_is(shiny_usage, c("tbl_df", "tbl", "data.frame"))

  expect_equal(vctrs::vec_ptype(shiny_usage), vctrs::vec_ptype(connectapi_ptypes$usage_shiny))
})

test_that("get_usage_static works", {
  content_visits <- get_usage_static(test_conn_1)
  expect_is(content_visits, c("tbl_df", "tbl", "data.frame"))

  expect_equal(vctrs::vec_ptype(content_visits), vctrs::vec_ptype(connectapi_ptypes$usage_static))
})

test_that("get_audit_logs works", {
  audit_list <- get_audit_logs(test_conn_1)
  expect_is(audit_list, c("tbl_df", "tbl", "data.frame"))

  expect_equal(vctrs::vec_ptype(audit_list), vctrs::vec_ptype(connectapi_ptypes$audit_logs))
})

test_that("get_procs works", {
  scoped_experimental_silence()
  proc_data <- get_procs(test_conn_1)

  # TODO: This is not a great test, since no processes are running
  # we could always start a content restoration...
  expect_is(proc_data, "tbl_df")
  expect_equal(vctrs::vec_ptype(proc_data), vctrs::vec_ptype(connectapi_ptypes$procs))
})

# experimental --------------------------------------------

test_that("content_list_with_permissions works", {
  scoped_experimental_silence()

  rlang::with_options(progress_enabled = FALSE, cl <- content_list_with_permissions(test_conn_1))

  expect_true("permission" %in% names(cl))
  expect_is(cl, "tbl_df")

})

test_that("content_list_with_permissions predicate works", {
  scoped_experimental_silence()

  # deploy a static app so we know it is not empty
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  uniq_id <- uuid::UUIDgenerate()
  deployed <- deploy(test_conn_1, bnd, uniq_id)

  rlang::with_options(progress_enabled = FALSE, cl <- content_list_with_permissions(test_conn_1, .p = ~ .x$guid == deployed$get_content()$guid))

  expect_true("permission" %in% names(cl))
  expect_is(cl, "tbl_df")
  expect_equal(nrow(cl), 1)
})

test_that("content_list_guid_has_access works", {
  scoped_experimental_silence()

  # deploy a static app so we know it is not empty
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  uniq_id <- uuid::UUIDgenerate()
  deployed <- deploy(test_conn_1, bnd, uniq_id)

  rlang::with_options(progress_enabled = FALSE, cl <- content_list_with_permissions(test_conn_1))

  my_guid <- test_conn_1$me()$guid
  filt <- content_list_guid_has_access(cl, my_guid)
  expect_true("permission" %in% names(filt))
  expect_true(nrow(filt) <= nrow(cl))

  expect_true(nrow(filt) > 0)
  expect_true(deployed$get_content()$guid %in% filt$guid)
})

test_that("content_list_by_tag works", {
  skip("not yet tested")
})
