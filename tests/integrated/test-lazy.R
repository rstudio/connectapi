testthat::skip_if_not_installed("dbplyr")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

bnd_static <- bundle_dir(rprojroot::find_package_root_file(
  "tests/testthat/examples/static"
))
tmp_content <- deploy(client, bnd_static)

test_that("error on bad 'src' object", {
  expect_error(
    tbl_connect("bad_src", "users"),
    "src.*Connect"
  )
})

test_that("error on bad 'from' value", {
  expect_error(
    tbl_connect(client, "bad_from")
  )
})

test_that("users works", {
  users <- tbl_connect(client, "users")
  expect_s3_class(users, c("tbl_connect", "tbl_lazy", "tbl"))

  users_local <- users %>% dplyr::collect()
  expect_s3_class(users_local, c("tbl_df", "tbl", "data.frame"))

  expect_true(is.na(nrow(users)))
  expect_type(colnames(users), "character")
  expect_gt(length(colnames(users)), 1)

  expect_true("guid" %in% names(users_local))
  expect_true("email" %in% names(users_local))
  expect_s3_class(users_local$created_time, "POSIXct")
})

test_that("usage_static works", {
  content_visits <- tbl_connect(client, "usage_static")
  expect_s3_class(content_visits, c("tbl_connect", "tbl_lazy", "tbl"))

  content_visits_local <- content_visits %>% dplyr::collect()
  expect_s3_class(content_visits_local, c("tbl_df", "tbl", "data.frame"))

  expect_true(is.na(nrow(content_visits)))
  expect_type(colnames(content_visits), "character")
  expect_gt(length(colnames(content_visits)), 1)

  expect_true("content_guid" %in% names(content_visits_local))
  expect_s3_class(content_visits_local$time, "POSIXct")
})

test_that("usage_shiny works", {
  shiny_usage <- tbl_connect(client, "usage_shiny")
  expect_s3_class(shiny_usage, c("tbl_connect", "tbl_lazy", "tbl"))

  shiny_usage_local <- shiny_usage %>% dplyr::collect()
  expect_s3_class(shiny_usage_local, c("tbl_df", "tbl", "data.frame"))

  expect_true(is.na(nrow(shiny_usage)))
  expect_type(colnames(shiny_usage), "character")

  # No shiny apps are deployed in integration tests, so this may be empty.
  if (nrow(shiny_usage_local) > 0) {
    expect_gt(length(colnames(shiny_usage)), 1)
    expect_true("content_guid" %in% names(shiny_usage_local))
    expect_s3_class(shiny_usage_local$started, "POSIXct")
  }
})

test_that("content works", {
  scoped_experimental_silence()
  content_list <- tbl_connect(client, "content")
  expect_s3_class(content_list, c("tbl_connect", "tbl_lazy", "tbl"))

  content_list_local <- content_list %>% dplyr::collect()
  expect_s3_class(content_list_local, c("tbl_df", "tbl", "data.frame"))

  expect_true(is.na(nrow(content_list)))
  expect_type(colnames(content_list), "character")
  expect_gt(length(colnames(content_list)), 1)

  expect_true("guid" %in% names(content_list_local))
  expect_s3_class(content_list_local$created_time, "POSIXct")
})

test_that("groups works", {
  scoped_experimental_silence()
  groups_list <- tbl_connect(client, "groups")
  expect_s3_class(groups_list, c("tbl_connect", "tbl_lazy", "tbl"))

  groups_list_local <- groups_list %>% dplyr::collect()
  expect_s3_class(groups_list_local, c("tbl_df", "tbl", "data.frame"))

  expect_true(is.na(nrow(groups_list)))
  expect_type(colnames(groups_list), "character")
  expect_gt(length(colnames(groups_list)), 1)

  expect_true("guid" %in% names(groups_list_local))
  expect_true("name" %in% names(groups_list_local))
})

test_that("audit_logs works", {
  scoped_experimental_silence()
  audit_list <- tbl_connect(client, "audit_logs")
  expect_s3_class(audit_list, c("tbl_connect", "tbl_lazy", "tbl"))

  audit_list_local <- audit_list %>% dplyr::collect()
  expect_s3_class(audit_list_local, c("tbl_df", "tbl", "data.frame"))

  expect_true(is.na(nrow(audit_list)))
  expect_type(colnames(audit_list), "character")
  expect_gt(length(colnames(audit_list)), 1)

  # This is different on older versions, not sure it's worth worrying about how
  skip_if_connect_older_than(client, "2022.09.0")
  expect_true("id" %in% names(audit_list_local))
  expect_s3_class(audit_list_local$time, "POSIXct")
})
