cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("basic behavior works", {
  all_apps <- cache_apps(test_conn_1)

  res <- audit_vanity_urls(all_apps, test_conn_1$server)
  expect_type(res, "character")

  # does not work today (because R version needed)
  # audit_r_versions(all_apps)

  runas <- audit_runas(all_apps)
  expect_s3_class(runas, "data.frame")

  access_open <- audit_access_open(all_apps)
  expect_type(access_open, "character")

  r_versions <- suppressWarnings(audit_r_versions(all_apps))
  expect_s3_class(r_versions, "gtable")
})
