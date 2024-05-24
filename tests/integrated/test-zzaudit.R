cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("basic behavior works", {
  all_apps <- cache_apps(test_conn_1)

  res <- audit_vanity_urls(all_apps, test_conn_1$server)
  expect_type(res, "character")
})
