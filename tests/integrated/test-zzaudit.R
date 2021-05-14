context("audits")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("basic behavior works", {
  all_apps <- cache_apps(test_conn_1)

  res <- audit_vanity_urls(all_apps, test_conn_1$host)
  expect_is(res, "character")

  # does not work today (because R version needed)
  # audit_r_versions(all_apps)

  runas <- audit_runas(all_apps)
  expect_is(runas, "data.frame")

  access_open <- audit_access_open(all_apps)
  expect_is(access_open, "character")

  r_versions <- suppressWarnings(audit_r_versions(all_apps))
  expect_is(r_versions, "gtable")
})
