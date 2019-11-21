context("test group apis")

# should connect with env vars
test_conn_1 <- connect(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- connect(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

test_that("groups_create works", {
  ss <- test_conn_1$server_settings()
  if (ss$authentication$name %in% c("LDAP")) {
    skip("not implemented for this authentication provider")
  }
  groupname <- random_name()
  res <- test_conn_1$groups_create(
    name = groupname
    )
  
  expect_equal(res$name, groupname)
})

test_that("groups works", {
  groups <- test_conn_1$groups()
  
  expect_gt(length(groups$results), 0)
})
