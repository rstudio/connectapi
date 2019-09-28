context("test user apis")

# should connect with env vars
test_conn_1 <- connect(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- connect(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

test_that("users_create works", {
  ss <- test_conn_1$server_settings()
  if (ss$authentication$name %in% c("LDAP")) {
    skip("not implemented for this authentication provider")
  }
  username <- uuid::UUIDgenerate(use.time = TRUE)
  password <- uuid::UUIDgenerate(use.time = TRUE)
  res <- test_conn_1$users_create(
    username = username,
    first_name = "Test",
    last_name = "User",
    user_role = "publisher",
    email = "test@example.com",
    password = password
    )
  
  expect_equal(res$username, username)
})

test_that("users works", {
  users <- test_conn_1$users()
  
  expect_gt(length(users$results), 0)
})
