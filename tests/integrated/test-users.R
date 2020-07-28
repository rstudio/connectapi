context("test user apis")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

test_that("users_create works", {
  ss <- test_conn_1$server_settings()
  if (ss$authentication$name %in% c("LDAP")) {
    skip("not implemented for this authentication provider")
  }
  username <- create_random_name()
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
