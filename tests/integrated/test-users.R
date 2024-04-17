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

test_that("user_guid_from_username works", {
  expect_error(user_guid_from_username(test_conn_1, "this-user-prefix-does-not-exist"), "user not found")

  user_username <- create_random_name(20)
  user_res <- test_conn_1$users_create(user_username, "example@example.com", password = user_username)

  user_username_2 <- paste0(user_username, "X")
  user_res_2 <- test_conn_1$users_create(user_username_2, "example@example.com", password = user_username_2)

  expect_warning(user_guid_from_username(test_conn_1, substr(user_username, 0, 19)), "multiple users found")

  expect_equal(user_guid_from_username(test_conn_1, user_username), user_res$guid)
  expect_equal(user_guid_from_username(test_conn_1, user_username_2), user_res_2$guid)
})
