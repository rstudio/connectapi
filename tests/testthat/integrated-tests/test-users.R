context("test user apis")

# should connect with env vars
test_conn_1 <- Connect$new(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- Connect$new(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

test_that("users_create works", {
  skip("not yet implemented")
  test_conn_1$users_create("testuser", email = "test@test.blah", password = "somepassword")
})

test_that("users works", {
  users <- test_conn_1$users()
  
  expect_gt(length(users$results), 0)
})
