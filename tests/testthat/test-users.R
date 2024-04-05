with_mock_api({
  test_that("we can retrieve the users list", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    users <- get_users(con)
    expect_is(users, "data.frame")
    expect_equal(nrow(users), 3)
  })

  test_that("we can retrieve a user by id", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    a_user <- con$user("20a79ce3-6e87-4522-9faf-be24228800a4")
    # There is no User class?
    expect_is(a_user, "list")
    expect_equal(a_user$first_name, "Carlos")
  })
})
