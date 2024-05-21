with_mock_api({
  test_that("we can retrieve the users list", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    users <- get_users(con)
    expect_s3_class(users, "data.frame")
    expect_equal(nrow(users), 3)
  })

  test_that("we can retrieve a user by id", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    a_user <- con$user("20a79ce3-6e87-4522-9faf-be24228800a4")
    # There is no User class?
    expect_type(a_user, "list")
    expect_equal(a_user$first_name, "Carlos")
  })
})

without_internet({
  test_that("Querying users by prefix", {
    client <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      client$users(prefix = "TEST"),
      "https://connect.example/__api__/v1/users?page_number=1&page_size=500&prefix=TEST"
    )

    # Now with spaces
    expect_GET(
      client$users(prefix = "A User Name", page_size = 5),
      "https://connect.example/__api__/v1/users?page_number=1&page_size=5&prefix=A%20User%20Name"
    )

    # Now with too big page size: it automatically truncates (maybe that's bad)
    expect_GET(
      client$users(page_size = 1000),
      "https://connect.example/__api__/v1/users?page_number=1&page_size=500"
    )
  })

  test_that("Querying remote users", {
    client <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      client$users_remote(prefix = "A user name"),
      "https://connect.example/__api__/v1/users/remote?prefix=A%20user%20name"
    )
  })
})
