with_mock_api({
  con <- Connect$new(server = "https://connect.example", api_key = "fake")

  test_that("groups_create_remote errors if prefix returns zero matches", {
    skip("not implemented")
  })
  test_that("groups_create_remote errors if prefix returns more than one matches", {
    skip("not implemented")
  })
  test_that("groups_create_remote errors if expect is greater than one", {
    skip("not implemented")
  })
  test_that("groups_create_remote works if match already exists", {
    expect_message(
      groups_create_remote(con, "Everyone"),
      "Creating remote group"
    )
  })
  test_that("groups_create_remote creates match if one is found", {
    skip("not implemented")
  })
  test_that("groups_create_remote creates multiples if multiple are found", {
    expect_error(
      groups_create_remote(con, "Art"),
      "The expected group\\(s\\) were not found"
    )
  })
  test_that("groups_create_remote with exact = TRUE", {
    expect_message(
      groups_create_remote(con, "Art", exact = TRUE),
      "Creating remote group"
    )
  })
})
