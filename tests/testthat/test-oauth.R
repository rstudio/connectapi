with_mock_api({
  test_that("we can retrieve the oauth user credentials", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_true(validate_R6_class(con, "Connect"))
    credentials <- get_oauth_credentials(con, user_session_token = "user-session-token")
    expect_equal(
      credentials,
      list(
        access_token = "user-access-token",
        issued_token_type = "urn:ietf:params:oauth:token-type:access_token",
        token_type = "Bearer"
      )
    )
  })

  test_that("we can retrieve the oauth content credentials", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_true(validate_R6_class(con, "Connect"))
    credentials <- get_oauth_content_credentials(con, content_session_token = "content-session-token")
    expect_equal(
      credentials,
      list(
        access_token = "content-access-token",
        issued_token_type = "urn:ietf:params:oauth:token-type:access_token",
        token_type = "Bearer"
      )
    )
  })
})
