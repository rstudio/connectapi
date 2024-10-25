with_mock_api({
  con <- Connect$new(server = "https://connect.example", api_key = "fake")

  test_that("groups_create_remote: err when local and remote groups return no matches", {
    skip("not implemented")
  })
  
  test_that("groups_create_remote: err when local group exists and check is TRUE", {
    expect_message(
      res <- groups_create_remote(con, "Everyone"),
      "At least one group with name prefix 'Everyone' already exists"
    )
    expect_identical(res$name, "Everyone Else")
  })
    
  test_that("groups_create_remote: succeed when local group exists and check is FALSE", {
    expect_message(
      groups_create_remote(con, "Everyone", check = FALSE),
      "Creating remote group"
    )
  })

  test_that("groups_create_remote: succeed when local group exists and check is FALSE (v2)", {
    client <- MockConnect$new()

    client$mock_response(
      method = "GET",
      path = "v1/groups/remote",
      content = list(
        results = list(list(
          name = "Everyone",
          guid = NULL,
          temp_ticket = "fake"
        )), 
        current_page = 1L,
        total = 1L
      )
    )

    client$mock_response(
      method = "GET",
      path = "v1/groups",
      content = list(
        results = list(list(
          name = "Everyone Else"
        )),
        current_page = 1L, 
        total = 1L
      )
    )

    client$mock_response(
      method = "PUT",
      path = "v1/groups",
      content = list(
        guid = "1c1ab604-4a6a-4d07-9477-a88ac08386cd",
        name = "Everyone", 
        owner_guid = NULL
      )
    )

    expect_message(
      res <- groups_create_remote(client, "Everyone", check = FALSE),
      "Creating remote group"
    )
    expect_equal(res$name, "Everyone")
  })

  test_that("groups_create_remote: err if number of remote groups != `expect`", {
    skip("not implemented")
  })

  
  test_that("groups_create_remote: create group if one is found", {
    skip("not implemented")
  })
  
  
  test_that("groups_create_remote: only consider exact matches when exact is TRUE", {
    print(.mockPaths())
    expect_message(
      groups_create_remote(con, "Art", exact = TRUE),
      "Creating remote group"
    )
  })
})

# TODO: This test last, because it requires multiple sequential requests
test_that("groups_create_remote: create groups if multiple found and n == `expect`", {
  expect_error(
    groups_create_remote(con, "Art"),
    "The expected group\\(s\\) were not found"
  )


  client <- MockConnect$new()

  client$mock_response(
    method = "GET",
    path = "v1/groups/remote",
    content = list(
      results = list(
        list(
          name = "Everyone",
          guid = NULL,
          temp_ticket = "fake"
        ),
        list(
          name = "Everyone two",
          guid = NULL,
          temp_ticket = "fake"
        )
      ), 
      current_page = 1L,
      total = 1L
    )
  )

  client$mock_response(
    method = "GET",
    path = "v1/groups",
    content = list(
      results = list(list(
        name = "Everyone Else"
      )),
      current_page = 1L, 
      total = 1L
    )
  )

  client$mock_response(
    method = "PUT",
    path = "v1/groups",
    content = list(
      guid = "fake-guid-1",
      name = "Everyone", 
      owner_guid = NULL
    )
  )

  client$mock_response(
    method = "PUT",
    path = "v1/groups",
    content = list(
      guid = "fake-guid-2",
      name = "Everyone two", 
      owner_guid = NULL
    )
  )

  expect_message(
    res <- groups_create_remote(client, "Everyone", expect = 2, check = FALSE),
    "Creating remote group"
  )
  expect_equal(
    res,
    list(
      list(
        guid = "1c1ab604-4a6a-4d07-9477-a88ac08386cd",
        name = "Everyone", 
        owner_guid = NULL
      ),
      list(
        guid = "12345",
        name = "Everyone 2",
        owner_guid = NULL
      )
    )
  )
  expect_equal(res$name, "Everyone")
})