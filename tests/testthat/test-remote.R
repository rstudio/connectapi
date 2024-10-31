
  
test_that("groups_create_remote: succeed when no local group exists", {
  client <- MockConnect$new()
  client$mock_response(
    method = "GET",
    path = "v1/groups",
    content = list(
      results = list(),
      current_page = 1L, 
      total = 1L
    )
  )
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
    method = "PUT",
    path = "v1/groups",
    content = list(
      guid = "1c1ab604-4a6a-4d07-9477-a88ac08386cd",
      name = "Everyone", 
      owner_guid = NULL
    )
  )
  client$mock_response(
    method = "GET",
    path = "v1/groups",
    content = list(
      results = list(
        list(
          guid = "1c1ab604-4a6a-4d07-9477-a88ac08386cd",
          name = "Everyone", 
          owner_guid = NULL
        )
      ),
      current_page = 1L, 
      total = 1L
    )
  )

  
  expect_message(
    res <- groups_create_remote(client, "Everyone"),
    "Creating remote group"
  )
  expect_equal(res$name, "Everyone")
  expect_equal(
    client$call_log,
    c(
      "GET https://connect.example/__api__/v1/groups",
      "GET https://connect.example/__api__/v1/groups/remote",
      "PUT https://connect.example/__api__/v1/groups",
      "GET https://connect.example/__api__/v1/groups"
    )
  )
})

test_that("groups_create_remote: succeed without checking local groups if check is FALSE", {
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
    method = "PUT",
    path = "v1/groups",
    content = list(
      guid = "1c1ab604-4a6a-4d07-9477-a88ac08386cd",
      name = "Everyone", 
      owner_guid = NULL
    )
  )
  client$mock_response(
    method = "GET",
    path = "v1/groups",
    content = list(
      results = list(
        list(
          guid = "1c1ab604-4a6a-4d07-9477-a88ac08386cd",
          name = "Everyone", 
          owner_guid = NULL
        )
      ),
      current_page = 1L, 
      total = 1L
    )
  )

  expect_message(
    res <- groups_create_remote(client, "Everyone", check = FALSE),
    "Creating remote group"
  )
  expect_equal(res$name, "Everyone")
  expect_equal(
    client$call_log,
    c(
      "GET https://connect.example/__api__/v1/groups/remote",
      "PUT https://connect.example/__api__/v1/groups",
      "GET https://connect.example/__api__/v1/groups"
      )
  )
})

test_that("groups_create_remote: err if number of remote groups != `expect`", {
  client <- MockConnect$new()
  client$mock_response(
    method = "GET",
    path = "v1/groups",
    content = list(
      results = list(),
      current_page = 1L, 
      total = 1L
    )
  )
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
  
  expect_error(
    res <- groups_create_remote(client, "Everyone", expect = 2),
    "The expected group\\(s\\) were not found. Please specify a more accurate 'prefix'"
  )
  expect_equal(
    client$call_log,
    c("GET https://connect.example/__api__/v1/groups",
      "GET https://connect.example/__api__/v1/groups/remote"
    )
  )
})

test_that("groups_create_remote: create groups if multiple found and n == `expect`", {
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
      results = list(),
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
  client$mock_response(
    method = "GET",
    path = "v1/groups",
    content = list(
      results = list(
        list(
          guid = "fake-guid-1",
          name = "Everyone", 
          owner_guid = NULL
        ),
        list(
          guid = "fake-guid-2",
          name = "Everyone two", 
          owner_guid = NULL
        )
      ),
      current_page = 1L, 
      total = 1L
    )
  )

  expect_message(
    res <- groups_create_remote(client, "Everyone", expect = 2),
    "Creating remote group"
  )
  expect_identical(
    res$name,
    c("Everyone", "Everyone two")
  )
  expect_identical(
    client$call_log,
    c(
      "GET https://connect.example/__api__/v1/groups",
      "GET https://connect.example/__api__/v1/groups/remote",
      "PUT https://connect.example/__api__/v1/groups",
      "PUT https://connect.example/__api__/v1/groups",
      "GET https://connect.example/__api__/v1/groups"
    )
  )
})

with_mock_api({
  mock_dir_client <- Connect$new(server = "https://connect.example", api_key = "fake")

  test_that("groups_create_remote: err when local and remote groups return no matches", {
    expect_error(
      res <- groups_create_remote(mock_dir_client, "Nothing"),
      "The expected group\\(s\\) were not found. Please specify a more accurate 'prefix'"
    )
  })
 
  test_that("groups_create_remote: message when local group exists and check is TRUE", {
    expect_message(
      res <- groups_create_remote(mock_dir_client, "Everyone"),
      "At least one group with name prefix 'Everyone' already exists"
    )
    expect_identical(res$name, "Everyone Else")
  })

  test_that("groups_create_remote: message when local group exists and check is TRUE", {
    expect_message(
      res <- groups_create_remote(mock_dir_client, "Everyone"),
      "At least one group with name prefix 'Everyone' already exists"
    )
    expect_identical(res$name, "Everyone Else")
  })
  
  test_that("groups_create_remote: only consider exact matches when exact is TRUE", {

    expect_message(
      groups_create_remote(mock_dir_client, "Art", exact = TRUE),
      "Creating remote group"
    )
  })

  expect_error(
    groups_create_remote(mock_dir_client, "Art"),
    "The expected group\\(s\\) were not found"
  )
})
