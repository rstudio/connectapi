test_that("get_runtimes() gets all runtimes if none specified", {
  client <- MockConnect$new("2024.10.0")

  client$mock_response(
    "GET", "v1/server_settings/r",
    content = list(
      installations = list(
        list(version = "4.3.1", cluster_name = "Local", image_name = "Local"),
        list(version = "4.4.0", cluster_name = "Local", image_name = "Local")
      )
    )
  )
  client$mock_response(
    "GET", "v1/server_settings/python",
    content = list(
      installations = list(
        list(version = "3.11.3", cluster_name = "Local", image_name = "Local"),
        list(version = "3.12.4", cluster_name = "Local", image_name = "Local")
      )
    )
  )
  client$mock_response(
    "GET", "v1/server_settings/quarto",
    content = list(
      installations = list(
        list(version = "1.4.557", cluster_name = "Local", image_name = "Local"),
        list(version = "1.5.55", cluster_name = "Local", image_name = "Local")
      )
    )
  )
  client$mock_response(
    "GET", "v1/server_settings/tensorflow",
    content = list(
      installations = list(
        list(version = "2.16.1", cluster_name = "Local", image_name = "Local")
      )
    )
  )

  expected <- tibble::as_tibble(list(
    runtime = c(
      "r", "r", "python", "python", "quarto",
      "quarto", "tensorflow"
    ), version = c(
      "4.3.1", "4.4.0", "3.11.3",
      "3.12.4", "1.4.557", "1.5.55", "2.16.1"
    ), cluster_name = c(
      "Local",
      "Local", "Local", "Local", "Local", "Local", "Local"
    ), image_name = c(
      "Local",
      "Local", "Local", "Local", "Local", "Local", "Local"
    )
  ))
  expect_identical(get_runtimes(client), expected)
  expect_identical(client$call_log, c(
    "GET https://connect.example/__api__/v1/server_settings/r",
    "GET https://connect.example/__api__/v1/server_settings/python",
    "GET https://connect.example/__api__/v1/server_settings/quarto",
    "GET https://connect.example/__api__/v1/server_settings/tensorflow"
  ))
})

test_that("get_runtimes() only specified runtimes", {
  client <- MockConnect$new("2024.10.0")

  client$mock_response(
    "GET", "v1/server_settings/python",
    content = list(
      installations = list(
        list(version = "3.11.3", cluster_name = "Local", image_name = "Local"),
        list(version = "3.12.4", cluster_name = "Local", image_name = "Local")
      )
    )
  )
  client$mock_response(
    "GET", "v1/server_settings/tensorflow",
    content = list(
      installations = list(
        list(version = "2.16.1", cluster_name = "Local", image_name = "Local")
      )
    )
  )

  expected <- tibble::as_tibble(list(
    runtime = c("python", "python", "tensorflow"),
    version = c("3.11.3", "3.12.4", "2.16.1"),
    cluster_name = c("Local", "Local", "Local"),
    image_name = c("Local", "Local", "Local")
  ))
  expect_identical(get_runtimes(client, c("python", "tensorflow")), expected)
  expect_identical(client$call_log, c(
    "GET https://connect.example/__api__/v1/server_settings/python",
    "GET https://connect.example/__api__/v1/server_settings/tensorflow"
  ))
})

test_that("get_runtimes() restricts available runtimes based on Connect version.", {
  client <- MockConnect$new("2024.10.0")
  expect_error(
    get_runtimes(client, c("r", "python", "foofy")),
    '`runtimes` must be one of "r", "python", "quarto", "tensorflow"; received: "r", "python", "foofy".'
  )

  client <- MockConnect$new("2024.02.0")
  expect_error(
    get_runtimes(client, "tensorflow"),
    '`runtimes` must be one of "r", "python", "quarto"; received: "tensorflow".'
  )

  client <- MockConnect$new("1.8.3")
  expect_error(
    get_runtimes(client, c("r", "quarto")),
    '`runtimes` must be one of "r", "python"; received: "r", "quarto".'
  )
})

with_mock_api({
  client <- connect(server = "https://connect.example", api_key = "fake")
  test_that("get_groups() paginates with no prefix", {
    # To get this result, the code has to paginate through two API requests.
    # groups-4eaf46.json
    # groups-125d47.json

    result <- get_groups(client, page_size = 5, limit = 10)
    expected_names <- c(
      "~!@#$%^&*()_+", "1111", "2_viewer_group", "amanda_test_group",
      "a_new_group", "azurepipelines", "cgGroup01", "chris_test_group",
      "connect_dev", "cool_kids_of_the_dmv"
    )
    expect_identical(result$name, expected_names)
  })

  test_that("get_groups() does not paginate when called with a prefix", {
    # Only one response exists for this query; by succeeding this test verifies
    # that the pagination behavior is not engaged.
    # groups-deae1f.json

    result <- get_groups(client, page_size = 2, prefix = "c")
    expect_identical(result$name, c("connect_dev", "cool_kids_of_the_dmv"))
  })
})

without_internet({
  client <- Connect$new(server = "https://connect.example", api_key = "fake")
  test_that("get_users() works with user_role and account_status", {
    # No filter parameters specified
    expect_GET(
      get_users(client),
      "https://connect.example/__api__/v1/users?page_number=1&page_size=500"
    )

    # Filter just on one parameter
    expect_GET(
      get_users(client, user_role = "administrator"),
      "https://connect.example/__api__/v1/users?page_number=1&page_size=500&user_role=administrator"
    )

    # Filter on two parameters, one requiring concatenation
    expect_GET(
      get_users(
        client,
        user_role = c("administrator", "publisher"),
        account_status = "licensed"
      ),
      paste0(
        "https://connect.example/__api__/v1/users?page_number=1&page_size=500&",
        "user_role=administrator%7Cpublisher&account_status=licensed"
      )
    )
  })
})
