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

