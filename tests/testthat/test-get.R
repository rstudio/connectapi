test_that("get_runtimes() gets all runtimes if none specified", {
  client <- MockConnect$new("2024.10.0")

  client$mock_response(
    "GET",
    "v1/server_settings/r",
    content = list(
      installations = list(
        list(version = "4.3.1", cluster_name = "Local", image_name = "Local"),
        list(version = "4.4.0", cluster_name = "Local", image_name = "Local")
      )
    )
  )
  client$mock_response(
    "GET",
    "v1/server_settings/python",
    content = list(
      installations = list(
        list(version = "3.11.3", cluster_name = "Local", image_name = "Local"),
        list(version = "3.12.4", cluster_name = "Local", image_name = "Local")
      )
    )
  )
  client$mock_response(
    "GET",
    "v1/server_settings/quarto",
    content = list(
      installations = list(
        list(version = "1.4.557", cluster_name = "Local", image_name = "Local"),
        list(version = "1.5.55", cluster_name = "Local", image_name = "Local")
      )
    )
  )
  client$mock_response(
    "GET",
    "v1/server_settings/tensorflow",
    content = list(
      installations = list(
        list(version = "2.16.1", cluster_name = "Local", image_name = "Local")
      )
    )
  )

  expected <- tibble::as_tibble(list(
    runtime = c(
      "r",
      "r",
      "python",
      "python",
      "quarto",
      "quarto",
      "tensorflow"
    ),
    version = c(
      "4.3.1",
      "4.4.0",
      "3.11.3",
      "3.12.4",
      "1.4.557",
      "1.5.55",
      "2.16.1"
    ),
    cluster_name = c(
      "Local",
      "Local",
      "Local",
      "Local",
      "Local",
      "Local",
      "Local"
    ),
    image_name = c(
      "Local",
      "Local",
      "Local",
      "Local",
      "Local",
      "Local",
      "Local"
    )
  ))
  expect_identical(get_runtimes(client), expected)
  expect_identical(
    client$call_log,
    c(
      "GET https://connect.example/__api__/v1/server_settings/r",
      "GET https://connect.example/__api__/v1/server_settings/python",
      "GET https://connect.example/__api__/v1/server_settings/quarto",
      "GET https://connect.example/__api__/v1/server_settings/tensorflow"
    )
  )
})

test_that("get_runtimes() only specified runtimes", {
  client <- MockConnect$new("2024.10.0")

  client$mock_response(
    "GET",
    "v1/server_settings/python",
    content = list(
      installations = list(
        list(version = "3.11.3", cluster_name = "Local", image_name = "Local"),
        list(version = "3.12.4", cluster_name = "Local", image_name = "Local")
      )
    )
  )
  client$mock_response(
    "GET",
    "v1/server_settings/tensorflow",
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
  expect_identical(
    client$call_log,
    c(
      "GET https://connect.example/__api__/v1/server_settings/python",
      "GET https://connect.example/__api__/v1/server_settings/tensorflow"
    )
  )
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

test_that("get_vanity_urls() works", {
  with_mock_api({
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    expect_equal(
      get_vanity_urls(client),
      tibble::tibble(
        content_guid = c(
          "39c8d85a-37ae-4b8b-8655-30a06adff2f1",
          "93a3cd6d-5a1b-236c-9808-6045f2a73fb5"
        ),
        path = c(
          "/team-dashboard/",
          "/streamlit/my-app/"
        ),
        created_time = structure(
          c(
            1602623489,
            1677679943
          ),
          tzone = Sys.timezone(),
          class = c("POSIXct", "POSIXt")
        )
      )
    )
  })
})

test_that("get_packages() works as expected with current return value", {
  client <- MockConnect$new("2024.11.0")
  client$mock_response(
    "GET",
    "v1/packages",
    content = list(
      current_page = 1,
      total = 2,
      results = list(
        list(
          language = "python",
          language_version = "3.7.6",
          name = "absl-py",
          version = "0.12.0",
          hash = NULL,
          bundle_id = "9375",
          app_id = "4906",
          app_guid = "9bf33774"
        ),
        list(
          language = "python",
          language_version = "3.7.7",
          name = "absl-py",
          version = "0.8.1",
          hash = NULL,
          bundle_id = "6623",
          app_id = "3652",
          app_guid = "1935b6cb"
        )
      )
    )
  )
  expect_identical(
    get_packages(client),
    tibble::tibble(
      language = c("python", "python"),
      language_version = c("3.7.6", "3.7.7"),
      name = c("absl-py", "absl-py"),
      version = c("0.12.0", "0.8.1"),
      hash = c(NA, NA),
      bundle_id = c("9375", "6623"),
      content_id = c("4906", "3652"),
      content_guid = c("9bf33774", "1935b6cb")
    )
  )
})

test_that("Pagination is wired up correctly for packages method", {
  with_mock_api({
    without_internet({
      client <- Connect$new(
        server = "https://connect.example",
        api_key = "fake"
      )
      expect_GET(
        client$packages(name = "mypkg", page_number = 1),
        "https://connect.example/__api__/v1/packages?name=mypkg&page_number=1&page_size=100000"
      )
    })
  })
})

test_that("get_packages() works as expected with `content_guid` names in API response", {
  # The responses in this test differ from the httptest fixtures used by the
  # prior test by the presence of `content_id` and `content_guid` keys in the
  # return objects.
  client <- MockConnect$new("2024.11.0")

  client$mock_response(
    "GET",
    "v1/packages",
    content = list(
      current_page = 1,
      total = 2,
      results = list(
        list(
          language = "python",
          language_version = "3.7.6",
          name = "absl-py",
          version = "0.12.0",
          hash = NULL,
          bundle_id = "9375",
          app_id = "4906",
          app_guid = "9bf33774",
          content_id = "4906",
          content_guid = "9bf33774"
        ),
        list(
          language = "python",
          language_version = "3.7.7",
          name = "absl-py",
          version = "0.8.1",
          hash = NULL,
          bundle_id = "6623",
          app_id = "3652",
          app_guid = "1935b6cb",
          content_id = "3652",
          content_guid = "1935b6cb"
        )
      )
    )
  )

  expect_identical(
    get_packages(client),
    tibble::tibble(
      language = c("python", "python"),
      language_version = c("3.7.6", "3.7.7"),
      name = c("absl-py", "absl-py"),
      version = c("0.12.0", "0.8.1"),
      hash = c(NA, NA),
      bundle_id = c("9375", "6623"),
      content_id = c("4906", "3652"),
      content_guid = c("9bf33774", "1935b6cb")
    )
  )
})

test_that("get_content only requests vanity URLs for Connect 2024.06.0 and up", {
  with_mock_dir("2024.05.0", {
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    # `$version` is lazy, so we need to call it before `without_internet()`.
    client$version
  })
  without_internet({
    expect_GET(
      get_content(client),
      "http://connect.example/__api__/v1/content?include=tags%2Cowner"
    )
  })

  with_mock_dir("2024.06.0", {
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    # `$version` is lazy, so we need to call it before `without_internet()`.
    client$version
  })
  without_internet({
    expect_GET(
      get_content(client),
      "http://connect.example/__api__/v1/content?include=tags%2Cowner"
    )
  })

  with_mock_dir("2024.07.0", {
    client <- Connect$new(
      server = "http://connect.example",
      api_key = "not-a-key"
    )
    # `$version` is lazy, so we need to call it before `without_internet()`.
    client$version
  })
  without_internet({
    expect_GET(
      get_content(client),
      "http://connect.example/__api__/v1/content?include=tags%2Cowner%2Cvanity_url"
    )
  })
})

with_mock_dir("2026.03.0", {
  test_that("get_usage() returns usage data in the expected shape", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    usage <- get_usage(
      client,
      from = as.POSIXct("2025-04-01 00:00:01", tz = "UTC")
    )

    expect_s3_class(usage, "connect_list_hits")
    expect_true(is.list(usage))
    expect_equal(length(usage), 5)
    expect_equal(usage[[1]]$id, "8966707")
    expect_equal(usage[[1]]$content_guid, "475618c9")

    expect_length(usage, 5)

    # Check first element (raw list, before conversion to data.frame).
    expect_equal(
      usage[[1]],
      list(
        id = "8966707",
        user_guid = NULL,
        content_guid = "475618c9",
        timestamp = "2025-04-30T12:49:16.269904Z",
        data = list(
          path = "/hello",
          user_agent = "Datadog/Synthetics"
        )
      )
    )

    # Check conversion to data.frame (with unnesting)
    usage_df <- as.data.frame(usage)
    expect_equal(
      usage_df,
      data.frame(
        id = c("8966707", "8966708", "8967206", "8967210", "8966214"),
        user_guid = c(NA, NA, NA, NA, "fecbd383"),
        content_guid = c(
          "475618c9",
          "475618c9",
          "475618c9",
          "475618c9",
          "b0eaf295"
        ),
        timestamp = c(
          parse_connect_rfc3339(c(
            "2025-04-30T12:49:16.269904Z",
            "2025-04-30T12:49:17.002848Z",
            "2025-04-30T13:01:47.40738Z",
            "2025-04-30T13:04:13.176791Z",
            "2025-04-30T12:36:13.818466Z"
          ))
        ),
        path = c("/hello", "/world", "/chinchilla", "/lava-lamp", NA),
        user_agent = c(
          "Datadog/Synthetics",
          NA,
          "Datadog/Synthetics",
          "Datadog/Synthetics",
          NA
        )
      )
    )

    # Check conversion with unnest=FALSE
    usage_df_no_unnest <- as.data.frame(usage, unnest = FALSE)
    expect_equal(
      names(usage_df_no_unnest),
      c("id", "user_guid", "content_guid", "timestamp", "data")
    )
  })

  test_that("Metrics firehose is called with expected parameters", {
    client <- Connect$new(server = "https://connect.example", api_key = "fake")
    # $version is loaded lazily, we need it before calling get_usage()
    client$version

    without_internet({
      expect_GET(
        get_usage(client),
        "https://connect.example/__api__/v1/instrumentation/content/hits"
      )
      expect_GET(
        get_usage(
          client,
          from = as.POSIXct("2025-04-01 00:00:01", tz = "UTC"),
          to = as.POSIXct("2025-04-02 00:00:01", tz = "UTC")
        ),
        paste0(
          "https://connect.example/__api__/v1/instrumentation/content/hits?",
          "from=2025-04-01T00%3A00%3A01Z&to=2025-04-02T00%3A00%3A01Z"
        )
      )
    })
  })
})

# The hits `id` field is transitioning from integer to string across Connect
# versions. The fixture in 2025.04.0/ uses integer IDs (older API); the
# fixture in 2026.03.0/ uses string IDs (newer API).
test_that("get_usage() handles both integer and string ids", {
  with_mock_dir("2025.04.0", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    usage <- get_usage(
      client,
      from = as.POSIXct("2025-04-01 00:00:01", tz = "UTC")
    )
    usage_df <- as.data.frame(usage)
    expect_type(usage_df$id, "integer")
    expect_equal(usage_df$id, c(8966707L, 8966708L, 8967206L, 8967210L, 8966214L))
    expect_s3_class(usage_df$timestamp, "POSIXct")
  })

  with_mock_dir("2026.03.0", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    usage <- get_usage(
      client,
      from = as.POSIXct("2025-04-01 00:00:01", tz = "UTC")
    )
    usage_df <- as.data.frame(usage)
    expect_type(usage_df$id, "character")
    expect_equal(usage_df$id, c("8966707", "8966708", "8967206", "8967210", "8966214"))
    expect_s3_class(usage_df$timestamp, "POSIXct")
  })
})

with_mock_dir("2026.01.0", {
  test_that("content_guid is passed to the hits endpoint", {
    client <- Connect$new(server = "https://connect.example", api_key = "fake")
    # $version is loaded lazily, we need it before calling get_usage()
    client$version

    without_internet({
      expect_GET(
        get_usage(
          client,
          content_guid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
        ),
        paste0(
          "https://connect.example/__api__/v1/instrumentation/content/hits?",
          "content_guid=aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
        )
      )
      expect_GET(
        get_usage(
          client,
          content_guid = c(
            "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee",
            "ffffffff-1111-2222-3333-444444444444"
          )
        ),
        paste0(
          "https://connect.example/__api__/v1/instrumentation/content/hits?",
          "content_guid=aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee",
          "%7Cffffffff-1111-2222-3333-444444444444"
        )
      )
    })
  })
})
