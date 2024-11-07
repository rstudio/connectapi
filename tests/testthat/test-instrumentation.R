without_internet({

  visits_url <- function(querypath) {
    glue::glue("https://connect.example/__api__/v1/instrumentation/content/visits?{querypath}")
  }
  usage_url <- function(querypath) {
    glue::glue("https://connect.example/__api__/v1/instrumentation/shiny/usage?{querypath}")
  }

  test_that("Query params to inst_content_visits", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      con$inst_content_visits(),
      visits_url("limit=500&asc_order=true")
    )

    expect_GET(
      con$inst_content_visits(content_guid = "f2f37341-e21d-3d80-c698-a935ad614066"),
      visits_url("content_guid=f2f37341-e21d-3d80-c698-a935ad614066&limit=500&asc_order=true")
    )
    # Multiple GUIDs are | separated
    expect_GET(
      # Use short ids for readability
      con$inst_content_visits(content_guid = c("1234", "5678")),
      visits_url("content_guid=1234%7C5678&limit=500&asc_order=true")
    )

    expect_GET(
      con$inst_content_visits(asc_order = FALSE),
      visits_url("limit=500&asc_order=false")
    )

    expect_GET(
      con$inst_content_visits(limit = 10, previous = "asdf", nxt = "qwer"),
      visits_url("limit=10&previous=asdf&asc_order=true&next=qwer")
    )

    expect_GET(
      con$inst_content_visits(from = "2022-01-01", to = "2022-01-31"),
      visits_url("from=2022-01-01&to=2022-01-31&limit=500&asc_order=true")
    )

    expect_GET(
      con$inst_content_visits(min_data_version = 0),
      visits_url("min_data_version=0&limit=500&asc_order=true")
    )

    expect_GET(
      con$inst_content_visits(limit = 1000),
      visits_url("limit=500&asc_order=true")
    )
  })

  test_that("Query params to inst_shiny_usage", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      con$inst_shiny_usage(),
      usage_url("limit=500&asc_order=true")
    )

    expect_GET(
      con$inst_shiny_usage(content_guid = "f2f37341-e21d-3d80-c698-a935ad614066"),
      usage_url("content_guid=f2f37341-e21d-3d80-c698-a935ad614066&limit=500&asc_order=true")
    )
    # Multiple GUIDs are | separated
    expect_GET(
      # Use short ids for readability
      con$inst_shiny_usage(content_guid = c("1234", "5678")),
      usage_url("content_guid=1234%7C5678&limit=500&asc_order=true")
    )

    expect_GET(
      con$inst_shiny_usage(asc_order = FALSE),
      usage_url("limit=500&asc_order=false")
    )

    expect_GET(
      con$inst_shiny_usage(limit = 10, previous = "asdf", nxt = "qwer"),
      usage_url("limit=10&previous=asdf&asc_order=true&next=qwer")
    )

    expect_GET(
      con$inst_shiny_usage(from = "2022-01-01", to = "2022-01-31"),
      usage_url("from=2022-01-01&to=2022-01-31&limit=500&asc_order=true")
    )

    expect_GET(
      con$inst_shiny_usage(min_data_version = 0),
      usage_url("min_data_version=0&limit=500&asc_order=true")
    )

    expect_GET(
      con$inst_shiny_usage(limit = 1000),
      usage_url("limit=500&asc_order=true")
    )
  })
})
