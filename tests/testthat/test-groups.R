without_internet({
  test_that("Querying groups by prefix", {
    client <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      client$groups(prefix = "TEST"),
      "https://connect.example/__api__/v1/groups?page_number=1&page_size=500&prefix=TEST"
    )

    # Now with spaces
    expect_GET(
      client$groups(prefix = "A Group Name", page_size = 5),
      "https://connect.example/__api__/v1/groups?page_number=1&page_size=5&prefix=A%20Group%20Name"
    )

    # Now with too big page size: it automatically truncates (maybe that's bad)
    expect_GET(
      client$groups(page_size = 1000),
      "https://connect.example/__api__/v1/groups?page_number=1&page_size=500"
    )
  })

  test_that("Querying remote groups", {
    client <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      client$groups_remote(),
      "https://connect.example/__api__/v1/groups/remote?limit=500"
    )
    expect_GET(
      client$groups_remote(prefix = "A group name"),
      "https://connect.example/__api__/v1/groups/remote?limit=500&prefix=A%20group%20name"
    )
    expect_GET(
      client$groups_remote(limit = 1000),
      "https://connect.example/__api__/v1/groups/remote?limit=500"
    )
  })
})

with_mock_api({
  client <- Connect$new(server = "https://connect.example", api_key = "not-a-key")

  test_that("get_group_content() works", {
    group_guids <- c(
      "a6fb5cea",
      "ae5c3b2c"
    )

    expect_snapshot(get_group_content(client, group_guids))
  })

  test_that("get_group_content() returns an empty data frame when no content exists", {
    group_guid <- "a6fb5cff"

    expect_snapshot(get_group_content(client, group_guid))
  })
})
