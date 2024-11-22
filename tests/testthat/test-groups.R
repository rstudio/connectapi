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

test_that("extract_role() extracts the role for the named principal", {
  p_list <- list(
    list(
      list(
        principal_guid = "fake-guid-1",
        principal_name = "User1",
        principal_role = "author",
        principal_type = "user"
      ),
      list(
        principal_guid = "fake-target-guid",
        principal_name = "connect_dev",
        principal_role = "viewer",
        principal_type = "group"
      )
    ),
    list(
      list(
        principal_guid = "fake-guid-2",
        principal_name = "User2",
        principal_role = "author",
        principal_type = "user"
      ),
      list(
        principal_guid = "fake-target-guid",
        principal_name = "connect_dev",
        principal_role = "publisher",
        principal_type = "group"
      ),
      list(
        principal_guid = "fake-guid-3",
        principal_name = "toph",
        principal_role = "publisher",
        principal_type = "user"
      )
    )
  )
  expect_equal(
    purrr::map_chr(p_list, extract_role, principal_guid = "fake-target-guid"),
    c("viewer", "publisher")
  )
})

test_that("extract_role() errs when multiple entries exist for the same principal", {
  p_list <- list(
    list(
      principal_guid = "fake-guid-1",
      principal_name = "User1",
      principal_role = "author",
      principal_type = "user"
    ),
    list(
      principal_guid = "fake-target-guid",
      principal_name = "connect_dev",
      principal_role = "viewer",
      principal_type = "group"
    ),
    list(
      principal_guid = "fake-target-guid",
      principal_name = "connect_dev",
      principal_role = "publisher",
      principal_type = "group"
    )
  )
  expect_error(
    extract_role(p_list, principal_guid = "fake-target-guid"),
    "Unexpected permissions structure."
  )
})

with_mock_api({
  client <- Connect$new(server = "https://connect.example", api_key = "not-a-key")

  test_that("get_group_content() successfully gets the content for multiple groups", {
    groups_df <- tibble::tibble(
      guid = c(
        "a6fb5cea",
        "ae5c3b2c"
      ),
      name = c(
        "connect_dev",
        "group12"
      ),
      owner_guid = c(
        "1a7a5703",
        "434f97ab"
      )
    )

    expect_snapshot(get_group_content(client, groups_df))
  })
})
