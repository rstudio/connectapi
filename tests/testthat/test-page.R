with_mock_dir("2025.09.0", {
  client <- Connect$new(
    server = "https://connect.example",
    api_key = "not-a-key"
  )

  test_that("page_offset respects a specified 'limit' ", {
    # These mocks return four results across two pages.
    res <- search_content(client, q = "blobfish", limit = 3)
    expect_equal(length(res), 3)
    expect_equal(
      purrr::map_chr(res, list("content", "title")),
      c("blobfish dashboard", "blobfish api", "blobfish report")
    )
  })
})

test_that("page_cursor accumulates data frames from multiple pages", {
  # Simulate a two-page cursor-paginated response. The first page comes from
  # the caller with simplify=FALSE (list-of-lists), while page_cursor fetches
  # subsequent pages with simplify=TRUE (data frames).
  mock_client <- list(
    GET = function(url, ..., simplify = FALSE) {
      list(
        results = data.frame(id = 3:4, name = c("c", "d")),
        paging = list(`next` = NULL)
      )
    }
  )

  initial_response <- list(
    results = list(
      list(id = 1L, name = "a"),
      list(id = 2L, name = "b")
    ),
    paging = list(`next` = "https://connect.example/__api__/v1/things?next=abc")
  )

  res <- page_cursor(mock_client, initial_response)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)
  expect_equal(res$id, 1:4)
  expect_equal(res$name, c("a", "b", "c", "d"))
})

test_that("page_cursor returns empty list for empty single page", {
  mock_client <- list(GET = function(...) stop("should not be called"))

  initial_response <- list(
    results = list(),
    paging = list(`next` = NULL)
  )

  res <- page_cursor(mock_client, initial_response)
  expect_equal(length(res), 0)
})

test_that("page_cursor respects limit", {
  call_count <- 0L
  mock_client <- list(
    GET = function(url, ..., simplify = FALSE) {
      call_count <<- call_count + 1L
      list(
        results = data.frame(id = 3:4, name = c("c", "d")),
        paging = list(`next` = "https://connect.example/__api__/v1/things?next=more")
      )
    }
  )

  initial_response <- list(
    results = list(
      list(id = 1L, name = "a"),
      list(id = 2L, name = "b")
    ),
    paging = list(`next` = "https://connect.example/__api__/v1/things?next=abc")
  )

  res <- page_cursor(mock_client, initial_response, limit = 3)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  # Only fetched one additional page since we hit the limit
  expect_equal(call_count, 1L)
})
