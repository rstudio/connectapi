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

test_that("page_cursor binds pages when a field is null on page 1 but populated on page 2", {
  # First page: "note" is NULL in every row → parse_connectapi produces logical
  # NA column. Second page: "note" has character values → jsonlite produces a
  # character column. vctrs::vec_rbind must coerce logical NA → character.
  mock_client <- list(
    GET = function(url, ..., simplify = FALSE) {
      list(
        results = data.frame(
          id = 3:4,
          note = c("hello", "world"),
          stringsAsFactors = FALSE
        ),
        paging = list(`next` = NULL)
      )
    }
  )

  initial_response <- list(
    results = list(
      list(id = 1L, note = NULL),
      list(id = 2L, note = NULL)
    ),
    paging = list(`next` = "https://connect.example/__api__/v1/things?next=abc")
  )

  res <- page_cursor(mock_client, initial_response)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)
  expect_type(res$note, "character")
  expect_equal(res$note, c(NA, NA, "hello", "world"))
})

test_that("page_cursor binds pages with integer vs. double coercion", {
  # parse_connectapi sees integers on page 1; jsonlite may produce doubles on
  # page 2 (e.g. from a JSON number with a decimal). vctrs should widen
  # integer → double.
  mock_client <- list(
    GET = function(url, ..., simplify = FALSE) {
      list(
        results = data.frame(id = 3L, value = 3.14),
        paging = list(`next` = NULL)
      )
    }
  )

  initial_response <- list(
    results = list(
      list(id = 1L, value = 100L),
      list(id = 2L, value = 200L)
    ),
    paging = list(`next` = "https://connect.example/__api__/v1/things?next=abc")
  )

  res <- page_cursor(mock_client, initial_response)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$value, c(100, 200, 3.14))
})

test_that("page_cursor binds pages when a column is missing from page 2", {
  # Page 1 has an "extra" column; page 2 does not. vctrs::vec_rbind should fill
  # the missing column with NA.
  mock_client <- list(
    GET = function(url, ..., simplify = FALSE) {
      list(
        results = data.frame(id = 3:4, stringsAsFactors = FALSE),
        paging = list(`next` = NULL)
      )
    }
  )

  initial_response <- list(
    results = list(
      list(id = 1L, extra = "yes"),
      list(id = 2L, extra = "no")
    ),
    paging = list(`next` = "https://connect.example/__api__/v1/things?next=abc")
  )

  res <- page_cursor(mock_client, initial_response)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)
  expect_equal(res$extra, c("yes", "no", NA, NA))
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
