# Old unversioned API
with_mock_api({
  test_that("get_thumbnail() gets the thumbnail", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "01234567")
    
    # User-specified path
    user_path <- tempfile("thumbnail_", fileext = ".jpg")
    received_path <- get_thumbnail(item, user_path)
    received <- readBin(received_path, "raw", n = 8)
    expected <- as.raw(c(0x4e, 0x41))
    expect_equal(normalizePath(user_path), normalizePath(received_path))
    expect_equal(received, expected)
    
    # Automatic path
    received_path <- get_thumbnail(item)
    received <- readBin(received_path, "raw", n = 8)
    expected <- as.raw(c(0x4e, 0x41))
    expect_equal(substring(received_path, nchar(received_path) - 4), ".jpeg")
    expect_equal(received, expected)
    })
})

with_mock_api({
  test_that("get_thumbnail() returns NA_character_ for 204 status codes", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "12345678")
    
    # User-specified path
    user_path <- tempfile("thumbnail_", fileext = ".jpg")
    received_path <- get_thumbnail(item, user_path)
    expect_equal(received_path, NA_character_)
    
    # Automatic path
    received_path <- get_thumbnail(item)
    expect_equal(received_path, NA_character_)
  })
})

with_mock_api({
  test_that("get_thumbnail() errors with 404 status codes", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "23456789")
    expect_error(get_thumbnail(item))
  })
})

with_mock_api({
  test_that("has_thumbnail() returns TRUE when the item has a thumbnail", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "01234567")
    expect_true(has_thumbnail(item))
  })
})

with_mock_api({
  test_that("has_thumbnail() returns FALSE when the status code is 204", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "12345678")
    expect_false(has_thumbnail(item))
  })
})

with_mock_api({
  test_that("has_thumbnail() errors with 404 status codes", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "23456789")
    expect_error(has_thumbnail(item))
  })
})

# set thumbnail()
# with a local file — 200 response
with_mock_api({
  test_that("set_thumbnail() returns returns a content item when successful", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "01234567")
    received <- set_thumbnail(item, "resources/smol.jpg")
    # TODO: Assert expectations that endpoint was called?
    expect_true(validate_R6_class(received, "Content"))
    expect_identical(item, received)
  })
})

# with a local file — error
with_mock_api({
  test_that("set_thumbnail() raises an error when the endpoint returns a 404", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "23456789")
    # TODO: What error?
    expect_error(set_thumbnail(item, "resources/smol.jpg"))
  })
})

# with a remote image — success
# TODO: it's looking for resources/smol.jpg.json
with_mock_api({
  test_that("set_thumbnail() works with remote images", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "01234567")
    received <- set_thumbnail(item, "https://connect.example/resources/smol.jpg")
    # TODO: Assert expectations that endpoint was called?
    expect_true(validate_R6_class(received, "Content"))
    expect_identical(item, received)
  })
})
# with a remote image — fails because image cannot be retrieved

# delete thumbnail()


# v1 API (2024.09.0)
# Same as above but with different .mockPaths()


