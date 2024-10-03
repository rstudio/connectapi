# Old unversioned API
print(.mockPaths())
with_mock_api({
  test_that("get_thumbnail() gets the thumbnail (unversioned)", {
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
  test_that("get_thumbnail() returns NA_character_ for 204 status codes (unversioned)", {
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
  test_that("get_thumbnail() errors with 404 status codes (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "23456789")
    expect_error(get_thumbnail(item))
  })
})

with_mock_api({
  test_that("has_thumbnail() returns TRUE when the item has a thumbnail (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "01234567")
    expect_true(has_thumbnail(item))
  })
})

with_mock_api({
  test_that("has_thumbnail() returns FALSE when the status code is 204 (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "12345678")
    expect_false(has_thumbnail(item))
  })
})

with_mock_api({
  test_that("has_thumbnail() errors with 404 status codes (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "23456789")
    expect_error(has_thumbnail(item))
  })
})

# set thumbnail()
# with a local file — 200 response
with_mock_api({
  test_that("set_thumbnail() returns returns a content item when successful (unversioned)", {
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
  test_that("set_thumbnail() raises an error when the endpoint returns a 404 (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "23456789")
    # TODO: What error?
    expect_error(set_thumbnail(item, "resources/smol.jpg"))
  })
})

# with a remote image — success
with_mock_api({
  test_that("set_thumbnail() works with remote images (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "01234567")
    received <- set_thumbnail(item, "https://other.server/working-image/image.png")
    # TODO: Assert expectations that endpoint was called?
    expect_true(validate_R6_class(received, "Content"))
    expect_identical(item, received)
  })
})

with_mock_api({
  test_that("set_thumbnail() returns an error when the remote image cannot be found (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "01234567")
    expect_error(set_thumbnail(item, "https://other.server/missing-image/image.png"))
    # TODO specify error
  })
})

# delete thumbnail()
with_mock_api({
  test_that("delete_thumbnail() returns the content item when delete works (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "01234567")
    expect_identical(delete_thumbnail(item), item)
  })
})
with_mock_api({
  test_that("delete_thumbnail() raises an error when delete fails (unversioned)", {
    client <- connect(server = "https://connect.example", api_key = "fake")
    item <- content_item(client, "23456789")
    expect_error(delete_thumbnail(item))
    # TODO specify error
  })
})


# v1 API (2024.09.0)
# Same as above but with different .mockPaths()
with_mock_dir("_mocks/2024.09.0", {
  with_mock_api({
    test_that("get_thumbnail() gets the thumbnail (v1)", {
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
    test_that("get_thumbnail() returns NA_character_ for 204 status codes (v1)", {
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
    test_that("get_thumbnail() errors with 404 status codes (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "23456789")
      expect_error(get_thumbnail(item))
    })
  })
  
  with_mock_api({
    test_that("has_thumbnail() returns TRUE when the item has a thumbnail (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "01234567")
      expect_true(has_thumbnail(item))
    })
  })
  
  with_mock_api({
    test_that("has_thumbnail() returns FALSE when the status code is 204 (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "12345678")
      expect_false(has_thumbnail(item))
    })
  })
  
  with_mock_api({
    test_that("has_thumbnail() errors with 404 status codes (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "23456789")
      expect_error(has_thumbnail(item))
    })
  })
  
  # set thumbnail()
  # with a local file — 200 response
  with_mock_api({
    test_that("set_thumbnail() returns returns a content item when successful (v1)", {
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
    test_that("set_thumbnail() raises an error when the endpoint returns a 404 (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "23456789")
      # TODO: What error?
      expect_error(set_thumbnail(item, "resources/smol.jpg"))
    })
  })
  
  # with a remote image — success
  with_mock_api({
    test_that("set_thumbnail() works with remote images (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "01234567")
      received <- set_thumbnail(item, "https://other.server/working-image/image.png")
      # TODO: Assert expectations that endpoint was called?
      expect_true(validate_R6_class(received, "Content"))
      expect_identical(item, received)
    })
  })
  
  with_mock_api({
    test_that("set_thumbnail() returns an error when the remote image cannot be found (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "01234567")
      expect_error(set_thumbnail(item, "https://other.server/missing-image/image.png"))
      # TODO specify error
    })
  })
  
  # delete thumbnail()
  with_mock_api({
    test_that("delete_thumbnail() returns the content item when delete works (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "01234567")
      expect_identical(delete_thumbnail(item), item)
    })
  })
  with_mock_api({
    test_that("delete_thumbnail() raises an error when delete fails (v1)", {
      client <- connect(server = "https://connect.example", api_key = "fake")
      item <- content_item(client, "23456789")
      expect_error(delete_thumbnail(item))
      # TODO specify error
    })
  })
})


