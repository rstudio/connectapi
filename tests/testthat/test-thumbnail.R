mock_dirs <- c(
  "unversioned" = "2024.08.0",
  "v1" = "2024.09.0"
)

# The mocked content items are as follows:
# - 01234567:
#   - Returns 200 and a tiny jpeg when getting the thumbnail
#   - Returns 204 (success) when setting and deleting
# - 12345678: Returns 204 when getting the thumbnail (no thumbnail)
# - 23456789: Returns 404 for all endpoints
# - non-connect/missing-image/image.R: a 404 response

for (version in names(mock_dirs)) {
  with_mock_dir(mock_dirs[[version]], {
    with_mock_api({
      test_that(glue::glue("get_thumbnail() gets the thumbnail ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "01234567")
        
        # User-specified path
        user_path <- tempfile("thumbnail_", fileext = ".jpg")
        received_path <- get_thumbnail(item, user_path)
        received <- readBin(received_path, "raw", n = 8)
        expected <- as.raw(c(0x4e, 0x41))
        expect_equal(user_path, received_path)
        expect_equal(received, expected)
        
        # Automatic path
        received_path <- get_thumbnail(item)
        received <- readBin(received_path, "raw", n = 8)
        expected <- as.raw(c(0x4e, 0x41))
        expect_equal(substring(received_path, nchar(received_path) - 4), ".jpeg")
        expect_equal(received, expected)
        })
    
      test_that(glue::glue("get_thumbnail() returns NA_character_ for 204 status codes ({version})"), {
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
    
      test_that(glue::glue("get_thumbnail() errors with 404 status codes ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "23456789")
        expect_error(get_thumbnail(item), "request failed with Client error: \\(404\\) Not Found")
      })
    
      test_that(glue::glue("has_thumbnail() returns TRUE when the item has a thumbnail ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "01234567")
        expect_true(has_thumbnail(item))
      })
    
      test_that(glue::glue("has_thumbnail() returns FALSE when the status code is 204 ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "12345678")
        expect_false(has_thumbnail(item))
      })
    
      test_that(glue::glue("has_thumbnail() errors with 404 status codes ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "23456789")
        expect_error(has_thumbnail(item), "request failed with Client error: \\(404\\) Not Found")
      })
    
      test_that(glue::glue("set_thumbnail() returns returns a content item when successful ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "01234567")
        received <- set_thumbnail(item, "resources/smol.jpg")
        expect_true(validate_R6_class(received, "Content"))
        expect_identical(item, received)
      })
    
      test_that(glue::glue("set_thumbnail() raises an error when the endpoint returns a 404 ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "23456789")
        expect_error(set_thumbnail(item, "resources/smol.jpg"), "request failed with Client error: \\(404\\) Not Found")
      })
    
      test_that(glue::glue("set_thumbnail() works with remote images ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "01234567")
        expect_GET(set_thumbnail(item, "https://other.server/non-connect/working-image/not-an-image"),
                   "https://other.server/non-connect/working-image/not-an-image")
        # We're only asserting that this GETs.
        # We can't use a mock because of this issue:
        # https://github.com/nealrichardson/httptest/issues/86
      })
    
      test_that(glue::glue("set_thumbnail() returns an error when the remote image cannot be found ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "01234567")
        expect_error(set_thumbnail(item, "https://other.server/non-connect/missing-image/image.png"),
                     "Could not download image from https")
      })
    
      test_that(glue::glue("delete_thumbnail() returns the content item when delete works ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "01234567")
        expect_identical(delete_thumbnail(item), item)
      })
    
      test_that(glue::glue("delete_thumbnail() raises an error when delete fails ({version})"), {
        client <- connect(server = "https://connect.example", api_key = "fake")
        item <- content_item(client, "23456789")
        expect_error(delete_thumbnail(item), "request failed with Client error: \\(404\\) Not Found")
      })
    })
  })
}

