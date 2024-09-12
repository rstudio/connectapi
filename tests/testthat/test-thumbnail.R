# Old unversioned API

# get thumbnail
test_that("get_thumbnail() gets the thumbnail", {
  client <- connect(server = "https://connect.example", api_key = "fake")
  item <- content_item(client, "f2ba0f64")
  image_path <- tempfile("thumbnail_", fileext = ".jpg")
  get_thumbnail(item, image_path)
})

# set thumbnail
# delete thumbnail


# v1 API (2024.09.0)

# get thumbnail
# has thumbnail with 200
# has thumbnail with 204
# set thumbnail
# delete thumbnail