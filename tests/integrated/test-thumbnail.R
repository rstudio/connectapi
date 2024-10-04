# The tests in this file were not written explicitly for the *_thumbnail()
# functions; they are taken from `tests/integrated/test-deploy.R` where they
# test the *_image.R functions.

test_that("set_thumbnail works with local images", {
  scoped_experimental_silence()
  img_path <- rprojroot::find_package_root_file("tests/testthat/examples/logo.png")

  res <- set_thumbnail(cont1_content, img_path)

  expect_true(validate_R6_class(res, "Content"))
})

test_that("get_thumbnail works", {
  scoped_experimental_silence()
  img_path <- rprojroot::find_package_root_file("tests/testthat/examples/logo.png")

  tmp_img <- fs::file_temp(pattern = "img", ext = ".png")
  get_thumbnail(cont1_content, tmp_img)

  expect_identical(
    readBin(img_path, "raw"),
    readBin(tmp_img, "raw")
  )

  # works again (i.e. does not append data)
  get_thumbnail(cont1_content, tmp_img)
  expect_identical(
    readBin(img_path, "raw"),
    readBin(tmp_img, "raw")
  )

  # works with no path
  auto_path <- get_thumbnail(cont1_content)
  expect_identical(
    readBin(img_path, "raw"),
    readBin(auto_path, "raw")
  )
  expect_identical(fs::path_ext(auto_path), "png")
})

test_that("has_thumbnail works with an image", {
  scoped_experimental_silence()

  expect_true(has_thumbnail(cont1_content))
})

test_that("delete_thumbnail works", {
  scoped_experimental_silence()
  # from above
  img_path <- rprojroot::find_package_root_file("tests/testthat/examples/logo.png")

  tmp_img <- fs::file_temp(pattern = "img", ext = ".png")
  # retains the image at the path
  expect_false(fs::file_exists(tmp_img))
  expect_true(validate_R6_class(delete_thumbnail(cont1_content, tmp_img), "Content"))
  expect_true(fs::file_exists(tmp_img))
  expect_identical(
    readBin(img_path, "raw"),
    readBin(tmp_img, "raw")
  )
  expect_false(has_thumbnail(cont1_content))

  # works again - i.e. if no image available
  expect_true(validate_R6_class(delete_thumbnail(cont1_content), "Content"))
})

test_that("has_thumbnail works with no image", {
  scoped_experimental_silence()

  expect_false(has_thumbnail(cont1_content))
})

test_that("get_thumbnail returns NA if no image", {
  scoped_experimental_silence()

  tmp_img <- fs::file_temp(pattern = "img", ext = ".png")
  response <- get_thumbnail(cont1_content, tmp_img)

  expect_false(identical(tmp_img, response))
  expect_true(is.na(response))
})

test_that("set_thumbnail works with remote paths", {
  scoped_experimental_silence()

  res <- set_thumbnail(cont1_content, glue::glue("{cont1_content$get_connect()$server}/connect/__favicon__"))

  expect_true(validate_R6_class(res, "Content"))

  # TODO: verify round-trip on the image is actually correct... SHA?
})
