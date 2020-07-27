context("verify_content_name")

test_that("works with valid names", {

  short_name <- "123"
  expect_equal(verify_content_name(short_name), short_name)
  long_name <- "abcdefghijklmnopqrstuvwxyabcdefghijklmnopqrstuvwxyabcdefghijklmn"
  expect_equal(verify_content_name(long_name), long_name)
})

test_that("fails for invalid names", {
  expect_error(verify_content_name("a"))
  expect_error(verify_content_name(NA))
  # 65 characters
  expect_error(verify_content_name("abcdefghijklmnopqrstuvwxyabcdefghijklmnopqrstuvwxyabcdefghijklmno"))
  expect_error(verify_content_name(NULL))
  expect_error(verify_content_name("abc!@#$"))
  expect_error(verify_content_name("123 abc"))
})

test_that("works with no length", {
  expect_is(create_random_name(), "character")
})

test_that("works with length", {
  expect_equal(nchar(create_random_name(200)), 200)
  expect_equal(nchar(create_random_name(1)), 1)
})
