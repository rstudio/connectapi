test_that("verify_content_name works with valid names", {
  short_name <- "123"
  expect_equal(verify_content_name(short_name), short_name)

  long_name <- "abcdefghijklmnopqrstuvwxyabcdefghijklmnopqrstuvwxyabcdefghijklmn"
  expect_equal(verify_content_name(long_name), long_name)

  uuid <- uuid::UUIDgenerate()
  expect_equal(verify_content_name(uuid), uuid)
})

test_that("verify_content_name fails for invalid names", {
  expect_error(verify_content_name("a"))
  expect_error(verify_content_name(NA))
  # 65 characters
  expect_error(verify_content_name("abcdefghijklmnopqrstuvwxyabcdefghijklmnopqrstuvwxyabcdefghijklmno"))
  expect_error(verify_content_name(NULL))
  expect_error(verify_content_name("abc!@#$"))
  expect_error(verify_content_name("123 abc"))
})

test_that("create_random_name works with no length", {
  expect_is(create_random_name(), "character")
})

test_that("create_random_name works with length", {
  expect_equal(nchar(create_random_name(200)), 200)
  expect_equal(nchar(create_random_name(1)), 1)
})

with_mock_api({
  test_that("we can retrieve the content list", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_is(con, "Connect")
    content_list <- get_content(con)
    expect_is(content_list, "data.frame")
    expect_equal(nrow(content_list), 3)
  })
})
