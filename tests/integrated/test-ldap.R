if (Sys.getenv("CONNECTAPI_LOCAL") == "TRUE") {
  admin <- connectapi:::create_first_admin(
    "http://localhost:60330",
    user = "john", password = "john",
    provider = "ldap"
  )
}

test_that("users_create_remote works with a simple case", {
  skip("tests are not automated")
  res <- connectapi::users_create_remote(admin, "julie")

  expect_s3_class(res, "tbl")
  expect_length(res[["guid"]], 1)
  expect_equal(res[["username"]], "julie")
})

test_that("users_create_remote works with a complex case", {
  skip("tests are not automated")
  res <- users_create_remote(admin, "bobo", expect = 2)
  # TODO: need a way to "exclude already existing users...?"

  expect_s3_class(res, "tbl")
  expect_length(res[["guid"]], 2)
  expect_true("bobo" %in% res[["username"]])
  expect_true("bobo2" %in% res[["username"]])
})

test_that("users_create_remote works with 'exact'", {
  skip("tests are not automated")
  res <- users_create_remote(admin, "joe", exact = TRUE)

  expect_s3_class(res, "tbl")
  expect_length(res[["guid"]], 1)
  expect_true("joe" %in% res[["username"]])

  res2 <- users_create_remote(admin, "joe2", exact = TRUE)
  expect_s3_class(res2, "tbl")
  expect_length(res2[["guid"]], 1)
  expect_true("joe2" %in% res2[["username"]])
})
