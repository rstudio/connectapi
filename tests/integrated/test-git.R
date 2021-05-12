context("git")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("git deployment works", {
  scoped_experimental_silence()
  cont0 <- deploy_repo(test_conn_1, "https://github.com/rstudio/connectapi", "main", "tests/testthat/examples/static")
  expect_true(validate_R6_class(cont0, "Content"))
  expect_true(validate_R6_class(cont0, "ContentTask"))

  new_name <- uuid::UUIDgenerate()
  cont1 <- deploy_repo(test_conn_1, "https://github.com/colearendt/connectapi", "main", "tests/testthat/examples/static", new_name, cont1_title)
  expect_true(validate_R6_class(cont1, "Content"))
  expect_true(validate_R6_class(cont1, "ContentTask"))

  deploy_again <- deploy_current(cont1)
  expect_true(validate_R6_class(deploy_again, "Content"))
  expect_true(validate_R6_class(deploy_again, "ContentTask"))
})

test_that("repo_check_account works", {
  scoped_experimental_silence()
  acc <- expect_message(
    repo_check_account(test_conn_1, "https://github.com"),
    "anonymous"
  )

  expect_true(nchar(acc$username) == 0)
})

test_that("repo_check_branches works", {
  scoped_experimental_silence()
  expect_message(
    expect_error(repo_check_branches(test_conn_1, "https://github.com")),
    "not found"
  )

  br <- repo_check_branches(test_conn_1, "https://github.com/rstudio/connectapi")
  expect_true("main" %in% br)
})

test_that("repo_check_manifest_dirs works", {
  scoped_experimental_silence()
  expect_message(
    expect_error(repo_check_manifest_dirs(test_conn_1, "https://github.com", "main")),
    "not found"
  )

  drs <- repo_check_manifest_dirs(test_conn_1, "https://github.com/rstudio/connectapi", "main")
  expect_true("tests/testthat/examples/static" %in% drs)
})
