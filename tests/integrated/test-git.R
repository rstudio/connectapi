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
  cont1 <- deploy_repo(test_conn_1, "https://github.com/rstudio/connectapi", "main", "tests/testthat/examples/static", new_name, cont1_title)
  expect_true(validate_R6_class(cont1, "Content"))
  expect_true(validate_R6_class(cont1, "ContentTask"))

  deploy_again <- deploy_current(cont1)
  expect_true(validate_R6_class(deploy_again, "Content"))
  expect_true(validate_R6_class(deploy_again, "ContentTask"))
})

test_that("repo_check_account works", {
  scoped_experimental_silence()
  expect_message(
    acc <- repo_check_account(test_conn_1, "https://github.com"),
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

test_that("repo_check_branches_ref works", {
  scoped_experimental_silence()
  expect_message(
    expect_error(repo_check_branches_ref(test_conn_1, "https://github.com")),
    "not found"
  )

  br <- repo_check_branches_ref(test_conn_1, "https://github.com/rstudio/connectapi")
  expect_type(br, "character")
  expect_true("main" %in% names(br))
  expect_false("main" %in% br)
  expect_true(all(nchar(br) == 40))
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

test_that("deploy_repo_enable works", {
  scoped_experimental_silence()

  new_name <- uuid::UUIDgenerate()
  cont1 <- deploy_repo(test_conn_1, "https://github.com/rstudio/connectapi", "main", "tests/testthat/examples/static", new_name, "deploy_repo_enable test")
  expect_true(validate_R6_class(cont1, "Content"))
  expect_true(validate_R6_class(cont1, "ContentTask"))

  # TODO: flaky... how to be safer?
  Sys.sleep(5) # sleep for deployment...?
  expect_true(cont1$internal_content()$git$enabled)
  res <- deploy_repo_enable(cont1, FALSE)
  expect_false(cont1$internal_content()$git$enabled)
  res <- deploy_repo_enable(cont1, TRUE)
  expect_true(cont1$internal_content()$git$enabled)
})

test_that("deploy_repo_update works", {
  scoped_experimental_silence()

  # this is really hard to test... because we need a git repo that changes

  new_name <- uuid::UUIDgenerate()
  cont1 <- deploy_repo(test_conn_1, "https://github.com/rstudio/connectapi", "main", "tests/testthat/examples/static", new_name, "deploy_repo_update test - good")
  expect_true(validate_R6_class(cont1, "Content"))
  expect_true(validate_R6_class(cont1, "ContentTask"))

  res <- deploy_repo_update(cont1)
  expect_true(validate_R6_class(res, "Content"))

  wrong_branch <- uuid::UUIDgenerate()
  cont2 <- deploy_repo(test_conn_1, "https://github.com/rstudio/connectapi", "master-not-a-real-branch", "tests/testthat/examples/static", wrong_branch, "deploy_repo_update test - wrong branch")
  expect_true(validate_R6_class(cont2, "Content"))
  expect_true(validate_R6_class(cont2, "ContentTask"))

  res <- expect_error(deploy_repo_update(cont2), "master-not-a-real-branch")

  not_git_name <- uuid::UUIDgenerate()
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  cont3 <- deploy(test_conn_1, bnd, not_git_name)

  expect_error(deploy_repo_update(cont3), "not git-backed content")
})
