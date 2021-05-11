context("git")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL

test_that("git deployment works", {
  cont0 <- deploy_repo(test_conn_1, "https://github.com/colearendt/shiny-shell", "master", ".")
  expect_true(validate_R6_class(cont0, "Content"))
  expect_true(validate_R6_class(cont0, "Task"))

  cont1 <- deploy_repo(test_conn_1, "https://github.com/colearendt/shiny-shell", "master", ".", cont1_name, cont1_title)
  expect_true(validate_R6_class(cont1, "Content"))
  expect_true(validate_R6_class(cont1, "Task"))

  deploy_again <- deploy_current(cont1)
  expect_true(validate_R6_class(deploy_again, "Content"))
  expect_true(validate_R6_class(deploy_again, "Task"))
})

