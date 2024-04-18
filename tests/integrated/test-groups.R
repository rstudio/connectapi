# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

user_guid <- NULL
group_guid <- NULL

test_that("groups_create works", {
  ss <- test_conn_1$server_settings()
  if (ss$authentication$name %in% c("LDAP")) {
    skip("not implemented for this authentication provider")
  }
  groupname <- create_random_name()
  res <- test_conn_1$groups_create(
    name = groupname
  )

  group_guid <<- res$guid

  expect_equal(res$name, groupname)
})

test_that("groups works", {
  groups <- test_conn_1$groups()

  expect_gt(length(groups$results), 0)
})

test_that("group_member_add works", {
  user_guid <<- test_conn_1$users_create(
    username = paste0("group_member", create_random_name()),
    email = "test@example.com",
    user_must_set_password = TRUE
  )$guid

  res <- test_conn_1$group_member_add(group_guid, user_guid)

  members <- test_conn_1$group_members(group_guid)

  expect_true(user_guid %in% purrr::map_chr(members$results, ~ .x$guid))
})

test_that("group_member_remove works", {
  res <- test_conn_1$group_member_remove(group_guid, user_guid)

  members <- test_conn_1$group_members(group_guid)

  expect_false(user_guid %in% purrr::map_chr(members$results, ~ .x$guid))
})
