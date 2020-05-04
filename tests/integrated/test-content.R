context("content")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

cont2_name <- uuid::UUIDgenerate()
cont2_title <- "Test Content 2"

collab_guid <- NULL
collab_alt_guid <- NULL
viewer_guid <- NULL
viewer_alt_guid <- NULL

collap_group_guid <- NULL
viewer_group_guid <- NULL

# deploy content
dir_path <- rprojroot::find_package_root_file("tests/testthat/examples/static")
tmp_file <- fs::file_temp(pattern = "bundle", ext = ".tar.gz")
bund <- bundle_dir(path = dir_path, filename = tmp_file)

tsk <- deploy(connect = test_conn_1, bundle = bund, name = cont1_name, title = cont1_title)

cont1_guid <- tsk$get_content()$guid
cont1_content <- content_item(tsk$get_connect(), cont1_guid)

tsk2 <- deploy(connect = test_conn_1, bundle = bund, name = cont2_name, title = cont2_title)

cont2_guid <- tsk2$get_content()$guid
cont2_content <- content_item(tsk2$get_connect(), cont2_guid)

test_that("content_item works", {
  cont1_tmp <- test_conn_1 %>% content_item(guid = cont1_guid)

  expect_true(validate_R6_class(cont1_tmp, "Content"))
  expect_equal(cont1_tmp$get_content()$guid, cont1_guid)
})

test_that("content_title works in a simple example", {
  test_title <- content_title(test_conn_1, cont1_guid)
  expect_identical(test_title, cont1_title)
})

test_that("content_title handles missing content gracefully", {
  null_title <- content_title(test_conn_1, "not_a_real_guid")
  expect_identical(null_title, "Unknown Content")

  null_title_custom <- content_title(test_conn_1, "not_a_real_guid", "other-default")
  expect_identical(null_title_custom, "other-default")
})

test_that("content_title handles NULL titles gracefully", {
  c2_name <- uuid::UUIDgenerate()
  c2 <- deploy(connect = test_conn_1, bundle = bund, name = c2_name, title = NA)
  expect_null(c2$get_content()$title)
  null_title <- content_title(test_conn_1, c2$get_content()$guid, "Test Title")
  expect_identical(null_title, "Test Title")
})

test_that("acl returns owner once and only once", {
  scoped_experimental_silence()

  # get acl
  acls <- get_acl_user(cont1_content)

  my_guid <- test_conn_1$GET("me")$guid

  # first entry is me
  expect_true(acls[1, ]$guid == my_guid)
})

test_that("add a collaborator works", {
  scoped_experimental_silence()

  # create a user
  collab <- test_conn_1$users_create(username = glue::glue("test_collab{random_name()}"), email = "collab@example.com", user_must_set_password = TRUE, user_role = "publisher")
  collab_guid <<- collab$guid

  # add a collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))

  expect_equal(acl_user_role(cont1_content, collab_guid), "owner")

  # owner is present
  my_guid <- test_conn_1$GET("me")$guid
  expect_equal(acl_user_role(cont1_content, my_guid), "owner")
})

test_that("add collaborator twice works", {
  scoped_experimental_silence()
  # add a collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_add_collaborator(cont1_content, collab_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("add a viewer works", {
  scoped_experimental_silence()
  # create a user
  view_user <- test_conn_1$users_create(username = glue::glue("test_viewer{random_name()}"), email = "viewer@example.com", user_must_set_password = TRUE, user_role = "viewer")
  viewer_guid <<- view_user$guid

  # add a viewer
  invisible(acl_add_viewer(cont1_content, viewer_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("add a viewer twice works", {
  scoped_experimental_silence()
  # add a viewer
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_add_viewer(cont1_content, viewer_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("remove a collaborator works", {
  scoped_experimental_silence()
  # remove a collaborator
  invisible(acl_remove_collaborator(cont1_content, collab_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_false(any(which_match))
})

test_that("remove a collaborator twice works", {
  scoped_experimental_silence()
  # remove a collaborator
  invisible(acl_remove_collaborator(cont1_content, collab_guid))
  invisible(acl_remove_collaborator(cont1_content, collab_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_false(any(which_match))
})

# Side effect test... lest POST / DELETE cause trouble... ------------------------------------------------
test_that("a collaborator does not affect other collaborators", {
  scoped_experimental_silence()
  # create a user
  collab_alt <- test_conn_1$users_create(username = glue::glue("test_collab_alt{random_name()}"), email = "collab_alt@example.com", user_must_set_password = TRUE, user_role = "publisher")
  collab_alt_guid <<- collab_alt$guid

  # add both
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_add_collaborator(cont1_content, collab_alt_guid))

  acls <- get_acl_user(cont1_content)

  # both present
  expect_true(all(c(collab_guid, collab_alt_guid) %in% acls$guid))

  # remove one
  invisible(acl_remove_collaborator(cont1_content, collab_alt_guid))

  acls2 <- get_acl_user(cont1_content)
  # other present
  expect_true(collab_guid %in% acls2$guid)
})

test_that("a collaborator and a viewer do not affect each other", {
  scoped_experimental_silence()
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_add_viewer(cont1_content, viewer_guid))

  acls <- get_acl_user(cont1_content)

  which_match_collab <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  which_match_viewer <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_true(any(which_match_collab))
  expect_true(any(which_match_viewer))

  invisible(acl_remove_collaborator(cont1_content, collab_guid))
  acls2 <- get_acl_user(cont1_content)

  which_match_viewer2 <- purrr::map2_lgl(acls2$guid, acls2$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_true(any(which_match_viewer2))

  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_remove_viewer(cont1_content, viewer_guid))

  acls3 <- get_acl_user(cont1_content)
  which_match_collab3 <- purrr::map2_lgl(acls3$guid, acls3$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_true(any(which_match_collab3))
})

test_that("a viewer does not affect other viewers", {
  scoped_experimental_silence()
  # create a user
  view_user_alt <- test_conn_1$users_create(username = glue::glue("test_viewer_alt{random_name()}"), email = "viewer_alt@example.com", user_must_set_password = TRUE, user_role = "viewer")
  viewer_alt_guid <<- view_user_alt$guid

  # add both
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_add_viewer(cont1_content, viewer_alt_guid))

  acls <- get_acl_user(cont1_content)

  # both present
  expect_true(all(c(viewer_guid, viewer_alt_guid) %in% acls$guid))

  # remove one
  invisible(acl_remove_viewer(cont1_content, viewer_alt_guid))

  acls2 <- get_acl_user(cont1_content)

  # other present
  expect_true(viewer_guid %in% acls2$guid)
})

test_that("a collaborator can be added as a viewer (overwrites)", {
  scoped_experimental_silence()
  # remove user to be sure
  invisible(acl_remove_user(cont1_content, collab_guid))

  # add collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))

  # add as viewer
  invisible(acl_add_viewer(cont1_content, collab_guid))

  acls <- get_acl_user(cont1_content)

  # TODO: Should this be a warning?
  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == collab_guid && .y == "viewer"
  })
  expect_true(any(which_match))
})

test_that("a viewer can be added as a collaborator", {
  scoped_experimental_silence()
  # remove user to be sure
  invisible(acl_remove_user(cont1_content, collab_guid))

  # add as viewer
  invisible(acl_add_viewer(cont1_content, collab_guid))

  # add collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))

  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_true(any(which_match))
})

test_that("remove a viewer works", {
  scoped_experimental_silence()
  # remove a viewer
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_remove_viewer(cont1_content, viewer_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_false(any(which_match))
})

test_that("remove a viewer twice works", {
  scoped_experimental_silence()
  # remove a viewer
  invisible(acl_remove_viewer(cont1_content, viewer_guid))
  invisible(acl_remove_viewer(cont1_content, viewer_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_false(any(which_match))
})

test_that("acl_user_role works", {
  scoped_experimental_silence()
  acl_remove_user(cont1_content, collab_guid)
  acl_remove_user(cont1_content, viewer_guid)

  acl_add_collaborator(cont1_content, collab_guid)
  expect_equal(acl_user_role(cont1_content, collab_guid), "owner")

  acl_add_viewer(cont1_content, viewer_guid)
  expect_equal(acl_user_role(cont1_content, viewer_guid), "viewer")
})


test_that("acl_user_role with null user_guid returns NULL", {
  scoped_experimental_silence()
  expect_null(acl_user_role(cont1_content, NULL))
})

test_that("acl_user_role with no role returns NULL", {
  scoped_experimental_silence()
  acl_remove_user(cont1_content, viewer_guid)
  expect_null(acl_user_role(cont1_content, viewer_guid))
})

test_that("acl_add_self works", {
  skip("not yet tested")
})

test_that("acl_remove_self works", {
  skip("not yet tested")
})

test_that("acl_add_group works", {
  scoped_experimental_silence()
  grp <- test_conn_1$groups_create(name = random_name())
  
  grpa <- acl_add_group(cont2_content, grp$guid, "owner")
  
  expect_is(grpa, "Content")
  
})
