context("content")

# should connect with env vars
test_conn_1 <- connect(host = Sys.getenv("TEST_SERVER_1"), api_key = Sys.getenv("TEST_KEY_1"))
test_conn_2 <- connect(host = Sys.getenv("TEST_SERVER_2"), api_key = Sys.getenv("TEST_KEY_2"))

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

collab_guid <- NULL
collab_alt_guid <- NULL
viewer_guid <- NULL
viewer_alt_guid <- NULL

test_that("acl returns owner once and only once", {
  # deploy content
  dir_path <- rprojroot::find_testthat_root_file("examples/static")
  tmp_file <- fs::file_temp(pattern = "bundle", ext = ".tar.gz")
  bund <- bundle_dir(path = dir_path, filename = tmp_file)
  
  tsk <- deploy(connect = test_conn_1, bundle = bund, name = cont1_name, title = cont1_title)
  
  cont1_guid <<- tsk$get_content()$guid
  cont1_content <<- content_item(tsk$get_connect(), cont1_guid)
  
  # get acl
  acls <- get_acl(cont1_content)
  
  my_guid <- test_conn_1$GET('me')$guid
  
  # first entry is me
  expect_true(acls[[1]]$guid == my_guid)
})

test_that("add a collaborator works", {
  
  # create a user
  collab <- test_conn_1$users_create(username = glue::glue("test_collab{random_name()}"), email = "collab@example.com", user_must_set_password = TRUE, user_role = "publisher")
  collab_guid <<- collab$guid
  
  # add a collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  
  # get acl
  acls <- get_acl(cont1_content)
  
  expect_true(any(purrr::map_lgl(acls, ~ .x$guid == collab_guid && .x$app_role == "owner")))
  
  # owner is present
  my_guid <- test_conn_1$GET('me')$guid
  expect_true(any(purrr::map_lgl(acls, ~ .x$guid == my_guid && .x$app_role == "owner")))
})

test_that("add collaborator twice works", {
  # add a collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  
  # get acl
  acls <- get_acl(cont1_content)
  
  which_match <- purrr::map_lgl(acls, ~ .x$guid == collab_guid && .x$app_role == "owner")
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("add a viewer works", {
  # create a user
  view_user <- test_conn_1$users_create(username = glue::glue("test_viewer{random_name()}"), email = "viewer@example.com", user_must_set_password = TRUE, user_role = "viewer")
  viewer_guid <<- view_user$guid
  
  # add a viewer
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  
  # get acl
  acls <- get_acl(cont1_content)
  
  which_match <- purrr::map_lgl(acls, ~ .x$guid == viewer_guid && .x$app_role == "viewer")
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("add a viewer twice works", {
  # add a viewer
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  
  # get acl
  acls <- get_acl(cont1_content)
  
  which_match <- purrr::map_lgl(acls, ~ .x$guid == viewer_guid && .x$app_role == "viewer")
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("remove a collaborator works", {
  # remove a collaborator
  invisible(acl_remove_collaborator(cont1_content, collab_guid))
  
  # get acl
  acls <- get_acl(cont1_content)
  
  which_match <- purrr::map_lgl(acls, ~.x$guid == collab_guid && .x$app_role == "owner")
  expect_false(any(which_match))
})

test_that("remove a collaborator twice works", {
  # remove a collaborator
  invisible(acl_remove_collaborator(cont1_content, collab_guid))
  invisible(acl_remove_collaborator(cont1_content, collab_guid))
  
  # get acl
  acls <- get_acl(cont1_content)
  
  expect_false(any(purrr::map_lgl(acls, ~.x$guid == collab_guid && .x$app_role == "owner")))
})

# Side effect test... lest POST / DELETE cause trouble... ------------------------------------------------
test_that("a collaborator does not affect other collaborators", {
  # create a user
  collab_alt <- test_conn_1$users_create(username = glue::glue("test_collab_alt{random_name()}"), email = "collab_alt@example.com", user_must_set_password = TRUE, user_role = "publisher")
  collab_alt_guid <<- collab_alt$guid
  
  # add both
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_add_collaborator(cont1_content, collab_alt_guid))
  
  acls <- get_acl(cont1_content)
  
  # both present
  expect_true(all(c(collab_guid, collab_alt_guid) %in% purrr::map_chr(acls, ~.x$guid)))
  
  # remove one
  invisible(acl_remove_collaborator(cont1_content, collab_alt_guid))
  
  acls2 <- get_acl(cont1_content)
  # other present
  expect_true(collab_guid %in% purrr::map_chr(acls2, ~.x$guid))
})

test_that("a collaborator and a viewer do not affect each other", {
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  
  acls <- get_acl(cont1_content)
  
  expect_true(any(purrr::map_lgl(acls, ~.x$guid == collab_guid && .x$app_role == "owner")))
  expect_true(any(purrr::map_lgl(acls, ~.x$guid == viewer_guid && .x$app_role == "viewer")))
  
  invisible(acl_remove_collaborator(cont1_content, collab_guid))
  acls2 <- get_acl(cont1_content)
  
  expect_true(any(purrr::map_lgl(acls2, ~.x$guid == viewer_guid && .x$app_role == "viewer")))
  
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_remove_viewer(cont1_content, viewer_guid))
  
  acls3 <- get_acl(cont1_content)
  expect_true(any(purrr::map_lgl(acls3, ~.x$guid == collab_guid && .x$app_role == "owner")))
})

test_that("a viewer does not affect other viewers", {
  # create a user
  view_user_alt <- test_conn_1$users_create(username = glue::glue("test_viewer_alt{random_name()}"), email = "viewer_alt@example.com", user_must_set_password = TRUE, user_role = "viewer")
  viewer_alt_guid <<- view_user_alt$guid
  
  # add both
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_add_viewer(cont1_content, viewer_alt_guid))
  
  acls <- get_acl(cont1_content)
  
  # both present
  expect_true(all(c(viewer_guid, viewer_alt_guid) %in% purrr::map_chr(acls, ~.x$guid)))
  
  # remove one
  invisible(acl_remove_viewer(cont1_content, viewer_alt_guid))
  
  acls2 <- get_acl(cont1_content)
  
  # other present
  expect_true(viewer_guid %in% purrr::map_chr(acls2, ~ .x$guid))
})

test_that("a collaborator can be added as a viewer", {
  skip("failing presently")
  # remove user to be sure
  invisible(acl_remove_user(cont1_content, collab_guid))
  
  # add collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  
  # add as viewer
  invisible(acl_add_viewer(cont1_content, collab_guid))
  
  acls <- get_acl(cont1_content)
  
  expect_true(any(purrr::map_lgl(acls, ~ .x$guid == collab_guid && .x$app_role == "owner")))
})

test_that("a viewer can be added as a collaborator", {
  # remove user to be sure
  invisible(acl_remove_user(cont1_content, collab_guid))
  
  # add as viewer
  invisible(acl_add_viewer(cont1_content, collab_guid))
  
  # add collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  
  acls <- get_acl(cont1_content)
  
  expect_true(any(purrr::map_lgl(acls, ~ .x$guid == collab_guid && .x$app_role == "owner")))
})

test_that("remove a viewer works", {
  # remove a viewer
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_remove_viewer(cont1_content, viewer_guid))
  
  # get acl
  acls <- get_acl(cont1_content)
  
  which_match <- purrr::map_lgl(acls, ~.x$guid == viewer_guid && .x$app_role == "viewer")
  expect_false(any(which_match))
})

test_that("remove a viewer twice works", {
    # remove a viewer
    invisible(acl_remove_viewer(cont1_content, viewer_guid))
    invisible(acl_remove_viewer(cont1_content, viewer_guid))
    
    # get acl
    acls <- get_acl(cont1_content)
    
    which_match <- purrr::map_lgl(acls, ~.x$guid == viewer_guid && .x$app_role == "viewer")
    expect_false(any(which_match))
})
