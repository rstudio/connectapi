context("content")


# Setup ----------------------------------------------------

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

# Setup - "REAL" content ------------------------------------------------
# ensure that RSPM is being used so this does not take eternity

shiny_name <- uuid::UUIDgenerate()
shiny_title <- "Test Shiny 1"
shiny_guid <- NULL
shiny_content <- NULL

rmd_name <- uuid::UUIDgenerate()
rmd_title <- "Test RMarkdown 1"
rmd_guid <- NULL
rmd_content <- NULL

prmd_name <- uuid::UUIDgenerate()
prmd_title <- "Test Param RMarkdown 1"
prmd_guid <- NULL
prmd_content <- NULL

# Setup - Shiny -------------------------------------------------------

dir_shiny <- rprojroot::find_package_root_file("tests/testthat/examples/shiny")
tmp_file_shiny <- fs::file_temp(pattern = "bundle_shiny", ext = ".tar.gz")
bund_shiny <- bundle_dir(path = dir_shiny, filename = tmp_file_shiny)

tsk_shiny <- deploy(connect = test_conn_1, bundle = bund_shiny, name = shiny_name, title = shiny_title)

shiny_guid <- tsk_shiny$get_content()$guid
shiny_content <- content_item(tsk_shiny$get_connect(), shiny_guid)

# TODO: a smarter, noninteractive wait...
shiny_wait <- suppressMessages(poll_task(tsk_shiny))

# Setup - RMarkdown -------------------------------------------------------

dir_rmd <- rprojroot::find_package_root_file("tests/testthat/examples/rmarkdown")
tmp_file_rmd <- fs::file_temp(pattern = "bundle_rmd", ext = ".tar.gz")
bund_rmd <- bundle_dir(path = dir_rmd, filename = tmp_file_rmd)

tsk_rmd <- deploy(connect = test_conn_1, bundle = bund_rmd, name = rmd_name, title = rmd_title)

rmd_guid <- tsk_rmd$get_content()$guid
rmd_content <- content_item(tsk_rmd$get_connect(), rmd_guid)

# TODO: a smarter, noninteractive wait...
rmd_wait <- suppressMessages(poll_task(tsk_rmd))

# Setup - Param RMarkdown -------------------------------------------------------

dir_prmd <- rprojroot::find_package_root_file("tests/testthat/examples/param_rmarkdown")
tmp_file_prmd <- fs::file_temp(pattern = "bundle_prmd", ext = ".tar.gz")
bund_prmd <- bundle_dir(path = dir_prmd, filename = tmp_file_prmd)

tsk_prmd <- deploy(connect = test_conn_1, bundle = bund_prmd, name = prmd_name, title = prmd_title)

prmd_guid <- tsk_prmd$get_content()$guid
prmd_content <- content_item(tsk_prmd$get_connect(), prmd_guid)

# TODO: a smarter, noninteractive wait
prmd_wait <- suppressMessages(poll_task(tsk_prmd))

# Metadata Tests ----------------------------------------------------

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

test_that("update owner_guid works", {
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  myc <- deploy(test_conn_1, bnd)

  new_user <- test_conn_1$users_create(
    username = glue::glue("test_admin_{create_random_name()}"),
    email = "example@example.com",
    user_role = "administrator",
    user_must_set_password = TRUE
  )

  new_owner_guid <- myc$update(owner_guid = new_user$guid)

  expect_equal(
    myc$get_content_remote()$owner_guid,
    new_user$guid
  )
})

# Environment ------------------------------------

test_that("get_environment works with no environment variables", {
  env <- get_environment(rmd_content)
  curr_vers <- env$env_version
  env <- get_environment(rmd_content)

  expect_identical(env$env_vars, list())
})

test_that("set_environment works", {
  scoped_experimental_silence()
  env <- get_environment(rmd_content)

  new_env <- set_environment_new(env, test = "value", test1 = TRUE, test2 = 4567)

  expect_equal(
    new_env$env_vars,
    list(
      "test", "test1", "test2"
      )
    )

  new_env1 <- set_environment_new(env, test1 = "another")
  expect_equal(
    new_env$env_vars,
    list(
      "test", "test1", "test2"
    )
  )

  # remove a value multiple times
  rm1 <- set_environment_remove(env, test)
  expect_equal(rm1$env_vars, list("test1", "test2"))
  rm2 <- set_environment_remove(env, test)
  expect_equal(rm1$env_vars, list("test1", "test2"))

  rm3 <- set_environment_remove(env, "test1")
  expect_equal(rm3$env_vars, list("test2"))

  rm4 <- set_environment_remove(env, "test2")
  expect_equal(rm4$env_vars, list())
})

test_that("add environment variable works", {
  res <- set_environment_new(rmd_content, MYVAR="hi")
  expect_true("MYVAR" %in% res$env_vars)
  expect_true(validate_R6_class(res, "Environment"))
})

test_that("edit environment variable works", {
  res <- set_environment_new(rmd_content, MYVAR="hi", OTHER="other")
  res2 <- set_environment_new(rmd_content, MYVAR="hi2")
  expect_true("MYVAR" %in% res2$env_vars)
  expect_true("OTHER" %in% res2$env_vars)
  expect_true(validate_R6_class(res2, "Environment"))
})

test_that("get environment works", {
  res <- set_environment_new(rmd_content, MYVAR="hi")
  res <- get_environment(rmd_content)
  expect_true("MYVAR" %in% res$env_vars)
  expect_true(validate_R6_class(res, "Environment"))
})

test_that("remove environment variable works", {
  res <- set_environment_new(rmd_content, MYVAR="hi", ANOTHER="how")
  rem <- set_environment_new(rmd_content, MYVAR=NA)

  expect_false("MYVAR" %in% rem$env_vars)
  expect_true("ANOTHER" %in% rem$env_vars)
  expect_true(validate_R6_class(res, "Environment"))
})

test_that("set all environment variables works", {
  res <- set_environment_all(rmd_content)
  expect_true(length(res$env_vars) == 0)
  expect_true(validate_R6_class(res, "Environment"))

  res <- set_environment_all(rmd_content, "MYVAR" = "ONE", "ANOTHER" = "HELLO")
  expect_equal(res$env_vars, list("ANOTHER", "MYVAR"))
  expect_true(validate_R6_class(res, "Environment"))

  res <- set_environment_all(rmd_content, "MYVAR" = "TWO", "HELLO" = "AGAIN")
  expect_equal(res$env_vars, list("HELLO", "MYVAR"))
  expect_true(validate_R6_class(res, "Environment"))

  res <- set_environment_all(rmd_content)
  expect_true(length(res$env_vars) == 0)
  expect_true(validate_R6_class(res, "Environment"))
})

# Bundles ---------------------------------------------------

test_that("get_bundles and delete_bundle work", {
  bnd_name <- create_random_name()

  bc1 <- deploy(test_conn_1, bund, bnd_name)
  bc1 <- deploy(test_conn_1, bund, bnd_name)
  bc1 <- deploy(test_conn_1, bund, bnd_name)

  bnd_dat <- get_bundles(bc1)
  expect_equal(nrow(bnd_dat), 3)
  expect_is(bnd_dat, "tbl_df")

  not_active_bundles <- bnd_dat[!bnd_dat$active,]

  bnd_del <- delete_bundle(test_conn_1, not_active_bundles[["id"]][[1]])
  expect_true(validate_R6_class(bnd_del, "Connect"))

  bnd_dat2 <- get_bundles(bc1)
  expect_equal(nrow(bnd_dat2), 2)
})

# Execution ----------------------------------------------------
#
# i.e. deploying real content...
#
context("render")

# TODO: very hard to test parameterized rmarkdown because creating a
# programmatic variant is not possible

test_that("get_variants works", {
  scoped_experimental_silence()
  vrs <- get_variants(rmd_content)

  expect_equal(nrow(vrs), 1)

  vr <- get_variant(rmd_content, vrs$key)
  expect_true(validate_R6_class(vr, "Variant"))
})

test_that("variant_render works", {
  scoped_experimental_silence()
  vr <- get_variant_default(rmd_content)

  rnd <- variant_render(vr)
  rnd2 <- variant_render(vr)

  expect_true(validate_R6_class(rnd, "VariantTask"))
  # TODO: would be great to be able to "tail the logs", for instance
  # i.e. actually reference the "job" itself...

  # wait for tasks to complete...
  suppressMessages(poll_task(rnd))
  suppressMessages(poll_task(rnd2))
})

test_that("get_variant_renderings works", {
  scoped_experimental_silence()

  vr <- get_variant_default(rmd_content)

  rnd <- get_variant_renderings(vr)

  expect_gt(nrow(rnd), 1)
})

test_that("get_jobs works", {
  scoped_experimental_silence()
  vr <- get_variant_default(rmd_content)

  all_jobs <- get_jobs(vr)
  expect_gt(nrow(all_jobs), 1)

  sel_key <- all_jobs$key[[1]]
  one_job <- get_job(vr, sel_key)
  expect_equal(nrow(one_job), 1)
  expect_equal(one_job$key[[1]], sel_key)
})

context("set_run_as")

test_that("fails for static content", {
  scoped_experimental_silence()
  expect_error(
    suppressMessages(set_run_as(cont1_content, "rstudio-connect")),
    "400"
  )
})

test_that("works with a good linux user", {
  scoped_experimental_silence()
  res <- set_run_as(shiny_content, "rstudio-connect")
  expect_equal(
    res$get_content()$run_as,
    "rstudio-connect"
  )
  res2 <- set_run_as(shiny_content, NULL)
  expect_null(res2$get_content()$run_as)
})

test_that("fails with a bad linux user", {
  scoped_experimental_silence()
  expect_error(
    suppressMessages(
      set_run_as(shiny_content, "fake-user")
    ),
    "400"
  )
})

test_that("works for run_as_current_user", {
  scoped_experimental_silence()
  res <- set_run_as(
    shiny_content,
    run_as = NULL,
    run_as_current_user = TRUE
  )

  expect_true(
    shiny_content$get_content()$run_as_current_user
  )

  res2 <- set_run_as(
    shiny_content,
    run_as = NULL,
    run_as_current_user = FALSE
  )

  expect_false(
    shiny_content$get_content()$run_as_current_user
  )
})

test_that("run_as_current_user fails for rmd", {
  scoped_experimental_silence()
  expect_error(
    suppressMessages(
      set_run_as(rmd_content, "rstudio-connect", TRUE),
      "400"
    )
  )
})

# ACLs ----------------------------------------------------

context("acl")

test_that("acl methods are deprecated", {
  scoped_experimental_silence()

  lifecycle::expect_deprecated(get_acl_user(cont1_content))
  lifecycle::expect_deprecated(get_acl_group(cont1_content))
})

test_that("acl returns owner once and only once", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")

  # get acl
  acls <- get_acl_user(cont1_content)

  my_guid <- test_conn_1$GET("me")$guid

  # first entry is me
  expect_true(acls[1, ]$guid == my_guid)
})

test_that("add a collaborator works", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")

  # create a user
  collab <- test_conn_1$users_create(username = glue::glue("test_collab{create_random_name()}"), email = "collab@example.com", user_must_set_password = TRUE, user_role = "publisher")
  collab_guid <<- collab$guid

  # add a collaborator
  invisible(acl_add_collaborator(cont1_content, collab_guid))

  expect_equal(get_acl_user_role(cont1_content, collab_guid), "owner")

  # owner is present
  my_guid <- test_conn_1$GET("me")$guid
  expect_equal(get_acl_user_role(cont1_content, my_guid), "owner")
})

test_that("add collaborator twice works", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
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
  withr::local_options(lifecycle_verbosity = "quiet")
  # create a user
  view_user <- test_conn_1$users_create(username = glue::glue("test_viewer{create_random_name()}"), email = "viewer@example.com", user_must_set_password = TRUE, user_role = "viewer")
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
  withr::local_options(lifecycle_verbosity = "quiet")
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
  withr::local_options(lifecycle_verbosity = "quiet")
  # remove a collaborator
  invisible(acl_remove_user(cont1_content, collab_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_false(any(which_match))
})

test_that("remove a collaborator twice works", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  # remove a collaborator
  invisible(acl_remove_user(cont1_content, collab_guid))
  invisible(acl_remove_user(cont1_content, collab_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_false(any(which_match))
})

# ACLs - Side effects ------------------------------------------
#
#   lest POST / DELETE cause trouble...
#
test_that("a collaborator does not affect other collaborators", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  # create a user
  collab_alt <- test_conn_1$users_create(username = glue::glue("test_collab_alt{create_random_name()}"), email = "collab_alt@example.com", user_must_set_password = TRUE, user_role = "publisher")
  collab_alt_guid <<- collab_alt$guid

  # add both
  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_add_collaborator(cont1_content, collab_alt_guid))

  acls <- get_acl_user(cont1_content)

  # both present
  expect_true(all(c(collab_guid, collab_alt_guid) %in% acls$guid))

  # remove one
  invisible(acl_remove_user(cont1_content, collab_alt_guid))

  acls2 <- get_acl_user(cont1_content)
  # other present
  expect_true(collab_guid %in% acls2$guid)
})

test_that("a collaborator and a viewer do not affect each other", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
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

  invisible(acl_remove_user(cont1_content, collab_guid))
  acls2 <- get_acl_user(cont1_content)

  which_match_viewer2 <- purrr::map2_lgl(acls2$guid, acls2$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_true(any(which_match_viewer2))

  invisible(acl_add_collaborator(cont1_content, collab_guid))
  invisible(acl_remove_user(cont1_content, viewer_guid))

  acls3 <- get_acl_user(cont1_content)
  which_match_collab3 <- purrr::map2_lgl(acls3$guid, acls3$app_role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_true(any(which_match_collab3))
})

test_that("a viewer does not affect other viewers", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  # create a user
  view_user_alt <- test_conn_1$users_create(username = glue::glue("test_viewer_alt{create_random_name()}"), email = "viewer_alt@example.com", user_must_set_password = TRUE, user_role = "viewer")
  viewer_alt_guid <<- view_user_alt$guid

  # add both
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_add_viewer(cont1_content, viewer_alt_guid))

  acls <- get_acl_user(cont1_content)

  # both present
  expect_true(all(c(viewer_guid, viewer_alt_guid) %in% acls$guid))

  # remove one
  invisible(acl_remove_user(cont1_content, viewer_alt_guid))

  acls2 <- get_acl_user(cont1_content)

  # other present
  expect_true(viewer_guid %in% acls2$guid)
})

test_that("a collaborator can be added as a viewer (overwrites)", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
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
  withr::local_options(lifecycle_verbosity = "quiet")
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
  withr::local_options(lifecycle_verbosity = "quiet")
  # remove a viewer
  invisible(acl_add_viewer(cont1_content, viewer_guid))
  invisible(acl_remove_user(cont1_content, viewer_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_false(any(which_match))
})

test_that("remove a viewer twice works", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  # remove a viewer
  invisible(acl_remove_user(cont1_content, viewer_guid))
  invisible(acl_remove_user(cont1_content, viewer_guid))

  # get acl
  acls <- get_acl_user(cont1_content)

  which_match <- purrr::map2_lgl(acls$guid, acls$app_role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_false(any(which_match))
})

test_that("get_acl_user_role works", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  acl_remove_user(cont1_content, collab_guid)
  acl_remove_user(cont1_content, viewer_guid)

  acl_add_collaborator(cont1_content, collab_guid)
  expect_equal(get_acl_user_role(cont1_content, collab_guid), "owner")

  acl_add_viewer(cont1_content, viewer_guid)
  expect_equal(get_acl_user_role(cont1_content, viewer_guid), "viewer")
})


test_that("get_acl_user_role with null user_guid returns NULL", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_null(get_acl_user_role(cont1_content, NULL))
})

test_that("get_acl_user_role with no role returns NULL", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  acl_remove_user(cont1_content, viewer_guid)
  expect_null(get_acl_user_role(cont1_content, viewer_guid))
})

test_that("acl_add_self works", {
  skip("not yet tested")
})

test_that("acl_remove_self works", {
  skip("not yet tested")
})

# ACLs - Groups ---------------------------------------------------

# TODO: Beware... this state can get muddy if the content has been modified
test_that("acl_add_group works", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  grp <- test_conn_1$groups_create(name = create_random_name())

  content_v1 <- acl_add_group(cont2_content, grp$guid, "owner")

  expect_is(content_v1, "Content")

  cacl <- get_acl_group(content_v1)
  expect_equal(purrr::map_chr(vctrs::vec_ptype(cacl), typeof), purrr::map_chr(vctrs::vec_ptype(connectapi_ptypes$acl_group), typeof))
  expect_equal(nrow(cacl), 1)
  expect_equal(get_acl_group_role(content_v1, grp$guid), "owner")

  # remove ACL
  content_v2 <- acl_remove_group(content_v1, grp$guid)
  cacl_new <- get_acl_group(content_v2)
  expect_equal(purrr::map_chr(vctrs::vec_ptype(cacl_new), typeof), purrr::map_chr(vctrs::vec_ptype(connectapi_ptypes$acl_group), typeof))
  expect_equal(nrow(cacl_new), 0)

  expect_null(get_acl_group_role(content_v2, grp$guid))
})

test_that("get_acl_group_role with null user_guid returns NULL", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_null(get_acl_group_role(cont1_content, NULL))
})

test_that("get_acl_group_role with no role returns NULL", {
  scoped_experimental_silence()
  withr::local_options(lifecycle_verbosity = "quiet")
  acl_remove_user(cont1_content, viewer_guid)
  expect_null(get_acl_group_role(cont1_content, viewer_guid))
})

# Permissions ---------------------------------------

context("permissions")

test_that("returns owner permission", {
  skip("not currently implemented")
})

test_that("add a collaborator works", {
  # create a user
  collab <- test_conn_1$users_create(username = glue::glue("test_collab{create_random_name()}"), email = "collab@example.com", user_must_set_password = TRUE, user_role = "publisher")
  collab_guid <<- collab$guid

  # add a collaborator
  invisible(content_add_user(cont1_content, collab_guid, "owner"))

  expect_equal(get_user_permission(cont1_content, collab_guid)$role, "owner")

  # # owner is present
  # my_guid <- test_conn_1$GET("me")$guid
  # expect_equal(get_acl_user_role(cont1_content, my_guid), "owner")
})

test_that("add collaborator twice works", {
  # add a collaborator
  invisible(content_add_user(cont1_content, collab_guid, "owner"))
  invisible(content_add_user(cont1_content, collab_guid, "owner"))

  # get acl
  acls <- get_content_permissions(cont1_content)

  which_match <- purrr::map2_lgl(acls$principal_guid, acls$role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("add a viewer works", {
  # create a user
  view_user <- test_conn_1$users_create(username = glue::glue("test_viewer{create_random_name()}"), email = "viewer@example.com", user_must_set_password = TRUE, user_role = "viewer")
  viewer_guid <<- view_user$guid

  # add a viewer
  invisible(content_add_user(cont1_content, viewer_guid, "viewer"))

  # get acl
  acls <- get_content_permissions(cont1_content)

  which_match <- purrr::map2_lgl(acls$principal_guid, acls$role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("add a viewer twice works", {
  # add a viewer
  invisible(content_add_user(cont1_content, viewer_guid, "viewer"))
  invisible(content_add_user(cont1_content, viewer_guid, "viewer"))

  # get acl
  acls <- get_content_permissions(cont1_content)

  which_match <- purrr::map2_lgl(acls$principal_guid, acls$role, function(.x, .y) {
    .x == viewer_guid && .y == "viewer"
  })
  expect_true(any(which_match))
  expect_equal(sum(which_match), 1)
})

test_that("remove a collaborator works", {
  # remove a collaborator
  invisible(content_delete_user(cont1_content, collab_guid))

  # get acl
  acls <- get_content_permissions(cont1_content)

  which_match <- purrr::map2_lgl(acls$principal_guid, acls$role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_false(any(which_match))
})

test_that("remove a collaborator twice works", {
  # remove a collaborator
  invisible(content_delete_user(cont1_content, collab_guid))
  invisible(content_delete_user(cont1_content, collab_guid))

  # get acl
  acls <- get_content_permissions(cont1_content)

  which_match <- purrr::map2_lgl(acls$principal_guid, acls$role, function(.x, .y) {
    .x == collab_guid && .y == "owner"
  })
  expect_false(any(which_match))
})


