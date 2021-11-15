context("test tags")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

parent_tag_name <- uuid::UUIDgenerate(use.time = TRUE)
child_tag_name <- uuid::UUIDgenerate(use.time = TRUE)

parent_tag <- NULL
child_tag <- NULL

content_name <- uuid::UUIDgenerate(use.time = TRUE)

tag_content <- NULL

check_tag_exists <- function(con, id) {
  res <- tryCatch(suppressMessages(con$tag(id)), error = function(e){return(e)})
  if (is.numeric(res[["id"]])) {
    TRUE
  } else if (regexpr("simpleError", res) && regexpr("(404) Not Found", res)) {
    FALSE
  } else {
    stop("error retrieving tag")
  }
}

# Tests ---------------------------------------

test_that("create tags works", {
  parent_tag <<- test_conn_1$tag_create(parent_tag_name)
  child_tag <<- test_conn_1$tag_create(child_tag_name, parent_tag$id)

  expect_identical(parent_tag$name, parent_tag_name)
  expect_identical(child_tag$name, child_tag_name)
  expect_identical(child_tag$parent_id, parent_tag$id)

  # tag_id lookup works
  test_conn_1$get_tags(TRUE)
  expect_equal(child_tag$id, test_conn_1$get_tag_id(child_tag_name))
})

test_that("associate tag with content", {
  tag_content <<- deploy(
    test_conn_1,
    bundle_path(
      rprojroot::find_package_root_file("tests/testthat/examples", "static.tar.gz")
    ),
    name = content_name
  )

  # set tag
  test_conn_1$set_content_tag(tag_content$get_content()$guid, child_tag$id)

  # ensure content is found
  res <- test_conn_1$get_apps(filter = list(tag = child_tag$id))

  expect_true(
    tag_content$get_content()$guid %in% purrr::map_chr(res, ~ .x$guid)
  )
})

## Test high level functions --------------------------------------------------

test_that("get_tags works", {
  scoped_experimental_silence()
  atags <- get_tags(test_conn_1)
  expect_is(atags, "connect_tag_tree")
})

test_that("create_tag and delete_tag works", {
  scoped_experimental_silence()
  ptag_1 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_1 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_2 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_3 <- uuid::UUIDgenerate(use.time = TRUE)

  capture.output(res <- create_tag(test_conn_1, ptag_1))
  expect_true(validate_R6_class(res, "Connect"))

  tags <- get_tags(test_conn_1)
  capture.output(create_tag(test_conn_1, ctag_1, tags[[ptag_1]]))

  tags <- get_tags(test_conn_1)
  capture.output(create_tag(test_conn_1, ctag_2, tags[[ptag_1]][[ctag_1]]))

  tags <- get_tags(test_conn_1)
  capture.output(create_tag(test_conn_1, ctag_3, tags[[ptag_1]][[ctag_1]][[ctag_2]]))

  tags <- get_tags(test_conn_1)

  delete_tag(test_conn_1, tags[[ptag_1]][[ctag_1]][[ctag_2]][[ctag_3]])
  expect_false(check_tag_exists(test_conn_1, tags[[ptag_1]][[ctag_1]][[ctag_2]][[ctag_3]][["id"]]))

  delete_tag(test_conn_1, tags[[ptag_1]][[ctag_1]][[ctag_2]])
  expect_false(check_tag_exists(test_conn_1, tags[[ptag_1]][[ctag_1]][[ctag_2]][["id"]]))

  delete_tag(test_conn_1, tags[[ptag_1]][[ctag_1]])
  expect_false(check_tag_exists(test_conn_1, tags[[ptag_1]][[ctag_1]][["id"]]))

  res <- delete_tag(test_conn_1, tags[[ptag_1]])
  expect_false(check_tag_exists(test_conn_1, tags[[ptag_1]][["id"]]))

  expect_true(validate_R6_class(res, "Connect"))
})

test_that("delete_tag errs for whole tree", {
  scoped_experimental_silence()
  alltags <- get_tags(test_conn_1)

  expect_error(
    delete_tag(test_conn_1, alltags),
    "entire tag tree"
  )
})

test_that("create_tag_tree works", {
  scoped_experimental_silence()
  ptag_1 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_1 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_2 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_3 <- uuid::UUIDgenerate(use.time = TRUE)

  capture.output(a1 <- create_tag_tree(test_conn_1, ptag_1, ctag_1))
  expect_true(validate_R6_class(a1, "Connect"))

  tags1 <- get_tags(test_conn_1)

  capture.output(a2 <- create_tag_tree(test_conn_1, ptag_1, ctag_1, ctag_2, ctag_3))
  expect_true(validate_R6_class(a2, "Connect"))

  tags2 <- get_tags(test_conn_1)

  expect_identical(tags1[[ptag_1]][["id"]], tags2[[ptag_1]][["id"]])

  delete_tag(test_conn_1, tags2[[ptag_1]])
  expect_error(suppressMessages(test_conn_1$tag(a2[[ptag_1]][["id"]])), "Not Found")
})


test_that("get_content_tags and set_content_tags works", {
  scoped_experimental_silence()
  ptag_1 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_1_1 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_1_2 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_2_1 <- uuid::UUIDgenerate(use.time = TRUE)

  app1 <- deploy(test_conn_1, bundle_dir(rprojroot::find_package_root_file("tests", "testthat", "examples", "static")))

  capture.output(tmp1 <- create_tag_tree(test_conn_1, ptag_1, ctag_1_1, ctag_1_2))
  capture.output(tmp2 <- create_tag_tree(test_conn_1, ptag_1, ctag_2_1))

  expect_true(validate_R6_class(tmp1, "Connect"))
  expect_true(validate_R6_class(tmp2, "Connect"))

  ct <- get_content_tags(app1)
  expect_length(ct, 0)

  all_tags <- get_tags(test_conn_1)

  c1o <- capture.output(
    c1 <- set_content_tags(
      app1,
      all_tags[[ptag_1]][[ctag_1_1]][[ctag_1_2]]
      )
    )
  expect_identical(c1, app1)
  expect_length(get_content_tags(app1), 1)

  c2o <- capture.output(
    c2 <- set_content_tags(
      app1,
      all_tags[[ptag_1]][[ctag_1_1]][[ctag_1_2]],
      all_tags[[ptag_1]][[ctag_2_1]]
      )
    )
  expect_identical(c2, app1)
  expect_length(get_content_tags(app1)[[ptag_1]], 4) # 2 tags, id, name

  # TODO: use newer way to delete tags
  app1$tag_delete(get_content_tags(app1)[[ptag_1]][["id"]])
  expect_length(get_content_tags(app1), 0)
})

test_that("set_content_tag_tree works", {
  scoped_experimental_silence()

  ptag_1 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_1_1 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_1_2 <- uuid::UUIDgenerate(use.time = TRUE)
  ctag_2_1 <- uuid::UUIDgenerate(use.time = TRUE)

  app1 <- deploy(test_conn_1, bundle_dir(rprojroot::find_package_root_file("tests", "testthat", "examples", "static")))

  capture.output(tmp1 <- create_tag_tree(test_conn_1, ptag_1, ctag_1_1, ctag_1_2))
  capture.output(tmp2 <- create_tag_tree(test_conn_1, ptag_1, ctag_2_1))

  expect_true(validate_R6_class(tmp1, "Connect"))
  expect_true(validate_R6_class(tmp2, "Connect"))

  ct <- get_content_tags(app1)
  expect_length(ct, 0)

  c1o <- capture.output(
    c1 <- set_content_tag_tree(
      app1,
      ptag_1, ctag_1_1, ctag_1_2
      )
    )
  expect_identical(c1, app1)
  expect_length(get_content_tags(app1), 1)

  c2o <- capture.output(
    c2 <- set_content_tag_tree(
      app1,
      ptag_1, ctag_2_1
      )
    )
  expect_identical(c2, app1)
  expect_length(get_content_tags(app1)[[ptag_1]], 4) # 2 tags, id, name

  # TODO: use newer way to delete tags
  app1$tag_delete(get_content_tags(app1)[[ptag_1]][["id"]])
  expect_length(get_content_tags(app1), 0)
})

test_that("identical tag names are searched properly", {
  tag_content_guid <- tag_content$get_content()$guid

  # create another tag with same name
  child_tag_2 <- test_conn_1$tag_create(
    name = child_tag_name,
    parent_id = child_tag$id
  )

  # search with the "empty" tag in front
  res <- test_conn_1$get_apps(
    filter = list(tag = child_tag_2$id, tag = child_tag$id),
    .collapse = "||"
  )

  expect_true(
    tag_content_guid %in% purrr::map_chr(res, ~ .x$guid)
  )

  # check that duplicates do not happen
  test_conn_1$set_content_tag(
    tag_content_guid,
    child_tag_2$id
  )

  res2 <- test_conn_1$get_apps(
    filter = list(tag = child_tag_2$id, tag = child_tag$id),
    .collapse = "||"
  )

  guids <- purrr::map_chr(res2, ~ .x$guid)
  expect_true(length(guids[guids == tag_content_guid]) == 1)
})

# Tag Page -------------------------------------

context("tag_page")

test_that("tag_page works", {
  skip("test currently broken")
  scoped_experimental_silence()
  res <- tag_page(
    test_conn_1,
    tag = child_tag_name,
    quiet = TRUE
  )

  expect_true(fs::file_exists(res$LANDING_PAGE))
  expect_true(length(res$APPS) > 0)
  unlink(res$LANDING_PAGE)
  fs::dir_delete(paste0(fs::path_ext_remove(res$LANDING_PAGE), "-screenshots"))
})

test_that("tag_page_iframe works", {
  scoped_experimental_silence()
  res <- tag_page_iframe(
    test_conn_1,
    tag = child_tag_name,
    quiet = TRUE
  )

  expect_true(fs::file_exists(res$LANDING_PAGE))
  expect_true(length(res$APPS) > 0)
  unlink(res$LANDING_PAGE)
})

