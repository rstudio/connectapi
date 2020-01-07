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

test_that("create tags works", {
  scoped_experimental_silence()
  parent_tag <<- test_conn_1$create_tag(parent_tag_name)
  child_tag <<- test_conn_1$create_tag(child_tag_name, parent_tag$id)
  
  expect_identical(parent_tag$name, parent_tag_name)
  expect_identical(child_tag$name, child_tag_name)
  expect_identical(child_tag$parent_id, parent_tag$id)
  
  # tag_id lookup works
  test_conn_1$get_tags(TRUE)
  expect_equal(child_tag$id, test_conn_1$get_tag_id(child_tag_name))
})


test_that("associate tag with content", {
  scoped_experimental_silence()
  tag_content <<- deploy(
    test_conn_1, 
    bundle_path(
      rprojroot::find_testthat_root_file("examples", "static.tar.gz")
      ), 
    name = content_name
    )
  
  # set tag
  test_conn_1$set_content_tag(tag_content$get_content()$guid, child_tag$id)
  
  # ensure content is found
  res <- test_conn_1$get_apps(filter = list(tag = child_tag$id))
  
  expect_true(
    tag_content$get_content()$guid %in% purrr::map_chr(res, ~.x$guid)
    )
})


test_that("identical tag names are searched properly", {
  scoped_experimental_silence()
  tag_content_guid <- tag_content$get_content()$guid
  
  # create another tag with same name
  child_tag_2 <- test_conn_1$create_tag(
    name = child_tag_name, 
    parent_id = child_tag$id
    )
  
  # search with the "empty" tag in front
  res <- test_conn_1$get_apps(
    filter = list(tag = child_tag_2$id, tag = child_tag$id),
    .collapse = "||"
    )
  
  expect_true(
     tag_content_guid %in% purrr::map_chr(res, ~.x$guid)
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
  
  guids <- purrr::map_chr(res2, ~.x$guid)
  expect_true(length(guids[guids == tag_content_guid]) == 1)
})

test_that("tag_page works", {
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
