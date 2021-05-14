context("deploy")

# setup ---------------------------------------------------

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

# bundle ---------------------------------------------------

test_that("bundle_static deploys", {
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  uniq_id <- uuid::UUIDgenerate()
  deployed <- deploy(test_conn_1, bnd, uniq_id)

  expect_true(validate_R6_class(bnd, "Bundle"))
  expect_true(validate_R6_class(deployed, "Content"))

  deployed2 <- deploy(test_conn_1, bnd, uniq_id)
  expect_true(validate_R6_class(deployed2, "Content"))
})

test_that("bundle_dir deploys", {
  dir_path <- rprojroot::find_package_root_file("tests/testthat/examples/static")
  tmp_file <- fs::file_temp(pattern = "bundle", ext = ".tar.gz")
  bund <- bundle_dir(path = dir_path, filename = tmp_file)

  expect_equal(tmp_file, bund$path)

  # with a name / title
  tsk <- deploy(connect = test_conn_1, bundle = bund, name = cont1_name, title = cont1_title)

  cont1_guid <<- tsk$get_content()$guid
  cont1_content <<- tsk

  # how should we test that deployment happened?
  expect_true(validate_R6_class(tsk, "Content"))
  expect_equal(tsk$get_content()$name, cont1_name)
  expect_equal(tsk$get_content()$title, cont1_title)

  expect_true(validate_R6_class(tsk, "ContentTask"))
  expect_gt(nchar(tsk$get_task()$task_id), 0)

  # with a guid
  tsk2 <- deploy(connect = test_conn_1, bundle = bund, guid = cont1_guid)
  expect_true(validate_R6_class(tsk2, "Content"))
  expect_equal(tsk2$get_content()$name, cont1_name)
  expect_equal(tsk2$get_content()$title, cont1_title)
  expect_equal(tsk2$get_content()$guid, cont1_guid)
})

test_that("bundle_path deploys", {
  tar_path <- rprojroot::find_package_root_file("tests/testthat/examples/static.tar.gz")
  bund <- bundle_path(path = tar_path)

  expect_equal(tar_path, as.character(bund$path))

  # deploy to a new endpoint
  tsk <- deploy(connect = test_conn_1, bundle = bund)

  # how should we test that deployment happened?
  expect_true(validate_R6_class(tsk, "Content"))
})

# deploy ---------------------------------------------------

test_that("strange name re-casing does not break things", {
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  testname <- "test_Test_45"
  deploy1 <- deploy(test_conn_1, bnd, testname)
  deploy2 <- deploy(test_conn_1, bnd, testname)

  testname2 <- "test_Test"
  deployA <- deploy(test_conn_1, bnd, testname2)
  deployB <- deploy(test_conn_1, bnd, testname2)
})

test_that(".pre_deploy hook works", {
  scoped_experimental_silence()
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  deployed <- deploy(test_conn_1, bnd, uuid::UUIDgenerate(), .pre_deploy = {
    content %>% set_vanity_url(glue::glue("pre_deploy_{bundle_id}"))
  })

  active_bundle <- deployed$get_content_remote()$bundle_id
  expect_equal(
    get_vanity_url(deployed)$vanity$path_prefix,
    as.character(glue::glue("/pre_deploy_{active_bundle}/"))
  )
})

# iamge ---------------------------------------------------

test_that("set_image_path works", {
  scoped_experimental_silence()
  img_path <- rprojroot::find_package_root_file("tests/testthat/examples/logo.png")

  res <- set_image_path(cont1_content, img_path)

  expect_true(validate_R6_class(res, "Content"))
})

test_that("get_image works", {
  scoped_experimental_silence()
  img_path <- rprojroot::find_package_root_file("tests/testthat/examples/logo.png")

  tmp_img <- fs::file_temp(pattern = "img", ext = ".png")
  get_image(cont1_content, tmp_img)

  expect_identical(
    readBin(img_path, "raw"),
    readBin(tmp_img, "raw")
  )

  # works again (i.e. does not append data)
  get_image(cont1_content, tmp_img)
  expect_identical(
    readBin(img_path, "raw"),
    readBin(tmp_img, "raw")
  )

  # works with no path
  auto_path <- get_image(cont1_content)
  expect_identical(
    readBin(img_path, "raw"),
    readBin(auto_path, "raw")
  )
  expect_identical(fs::path_ext(auto_path), "png")
})

test_that("has_image works with an image", {
  scoped_experimental_silence()

  expect_true(has_image(cont1_content))
})

test_that("delete_image works", {
  scoped_experimental_silence()
  # from above
  img_path <- rprojroot::find_package_root_file("tests/testthat/examples/logo.png")

  tmp_img <- fs::file_temp(pattern = "img", ext = ".png")
  # retains the image at the path
  expect_false(fs::file_exists(tmp_img))
  expect_true(validate_R6_class(delete_image(cont1_content, tmp_img), "Content"))
  expect_true(fs::file_exists(tmp_img))
  expect_identical(
    readBin(img_path, "raw"),
    readBin(tmp_img, "raw")
  )
  expect_false(has_image(cont1_content))

  # works again - i.e. if no image available
  expect_true(validate_R6_class(delete_image(cont1_content), "Content"))
})

test_that("has_image works with no image", {
  scoped_experimental_silence()

  expect_false(has_image(cont1_content))
})

test_that("get_image returns NA if no image", {
  scoped_experimental_silence()

  tmp_img <- fs::file_temp(pattern = "img", ext = ".png")
  response <- get_image(cont1_content, tmp_img)

  expect_false(identical(tmp_img, response))
  expect_true(is.na(response))
})

test_that("set_image_url works", {
  # need to find a reliable image URL that is small
  # ... and we are willing to take a dependency on...
  # or... we could use the Connect instance itself :p
  skip("not implemented yet")
})

test_that("set_image_webshot works", {
  skip("currently broken")
  scoped_experimental_silence()
  res <- set_image_webshot(cont1_content)

  expect_true(validate_R6_class(res, "Content"))
})

# vanity_url ---------------------------------------------------

test_that("set_vanity_url works", {
  scoped_experimental_silence()
  res <- set_vanity_url(cont1_content, cont1_name)

  expect_true(validate_R6_class(res, "Vanity"))
  expect_equal(res$get_vanity()$path_prefix, paste0("/", cont1_name, "/"))

  res2 <- set_vanity_url(cont1_content, paste0(cont1_name, "update"))
  expect_true(validate_R6_class(res2, "Vanity"))
  expect_equal(res2$get_vanity()$path_prefix, paste0("/", cont1_name, "update/"))
})


test_that("get_vanity_url works", {
  scoped_experimental_silence()
  tmp_content_name <- uuid::UUIDgenerate()
  tmp_content_prep <- content_ensure(test_conn_1, name = tmp_content_name)
  tmp_content <- Content$new(connect = test_conn_1, content = tmp_content_prep)

  # without a vanity
  curr_vanity <- get_vanity_url(tmp_content)
  expect_true(validate_R6_class(curr_vanity, "Content"))
  expect_error(validate_R6_class(curr_vanity, "Vanity"), regexp = "R6 Vanity")

  # with a vanity
  res <- set_vanity_url(tmp_content, tmp_content_name)
  existing_vanity <- get_vanity_url(tmp_content)
  expect_true(validate_R6_class(existing_vanity, "Vanity"))
  expect_equal(existing_vanity$get_vanity()$path_prefix, paste0("/", tmp_content_name, "/"))
})

# misc functions ---------------------------------------------------

test_that("poll_task works and returns its input", {
  expect_message(
    res <- poll_task(cont1_content)
  )
  expect_equal(res, cont1_content)
})

test_that("download_bundle works", {
  bnd <- download_bundle(content_item(test_conn_1, cont1_guid))

  expect_true(validate_R6_class(bnd, "Bundle"))
})

test_that("download_bundle throws an error for undeployed content", {
  cont_prep <- content_ensure(test_conn_1)
  cont <- content_item(test_conn_1, cont_prep$guid)

  expect_error(
    download_bundle(cont),
    "This content has no bundle_id"
  )
})

test_that("dashboard_url resolves properly", {
  cont <- content_item(test_conn_1, cont1_guid)

  dash_url <- dashboard_url(cont)

  skip("not yet tested")
})

test_that("deployment timestamps respect timezone", {
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  myc <- deploy(test_conn_1, bnd)
  myc_guid <- myc$get_content()$guid

  # will fail without the png package
  invisible(tryCatch(test_conn_1$GET_URL(myc$get_url()), error = function(e){}))

  allusg <- get_usage_static(test_conn_1, content_guid = myc_guid)

  # we just did this, so it should be less than 1 minute ago...
  # (really protecting against being off by hours b/c of timezone differences)
  expect_true(any((Sys.time() - allusg$time) < lubridate::make_difftime(60, "seconds")))
})
