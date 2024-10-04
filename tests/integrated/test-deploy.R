# setup ---------------------------------------------------

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

  # TODO: how should we test that deployment happened?
  expect_true(validate_R6_class(tsk, "Content"))
})

test_that("download_bundle works", {
  tar_path <- rprojroot::find_package_root_file("tests/testthat/examples/static.tar.gz")
  bund <- bundle_path(path = tar_path)

  tsk <- deploy(connect = test_conn_1, bundle = bund)
  poll_task(tsk)
  downloaded <- download_bundle(tsk)

  # TODO: is shasum always available...? A way to do without the shell?
  expect_equal(
    system(glue::glue("shasum {downloaded$path} | cut -d ' ' -f 1"), intern = TRUE),
    system(glue::glue("shasum {bund$path} | cut -d ' ' -f 1"), intern = TRUE)
  )
})

test_that("delete_bundle() and get_bundles() work", {
  tar_path <- rprojroot::find_package_root_file("tests/testthat/examples/static.tar.gz")
  bund <- bundle_path(path = tar_path)

  tsk <- deploy(connect = test_conn_1, bundle = bund)
  poll_task(tsk)
  first_bnd <- tsk$get_content_remote()$bundle_id
  my_guid <- tsk$get_content()$guid

  tsk <- deploy(connect = test_conn_1, bundle = bund, guid = my_guid)
  poll_task(tsk)
  second_bnd <- tsk$get_content_remote()$bundle_id

  all_bnd <- get_bundles(tsk)

  # check get_bundles() output
  expect_true(first_bnd %in% all_bnd[["id"]])
  expect_true(second_bnd %in% all_bnd[["id"]])
  expect_length(all_bnd[["id"]], 2)
  expect_s3_class(all_bnd, "tbl_df")

  # check delete_bundle() functions
  res <- delete_bundle(tsk, first_bnd)
  expect_true(validate_R6_class(res, "Content"))
  expect_length(get_bundles(res)[["id"]], 1)

  expect_error(
    expect_message(
      delete_bundle(tsk, second_bnd),
      "active bundle"
    ),
    "Bad Request"
  )
  expect_length(get_bundles(res)[["id"]], 1)
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
    get_vanity_url(deployed),
    as.character(glue::glue("/pre_deploy_{active_bundle}/"))
  )
})

test_that("deploy_current works", {
  tar_path <- rprojroot::find_package_root_file("tests/testthat/examples/static.tar.gz")
  bund <- bundle_path(path = tar_path)

  tsk <- deploy(connect = test_conn_1, bundle = bund)
  poll_task(tsk)

  created <- tsk$get_content_remote()$created_time
  first_deploy <- tsk$get_content_remote()$last_deployed_time

  # unrelated changes do not modify
  tsk$update(title = "test deploy_current")
  expect_equal(tsk$get_content_remote()$last_deployed_time, first_deploy)

  Sys.sleep(1)

  # a redeploy does
  res <- deploy_current(tsk)
  expect_true(validate_R6_class(res, "ContentTask"))

  expect_true(tsk$get_content_remote()$last_deployed_time > first_deploy)
})

# image ---------------------------------------------------

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
  scoped_experimental_silence()

  res <- set_image_url(cont1_content, glue::glue("{cont1_content$get_connect()$server}/connect/__favicon__"))

  expect_true(validate_R6_class(res, "Content"))

  # TODO: verify round-trip on the image is actually correct... SHA?
})

test_that("set_image_webshot works", {
  skip("test fails commonly in CI")
  scoped_experimental_silence()
  cont1_content$update(access_type = "all")
  res <- set_image_webshot(cont1_content)

  expect_true(validate_R6_class(res, "Content"))
  # TODO: verify round-trip on the image is actually correct... SHA?

  # returns content even when it cannot take the webshot
  cont1_content$update(access_type = "acl")
  expect_warning(
    {
      res <- set_image_webshot(cont1_content)
    },


    "authentication"
  )

  expect_true(validate_R6_class(res, "Content"))
})

# vanity_url ---------------------------------------------------

test_that("set_vanity_url works", {
  new_name <- uuid::UUIDgenerate()
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  cont1 <- deploy(test_conn_1, bnd, name = new_name)
  res <- set_vanity_url(cont1, new_name)

  expect_true(validate_R6_class(res, "Vanity"))
  expect_equal(res$get_vanity()$path, paste0("/", new_name, "/"))

  res2 <- set_vanity_url(cont1, paste0(new_name, "update"))
  expect_true(validate_R6_class(res2, "Vanity"))
  expect_equal(res2$get_vanity()$path, paste0("/", new_name, "update/"))
})

test_that("set_vanity_url force works", {
  new_name <- uuid::UUIDgenerate()
  bnd <- bundle_static(path = rprojroot::find_package_root_file("tests/testthat/examples/static/test.png"))
  cont <- deploy(test_conn_1, bnd, name = new_name)
  res <- set_vanity_url(cont, new_name)

  another_name <- uuid::UUIDgenerate()
  cont2 <- deploy(test_conn_1, bnd, name = another_name)

  expect_error(suppressMessages(set_vanity_url(cont2, new_name, force = FALSE), "409"))

  res2 <- set_vanity_url(cont2, new_name, force = TRUE)
  expect_identical(
    get_vanity_url(cont2),
    paste0("/", new_name, "/")
  )

  expect_null(suppressMessages(get_vanity_url(cont)))
})


test_that("get_vanity_url works", {
  tmp_content_name <- uuid::UUIDgenerate()
  tmp_content_prep <- content_ensure(test_conn_1, name = tmp_content_name)
  tmp_content <- Content$new(connect = test_conn_1, content = tmp_content_prep)

  # without a vanity
  curr_vanity <- suppressMessages(get_vanity_url(tmp_content))
  expect_null(curr_vanity)

  # with a vanity
  res <- set_vanity_url(tmp_content, tmp_content_name)
  existing_vanity <- get_vanity_url(tmp_content)
  expect_type(existing_vanity, "character")
  expect_equal(existing_vanity, paste0("/", tmp_content_name, "/"))
})

test_that("delete_vanity_url works", {
  tmp_content_name <- uuid::UUIDgenerate()
  tmp_content_prep <- content_ensure(test_conn_1, name = tmp_content_name)
  tmp_content <- Content$new(connect = test_conn_1, content = tmp_content_prep)

  # create a vanity
  res <- set_vanity_url(tmp_content, tmp_content_name)
  expect_true(validate_R6_class(res, "Vanity"))
  expect_equal(res$get_vanity()$path, paste0("/", tmp_content_name, "/"))

  # delete the vanity
  res <- delete_vanity_url(tmp_content)
  expect_true(validate_R6_class(res, "Content"))
  expect_error(validate_R6_class(res, "Vanity"), "R6 Vanity")

  # get the vanity
  res <- get_vanity_url(tmp_content)
  expect_null(res)
})

test_that("swap_vanity_url works", {
  tmp_content_name <- uuid::UUIDgenerate()
  tmp_content_prep <- content_ensure(test_conn_1, name = tmp_content_name)
  tmp_content <- Content$new(connect = test_conn_1, content = tmp_content_prep)

  tmp_content2_name <- uuid::UUIDgenerate()
  tmp_content2_prep <- content_ensure(test_conn_1, name = tmp_content2_name)
  tmp_content2 <- Content$new(connect = test_conn_1, content = tmp_content2_prep)

  # warns with no vanity urls
  res <- suppressMessages(expect_warning(swap_vanity_url(tmp_content, tmp_content2)))
  expect_null(res[["from"]])
  expect_null(res[["to"]])

  # works with just one vanity url (from)
  set_from <- set_vanity_url(tmp_content, tmp_content_name)
  swap_res <- suppressMessages(swap_vanity_url(tmp_content, tmp_content2))

  expect_identical(swap_res$to, paste0("/", tmp_content_name, "/"))
  expect_true(grepl("vanity-url", swap_res$from))

  # works with both vanity urls
  swap_res2 <- suppressMessages(swap_vanity_url(tmp_content, tmp_content2))

  expect_identical(swap_res2$from, paste0("/", tmp_content_name, "/"))
  expect_identical(swap_res2$to, swap_res$from)

  # works with just one vanity url (to)
  delete_vanity_url(tmp_content)
  expect_null(get_vanity_url(tmp_content))

  swap_res3 <- swap_vanity_url(tmp_content, tmp_content2)

  expect_identical(swap_res3$from, swap_res$from)
  expect_false(identical(swap_res3$to, swap_res2$from))
  expect_true(grepl("vanity-url", swap_res3$to))
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
  invisible(tryCatch(test_conn_1$GET(url = myc$get_url()), error = function(e) {}))

  all_usage <- get_usage_static(test_conn_1, content_guid = myc_guid)
  for (i in 1:5) {
    if (nrow(all_usage) == 0) {
      # We may need to wait a beat for the metrics to show up.
      # Retry a few times just in case.
      # This did not show up testing against Connect versions <= 2022.09.0,
      # but 2023.03.0 and newer seemed to hit this
      Sys.sleep(1)
      all_usage <- get_usage_static(test_conn_1, content_guid = myc_guid)
    }
  }
  expect_equal(nrow(all_usage), 1)

  # we just did this, so it should be less than 1 minute ago...
  # (really protecting against being off by hours b/c of timezone differences)
  expect_lt(Sys.time() - all_usage$time, lubridate::make_difftime(60, "seconds"))
})
