context("deploy")

test_that("dashboard_url_chr works with various length inputs", {
  expect_identical(dashboard_url_chr("a", "b", "c"), "a/connect/#/apps/b/c")
  expect_identical(dashboard_url_chr("a", c("b", "c"), "d"), c("a/connect/#/apps/b/d", "a/connect/#/apps/c/d"))
  expect_identical(
    dashboard_url_chr(
      c("a", "b"),
      c("c", "d"),
      c("e", "f")
    ),
    c("a/connect/#/apps/c/e", "b/connect/#/apps/d/f")
  )
})

test_that("dashboard_url_chr fails with invalid inputs", {
  expect_error(
    dashboard_url_chr(c("a", "b", "c"), "d", c("e", "f")),
    class = "purrr_error_bad_element_length"
  )
})

test_that("bundle_dir errors if no manifest.json", {
  expect_error(
    suppressMessages(bundle_dir(rprojroot::find_testthat_root_file("examples/no_manifest"))),
    "no `manifest.json`"
  )
})

test_that("bundle_dir warns if packrat.lock", {
  expect_warning(
    suppressMessages(bundle_dir(rprojroot::find_testthat_root_file("examples", "include_packrat_lock"))),
    "`packrat.lock` file found"
  )
})

test_that("bundle_dir warns if packrat folder", {
  expect_warning(
    suppressMessages(bundle_dir(rprojroot::find_testthat_root_file("examples", "include_packrat"))),
    "`packrat` directory found"
  )
})

test_that("bundle_dir not fooled by subfolders", {
  expect_error(
    suppressMessages(bundle_dir(rprojroot::find_testthat_root_file("examples", "include_multiple_folders"))),
    "no `manifest.json`"
  )
})
