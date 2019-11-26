context("deploy")

test_that("dashboard_url_chr works with various length inputs", {
  expect_identical(dashboard_url_chr("a", "b", "c"), "a/connect/#/apps/b/c")
  expect_identical(dashboard_url_chr("a", c("b","c"), "d"), c("a/connect/#/apps/b/d", "a/connect/#/apps/c/d"))
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
    "length.*2"
  )
})
