context("audit")

test_that("audit_vanity_urs fails on missing protocol", {
  apps <- list(
    list(url = "http://myserver.com/hello"),
    list(url = "http://myserver.com/goodbye/")
  )
  expect_error(
    audit_vanity_urls(apps, "myserver.com"),
    "protocol"
  )
})
test_that("audit_vanity_urls handles slashes", {

  apps <- list(
    list(url = "http://myserver.com/hello"),
    list(url = "http://myserver.com/goodbye/"),
    list(url = "http://myserver.com/hello/goodbye/")
  )

  expect_equal(
    audit_vanity_urls(apps, "http://myserver.com"),
    c("/hello/", "/goodbye/", "/hello/goodbye/")
  )

  apps2 <- list(
    list(url = "https://myserver.com/rstudio/hello"),
    list(url = "https://myserver.com/rstudio/goodbye/"),
    list(url = "https://myserver.com/rstudio/hello/goodbye/")
  )

  expect_equal(
    audit_vanity_urls(apps2, "https://myserver.com/rstudio/"),
    c("/hello/", "/goodbye/", "/hello/goodbye/")
  )
})
