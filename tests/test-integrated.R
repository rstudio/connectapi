library(testthat)
library(connectapi)

if (nzchar(Sys.getenv("CONNECTAPI_INTEGRATED"))) {
  # Make sure one server is up
  tryCatch(
    httr::content(httr::GET(paste0(Sys.getenv("TEST_1_SERVER"), "/__ping__"))),
    error = function(e) {
      stop("Server 1 is not healthy")
    }
  )

  test_dir(
    rprojroot::find_package_root_file("tests/integrated"),
    reporter = "summary",
    package = "connectapi",
    load_package = "installed"
  )
} else {
  message("Not running integration tests. Set environment variable CONNECTAPI_INTEGRATED=true to run integration tests")
}
