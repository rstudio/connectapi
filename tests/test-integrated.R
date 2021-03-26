library(testthat)
library(connectapi)

if (nchar(Sys.getenv("CONNECTAPI_INTEGRATED")) > 0) {
  progress_reporter <- ProgressReporter$new(max_failures = 1000)
  check_reporter <- CheckReporter$new(file = fs::path("integrated-results-check.txt"))

  reporter_list <- list(progress_reporter, check_reporter)

  if (as.logical(Sys.getenv("IS_JENKINS", "FALSE"))) {
    junit_reporter <- JunitReporter$new(file = fs::path("integrated-results-junit.xml"))
    reporter_list <- c(reporter_list, list(junit_reporter))
  }
  multi_reporter <- MultiReporter$new(reporters = reporter_list)

  integrated_vars <- c(
    server_1 = Sys.getenv("TEST_1_SERVER"),
    key_1 = Sys.getenv("TEST_1_API_KEY"),
    server_2 = Sys.getenv("TEST_2_SERVER"),
    key_2 = Sys.getenv("TEST_2_API_KEY")
  )

  health_checks <- list(
    server_1 = tryCatch(httr::content(httr::GET(paste0(integrated_vars[["server_1"]], "/__ping__"))), error = print),
    server_2 = tryCatch(httr::content(httr::GET(paste0(integrated_vars[["server_2"]], "/__ping__"))), error = print)
  )

  if (
    any(nchar(integrated_vars) == 0) ||
    any(as.logical(lapply(health_checks, function(x) {length(x) > 0})))
  ) {
    str(health_checks)
    stop("One or both of your integration test servers are not healthy")
  }

  devtools::load_all()
  test_dir(rprojroot::find_package_root_file("tests/integrated"), reporter = multi_reporter)
} else {
  message("Not running integrated tests. Set environment variable CONNECTAPI_INTEGRATED=true to run integration tests")
}
