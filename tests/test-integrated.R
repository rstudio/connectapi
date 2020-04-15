library(testthat)
library(connectapi)


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

test_dir(rprojroot::find_testthat_root_file("../integrated"), reporter = multi_reporter)
