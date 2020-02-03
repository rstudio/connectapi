library(testthat)
library(connectapi)

progress_reporter <- ProgressReporter$new(max_failures = 1000)
check_reporter <- CheckReporter$new(file = fs::path("test-results-check.txt"))

reporter_list <- list(progress_reporter, check_reporter)

if (as.logical(Sys.getenv("IS_JENKINS"))) {
  junit_reporter <- JunitReporter$new(file = fs::path("test-results-junit.xml"))
  reporter_list <- c(reporter_list, list(junit_reporter))
}
multi_reporter <- MultiReporter$new(reporters = reporter_list)

test_check("connectapi", reporter = multi_reporter)
