library(testthat)
library(connectapi)

progress_reporter <- ProgressReporter$new(max_failures = 1000)
junit_reporter <- JunitReporter$new(file = fs::path("test-results-junit.xml"))
check_reporter <- CheckReporter$new(file = fs::path("test-results-check.txt"))
rstudio_reporter <- RstudioReporter$new(file = fs::path("test-results-rstudio.txt"))
multi_reporter <- MultiReporter$new(reporters = list(progress_reporter, junit_reporter, check_reporter, rstudio_reporter))

setwd("tests")
testthat::test_check(package = "connectapi", report = multi_reporter, stop_on_failure = FALSE)
