library(testthat)
library(connectapi)

hosts <- compose_find_hosts(prefix = "ci_connect")

progress_reporter <- ProgressReporter$new(max_failures = 1000)
junit_reporter <- JunitReporter$new(file = fs::path("test-results-junit.xml"))
check_reporter <- CheckReporter$new(file = fs::path("test-results-check.txt"))
rstudio_reporter <- RstudioReporter$new(file = fs::path("test-results-rstudio.txt"))
multi_reporter <- MultiReporter$new(reporters = list(progress_reporter, junit_reporter, check_reporter, rstudio_reporter))

a1 <- create_first_admin(
  hosts[[1]],
  "admin", "admin0", "admin@example.com"
)
a2 <- create_first_admin(
  hosts[[2]],
  "admin", "admin0", "admin@example.com"
)

Sys.setenv(
  "TEST_1_SERVER" = a1$host,
  "TEST_1_API_KEY" = a1$api_key,
  "TEST_2_SERVER" = a2$host,
  "TEST_2_API_KEY" = a2$api_key
  )

testthat::test_dir(path = "tests/testthat/integrated-tests/", reporter = multi_reporter, stop_on_failure = FALSE)
