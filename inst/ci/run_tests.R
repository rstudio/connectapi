options("repos" = c(RSPM = "https://packagemanager.rstudio.com/cran/__linux__/bionic/latest"))
install.packages("devtools")

message("installing dependencies")
setwd("/test")
devtools::install_deps(dep = TRUE)

library(testthat)
devtools::load_all()

message("creating reporters")
progress_reporter <- ProgressReporter$new(max_failures = 1000)
junit_reporter <- JunitReporter$new(file = fs::path("test-results-junit.xml"))
check_reporter <- CheckReporter$new(file = fs::path("test-results-check.txt"))
multi_reporter <- MultiReporter$new(reporters = list(progress_reporter, junit_reporter, check_reporter))

message("creating first admins")
a1 <- create_first_admin(
  "http://connect1:3939",
  "admin", "admin0", "admin@example.com"
)
a2 <- create_first_admin(
  "http://connect2:3939",
  "admin", "admin0", "admin@example.com"
)

message("defining variables")
Sys.setenv(
  "TEST_1_SERVER" = a1$host,
  "TEST_1_API_KEY" = a1$api_key,
  "TEST_2_SERVER" = a2$host,
  "TEST_2_API_KEY" = a2$api_key
  )

message("running integration tests")
testthat::test_dir(path = "tests/testthat/integrated-tests/", reporter = multi_reporter, stop_on_failure = FALSE)
