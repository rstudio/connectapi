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

# decide if integrated tests can run
if ( 
  all(nchar(integrated_vars) > 0) &&
  all(as.logical(lapply(health_checks, function(x){length(x) == 0})))
  ) {
  test_dir("integrated-tests")
} else {
  context("integrated tests")
  test_that("all", {
    skip("test environment not set up properly")
  })
}
