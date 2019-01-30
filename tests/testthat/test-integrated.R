integrated_vars <- c(
  server_1 = Sys.getenv("TEST_SERVER_1"),
  key_1 = Sys.getenv("TEST_KEY_1"),
  server_2 = Sys.getenv("TEST_SERVER_2"),
  key_2 = Sys.getenv("TEST_KEY_2")
)

health_checks <- list(
  server_1 = httr::content(httr::GET(paste0(integrated_vars[["server_1"]], "/__health-check__"))),
  server_2 = httr::content(httr::GET(paste0(integrated_vars[["server_2"]], "/__health-check__")))
)

# decide if integrated tests can run
if ( 
  all(nchar(integrated_vars) > 0) &&
  all(as.logical(lapply(health_checks, function(x){x$Age > 0})))
  ) {
  test_dir("integrated-tests")
}
