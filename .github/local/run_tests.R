options(
  "repos" = c(
    RSPM = "https://packagemanager.rstudio.com/cran/__linux__/bionic/latest"
    )
  )
install.packages("devtools")

message("installing dependencies")
devtools::install_deps(dep = TRUE)

library(testthat)
devtools::load_all()

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
  "TEST_2_API_KEY" = a2$api_key,
  "CONNECTAPI_INTEGRATED" = "true"
  )

message("sourcing integration tests")
source(rprojroot::find_package_root_file("tests/test-integrated.R"))
