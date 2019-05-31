context("test printing")

# regenerate test printing file with the following
# writeLines(connectapi:::generate_R6_print_output(), rprojroot::find_testthat_root_file("test-print-output.txt"))

test_that("output matches previous expectation", {
  expect_equal(
    generate_R6_print_output(),
    readLines(rprojroot::find_testthat_root_file("test-print-output.txt"))
  )
})
