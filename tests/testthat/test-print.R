context("test printing")

# this (nonstandard) test checks output against a file
# thus allowing an easy preview of expected output
# AND some level of standard / consistency / expectation

# regenerate test printing file with the following
# writeLines(connectapi:::generate_R6_print_output(), rprojroot::find_testthat_root_file("test-print-output.txt"))

test_that("output matches previous expectation", {
  local_edition(3)
  expect_snapshot(generate_R6_print_output())
})
