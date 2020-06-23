context("connect_tag_tree")

simple_tag_tree <- connect_tag_tree(
  list(
    hi = list(
      name = "hi", id = 1, 
      ho = list(name = "ho", id = 2), 
      silver = list(name = "silver", id = 3), 
      away = list(name = "away", id = 4)
      )
    )
  )

test_that("works with no input", {
  expect_output(
    print(connect_tag_tree(list(), NULL)),
    "No tags defined"
  )
  expect_output(
    print(connect_tag_tree(list())),
    "No tags defined"
  )
})

test_that("print methods work as expected", {
  skip("not tested yet")
})

test_that("$ works as expected", {
  skip("not tested yet")
})

test_that("[ works as expected", {
  skip("not tested yet")
})

test_that("[[ works as expected", {
  skip("not tested yet")
})

test_that("filter works as expected", {
  
})

test_that("filter handles no responses", {
  tt <- simple_tag_tree
  
  expect_length(filter_tag_tree_chr(tt, character()), 0)
})

test_that("filter handles no input", {
  tt <- simple_tag_tree
  
  expect_length(filter_tag_tree_id(tt, integer()), 0)
})
