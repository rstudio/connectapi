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
    "No tags"
  )
  expect_output(
    print(connect_tag_tree(list())),
    "No tags"
  )
})

test_that("print method ends in a newline", {
  c0 <- capture.output({
    print(connect_tag_tree(list()))
    cat("hi")
  })
  expect_length(c0, 3)
  expect_equal(c0[3], "hi")

  c1 <- capture.output({
    print(simple_tag_tree)
    cat("max")
  })
  expect_identical(c1[length(c1)], "max")
})

test_that("print methods work as expected", {
  skip("not tested yet")
})

test_that("$ works as expected", {
  expect_is(simple_tag_tree$hi, "connect_tag_tree")
  expect_is(simple_tag_tree$hi$ho, "connect_tag_tree")
  expect_named(simple_tag_tree$hi$ho, c("name", "id"))
  expect_output(print(simple_tag_tree$hi), "filtered")
  expect_is(simple_tag_tree$hi$ho$name, "character")
  expect_is(simple_tag_tree$hi$ho$id, "numeric")
})

test_that("[ works as expected", {
  # drops the connect_tag_tree class
  # because maintaining the structure becomes hard...

  expect_warning(simple_tag_tree["hi"], "drops")
  expect_false(inherits(simple_tag_tree["hi"], "connect_tag_tree"))
  expect_is(simple_tag_tree["hi"], "list")

  # clear "warn_once" state
  warn_clear("[.connect_tag_tree")
})

test_that("[[ works as expected", {
  expect_is(simple_tag_tree[["hi"]], "connect_tag_tree")
  expect_is(simple_tag_tree[["hi"]][["ho"]], "connect_tag_tree")
  expect_named(simple_tag_tree[["hi"]][["ho"]], c("name", "id"))
  expect_output(print(simple_tag_tree[["hi"]]), "filtered")
  expect_is(simple_tag_tree[["hi"]][["ho"]][["name"]], "character")
  expect_is(simple_tag_tree[["hi"]][["ho"]][["id"]], "numeric")
})

test_that("filter_tag_tree_chr works as expected", {
  tt <- simple_tag_tree

  expect_length(filter_tag_tree_chr(tt, "hi"), 1)
  expect_length(filter_tag_tree_chr(tt, "ho"), 1)
  expect_length(filter_tag_tree_chr(tt, "ho")[["hi"]], 3) # name, id, ho

  expect_length(filter_tag_tree_chr(tt, "(ho)|(away)")[["hi"]], 4) # name, id, ho, away
})

test_that("filter_tag_tree_id works as expected", {
  tt <- simple_tag_tree

  expect_length(filter_tag_tree_id(tt, 1), 1)
  expect_length(filter_tag_tree_id(tt, 2), 1)
  expect_length(filter_tag_tree_id(tt, 2)[["hi"]], 3) # name, id, ho

  expect_length(filter_tag_tree_id(tt, c(2, 4))[["hi"]], 4) # name, id, ho, away
})

test_that("filter handles no responses", {
  tt <- simple_tag_tree

  expect_length(filter_tag_tree_chr(tt, "something"), 0)
  expect_length(filter_tag_tree_id(tt, 45), 0)
})

test_that("filter handles no input", {
  tt <- simple_tag_tree

  expect_length(filter_tag_tree_chr(tt, character()), 0)
})

test_that("filter handles no input", {
  tt <- simple_tag_tree

  expect_length(filter_tag_tree_id(tt, integer()), 0)
})

test_that("restructure from tag_data works", {
  ref_time <- Sys.time()
  ref_time_str <- format(ref_time, "%Y-%m-%dT%H:%M:%SZ")
  tag_data <- list(
    list(id = "1", name = "First Tag", parent_id = NA, created_time = ref_time_str, updated_time = ref_time_str),
    list(id = "2", name = "Second Tag", parent_id = NA, created_time = ref_time_str, updated_time = ref_time_str),
    list(id = "3", name = "Third Tag", parent_id = "1", created_time = ref_time_str, updated_time = ref_time_str),
    list(id = "4", name = "Fourth Tag", parent_id = "3", created_time = ref_time_str, updated_time = ref_time_str),
    list(id = "5", name = "Fifth Tag", parent_id = "2", created_time = ref_time_str, updated_time = ref_time_str)
  )
  t1 <- tag_tree_from_data(tag_data)


  tbl <- purrr::map_df(tag_data, identity)

  t2 <- tag_tree_from_data(tbl)

  expect_identical(t1, t2)
})
