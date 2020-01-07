context("get")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

cont1_name <- uuid::UUIDgenerate()
cont1_title <- "Test Content 1"
cont1_guid <- NULL
cont1_bundle <- NULL
cont1_content <- NULL

test_that("get_procs works", {
  scoped_experimental_silence()
  proc_data <- get_procs(test_conn_1)
  
  # TODO: This is not a great test, since no processes are running
  # we could always start a content restoration...
  expect_is(proc_data, "tbl_df")
  expect_identical(vctrs::vec_ptype(proc_data), vctrs::vec_ptype(connectapi_ptypes$procs))
})
