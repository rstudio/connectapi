context("test misc")

# should connect with env vars
test_conn_1 <- connect(prefix = "TEST_1")
test_conn_2 <- connect(prefix = "TEST_2")

test_that("audit_logs work", {
  logs <- test_conn_1$audit_logs()
  expect_gt(length(logs$results), 0)

  logs2 <- test_conn_1$audit_logs(nxt = logs$paging$cursors$`next`)
  expect_gt(length(logs2$results), 0)
})

test_that("server_settings work", {
  ss <- test_conn_1$server_settings()

  expect_gt(length(ss), 0)
})

test_that("server_settings_r work", {
  ssr <- test_conn_1$server_settings_r()

  expect_gt(length(ssr$installations), 0)
})
