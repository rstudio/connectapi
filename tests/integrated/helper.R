expect_ptype_equal <- function(actual, expected, exact = TRUE) {
  if (!exact) {
    # Keep only the columns from each that are in the other
    shared_names <- intersect(names(actual), names(expected))
    actual <- actual[, shared_names]
    expected <- expected[, shared_names]
  }
  expect_equal(vctrs::vec_ptype(actual), vctrs::vec_ptype(expected))
}

skip_if_connect_older_than <- function(client, version) {
  connect_version <- safe_server_version(client)
  if (is.null(connect_version) || nchar(connect_version) == 0) {
    connect_version <- "0"
  }
  if (numeric_version(connect_version) < numeric_version(version)) {
    skip(paste("Requires Connect >=", version))
  }
}
