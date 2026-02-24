expect_column_types <- function(actual, expected_types, exact = TRUE) {
  # expected_types is a named list: list(col_name = "type_string", ...)
  if (!exact) {
    shared_names <- intersect(names(actual), names(expected_types))
    expected_types <- expected_types[shared_names]
  }
  for (nm in names(expected_types)) {
    expect_true(
      nm %in% names(actual),
      info = paste("Expected column", nm, "not found")
    )
    if (nm %in% names(actual)) {
      expect_true(
        inherits(actual[[nm]], expected_types[[nm]]),
        info = paste0(
          "Column '", nm, "': expected ", expected_types[[nm]],
          ", got ", paste(class(actual[[nm]]), collapse = "/")
        )
      )
    }
  }
}

skip_if_connect_older_than <- function(client, version) {
  current <- numeric_version(simplify_version(safe_server_version(client)))
  if (current < numeric_version(version)) {
    skip(paste("Requires Connect >=", version))
  }
}

deploy_example <- function(connect, name, ...) {
  example_dir <- rprojroot::find_package_root_file(
    "tests",
    "testthat",
    "examples",
    name
  )
  bundle_file <- fs::file_temp(pattern = name, ext = ".tar.gz")
  bund <- bundle_dir(path = example_dir, filename = bundle_file)

  tsk <- deploy(
    connect = connect,
    bundle = bund,
    name = name,
    ...
  )

  guid <- tsk$content$guid
  content <- content_item(tsk$connect, guid)

  # TODO: a smarter, noninteractive wait...
  suppressMessages(poll_task(tsk))
  content
}
