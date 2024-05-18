without_internet({
  test_that("Query params to repo_account", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      con$repo_account("https://github.com/rstudio/connectapi"),
      "https://connect.example/__api__/repo/account?url=https%3A%2F%2Fgithub.com"
    )
    expect_error(
      con$repo_account("asdf"),
      "Scheme and hostname must be provided"
    )
  })

  test_that("Query params to repo_branches", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      con$repo_branches("https://github.com/rstudio/connectapi"),
      "https://connect.example/__api__/repo/branches?url=https%3A%2F%2Fgithub.com%2Frstudio%2Fconnectapi"
    )
  })

  test_that("Query params to repo_manifest_dirs", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    expect_GET(
      con$repo_manifest_dirs("https://github.com/rstudio/connectapi", "main"),
      "https://connect.example/__api__/repo/manifest-dirs?url=https%3A%2F%2Fgithub.com%2Frstudio%2Fconnectapi&branch=main"
    )
  })
})
