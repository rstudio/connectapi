
repo_check_account <- function(client, host) {
  account <- client$repo_account(host)
  # TODO: add messaging about which account is being used...
  return(client)
}


deploy_repo <- function(client, repository, branch, subdirectory, name = create_random_name(), title = name, ...) {
  validate_R6_class(client, "Connect")

  content_metadata <- content_ensure(connect = client, name = name, title = title, ..., .permitted = c("new"))

  deployed_content <- content_item(client, content_metadata$guid)
  res <- deployed_content$repo_set(repository = repository, branch = branch, subdirectory = subdirectory)

  task <- deployed_content$deploy()

  Task$new(connect = client, content = content_metadata, task = task)
}
