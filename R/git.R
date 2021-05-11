
#' @export
repo_check_account <- function(client, host) {
  validate_R6_class(client, "Connect")
  account <- client$repo_account(host)
  # TODO: add messaging about which account is being used...
  if (nchar(account$username) > 0) {
    message(glue::glue("Accessing '{host}' with account: {account$username}"))
  } else {
    message(glue::glue("Accessing '{host}' anonymously."))
  }
  return(account)
}

#' @export
repo_check_branches <- function(client, repository) {
  validate_R6_class(client, "Connect")
  branches_raw <- client$repo_branches(repository)
  branches_task <- Task$new(connect = client, task = branches_raw)

  task_res <- poll_task(branches_task, callback = identity)
  task_data <- task_res$get_data()
  stopifnot(identical(task_data$type, "git-repo-ref-branch-array"))
  branches <- purrr::map(task_data$data, ~ .x$branch)
  purrr::set_names(branches, branches)
}

#' @export
repo_check_manifest_dirs <- function(client, repository, branch) {
  validate_R6_class(client, "Connect")
  manifest_dirs_raw <- client$repo_manifest_dirs(repo = repository, branch = branch)
  manifest_dirs_task <- Task$new(connect = client, task = manifest_dirs_raw)

  task_res <- poll_task(manifest_dirs_task, callback = identity)
  task_data <- task_res$get_data()
  stopifnot(identical(task_data$type, "git-repo-branch-manifest-dirs-array"))
  manifest_dirs <- purrr::map(task_data$data, ~ .x)
  purrr::set_names(manifest_dirs, manifest_dirs)
}


#' @export
deploy_repo <- function(client, repository, branch, subdirectory, name = create_random_name(), title = name, ...) {
  validate_R6_class(client, "Connect")

  content_metadata <- content_ensure(connect = client, name = name, title = title, ..., .permitted = c("new"))

  deployed_content <- content_item(client, content_metadata$guid)
  res <- deployed_content$repo_set(repository = repository, branch = branch, subdirectory = subdirectory)

  task <- deployed_content$deploy()

  ContentTask$new(connect = client, content = content_metadata, task = task)
}
