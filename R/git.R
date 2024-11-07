#' Git Repository Helpers
#'
#' `r lifecycle::badge('experimental')` These functions help use Posit Connect's
#' configured authorization to query available branches and subdirectories for
#' deployment using `deploy_repo()`
#'
#' - `repo_check_account()` messages whether an account is in use, and then returns that account
#' - `repo_check_branches()` retrieves which branches are available, returning in a named list
#' - `repo_check_manifest_dirs()` retrieves which directories contain a
#'   `manifest.json`, returning in a named list
#'
#' @param client A Connect R6 object
#' @param host The git repository host (with schema). For example, "https://github.com"
#' @param repository The git repository to explore or consider deploying
#' @param branch The git branch to explore for subdirectories
#'
#' @rdname git
#' @name git
#' @seealso connectapi::deploy_repo
#' @family content functions
#' @export
repo_check_account <- function(client, host) {
  validate_R6_class(client, "Connect")
  warn_experimental("git")
  account <- client$repo_account(host)
  # TODO: add messaging about which account is being used...
  if (nchar(account$username) > 0) {
    message(glue::glue("Accessing '{host}' with account: {account$username}"))
  } else {
    message(glue::glue("Accessing '{host}' anonymously."))
  }
  return(account)
}

#' @rdname git
#' @export
repo_check_branches <- function(client, repository) {
  validate_R6_class(client, "Connect")
  warn_experimental("git")
  branches_raw <- client$repo_branches(repository)
  branches_task <- Task$new(connect = client, task = branches_raw)

  task_res <- poll_task(branches_task, callback = NULL)
  task_data <- task_res$get_data()
  stopifnot(identical(task_data$type, "git-repo-ref-branch-array"))
  branches <- purrr::map(task_data$data, ~ .x$branch)
  purrr::set_names(branches, branches)
}

#' @rdname git
#' @export
repo_check_branches_ref <- function(client, repository) {
  validate_R6_class(client, "Connect")
  warn_experimental("git")
  branches_raw <- client$repo_branches(repository)
  branches_task <- Task$new(connect = client, task = branches_raw)

  task_res <- poll_task(branches_task, callback = NULL)
  task_data <- task_res$get_data()
  stopifnot(identical(task_data$type, "git-repo-ref-branch-array"))
  branches <- purrr::map(task_data$data, ~ .x$branch)
  refs <- purrr::map_chr(task_data$data, ~ .x$ref)
  purrr::set_names(refs, branches)
}

#' @rdname git
#' @export
repo_check_manifest_dirs <- function(client, repository, branch) {
  validate_R6_class(client, "Connect")
  warn_experimental("git")
  manifest_dirs_raw <- client$repo_manifest_dirs(repo = repository, branch = branch)
  manifest_dirs_task <- Task$new(connect = client, task = manifest_dirs_raw)

  task_res <- poll_task(manifest_dirs_task, callback = NULL)
  task_data <- task_res$get_data()
  stopifnot(identical(task_data$type, "git-repo-branch-manifest-dirs-array"))
  manifest_dirs <- purrr::map(task_data$data, ~.x)
  purrr::set_names(manifest_dirs, manifest_dirs)
}

#' Deploy a Git Repository
#'
#' `r lifecycle::badge('experimental')` Deploy a git repository directly to Posit Connect,
#' using Posit Connect's "pull-based" "git-polling" method of deployment.
#'
#' - `deploy_repo_enable()` enables (or disables) Posit Connect's git polling for a piece of content
#' - `deploy_repo_update()` triggers an update of the content from its git
#'   repository, if any are present
#'
#' @param client A Connect R6 object
#' @param repository The git repository to deploy
#' @param branch The git branch to deploy
#' @param subdirectory The subdirectory to deploy (must contain a `manifest.json`)
#' @param name The "name" / unique identifier for the content. Defaults to a random character string
#' @param title The "title" of the content
#' @param content An R6 Content object (i.e. the result of `content_item()`)
#' @param enabled Whether Connect will enable automatic polling for repository updates
#' @param ... Additional options for defining / specifying content attributes
#'
#' @return A ContentTask object, for use with `poll_task()` (if you want to follow the logs)
#'
#' @seealso connectapi::poll_task, connectapi::repo_check_branches, connectapi::repo_check_manifest_dirs
#'
#' @family content functions
#' @rdname deploy_repo
#' @export
deploy_repo <- function(
    client,
    repository,
    branch,
    subdirectory,
    name = create_random_name(),
    title = name,
    ...) {
  validate_R6_class(client, "Connect")
  warn_experimental("deploy_repo")

  content_metadata <- content_ensure(connect = client, name = name, title = title, ..., .permitted = c("new"))

  deployed_content <- content_item(client, content_metadata$guid)
  deployed_content$repo_set(repository = repository, branch = branch, subdirectory = subdirectory)

  task <- deployed_content$deploy()

  ContentTask$new(connect = client, content = content_metadata, task = task)
}

#' @rdname deploy_repo
#' @export
deploy_repo_enable <- function(content, enabled = TRUE) {
  validate_R6_class(content, "Content")
  warn_experimental("deploy_repo_enable")

  invisible(content$repo_enable(enabled))
  invisible(content$get_content_remote())
  return(content)
}

#' @rdname deploy_repo
#' @export
deploy_repo_update <- function(content) {
  validate_R6_class(content, "Content")
  warn_experimental("deploy_repo_update")
  scoped_experimental_silence()

  con <- content$get_connect()
  internal_meta <- content$internal_content()
  repo_data <- tryCatch(
    {
      internal_meta$git
    },
    error = function(e) {
      message(e)
      return(NULL)
    }
  )
  if (is.null(repo_data)) {
    stop(glue::glue("Content item '{internal_meta$guid}' is not git-backed content"))
  }
  branch_status <- repo_check_branches_ref(con, repo_data$repository_url)

  if (!repo_data$branch %in% names(branch_status)) {
    stop(glue::glue("Branch '{repo_data$branch}' was no longer found on repository '{repo_data$repository_url}'"))
  }
  if (identical(repo_data$last_known_commit, branch_status[[repo_data$branch]])) {
    message(glue::glue("No changes were found in the Git repository: {repo_data$repository_url}@{repo_data$branch}"))
    return(content)
  }
  task <- content$deploy()

  ContentTask$new(connect = con, content = content$get_content(), task = task)
}
