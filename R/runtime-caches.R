#' Get runtime caches
#'
#' View the runtime caches on a Connect server.
#' Requires Administrator privileges.
#'
#' @param client A `Connect` object.
#'
#' @returns A tibble of runtime caches on the server, showing `language`,
#' `version` and `image_name`. For Connect servers not using off-host execution,
#' `image_name` is `"Local"`.
#'
#' @examples
#' \dontrun{
#' client <- connect()
#' get_runtime_caches(client)
#' }
#'
#' @family server management functions
#' @seealso [delete_runtime_cache()]
#' @export
get_runtime_caches <- function(client) {
  validate_R6_class(client, "Connect")
  res <- client$GET(v1_url("system/caches/runtime"))
  purrr::map_dfr(res$caches, ~.x)
}

#' Delete a runtime cache
#'
#' Delete a runtime cache from a Connect server.
#' Requires Administrator privileges.
#'
#' @param client A `Connect` object.
#' @param language The language of the cache, either "R" or "Python".
#' @param version The version of the cache, e.g. "4.3.3".
#' @param image_name Optional. The name of the off-host execution image for
#'   the cache, or "Local" (the default) for native execution caches.
#' @param dry_run Optional, default `FALSE`. If true, perform a dry run of
#'   the deletion.
#'
#' @returns A `Task` object representing the deletion task. If `dry_run` is
#' `TRUE`, returns `NULL` or throws an error if the deletion would fail.
#'
#' @examples
#' \dontrun{
#' client <- connect()
#' task <- delete_runtime_cache(client, "R", "4.3.3")
#' poll_task(task)
#' }
#'
#' @family server management functions
#' @seealso [get_runtime_caches()]
#' @export
delete_runtime_cache <- function(client, language, version, image_name = "Local", dry_run = FALSE) {
  res <- client$DELETE(
    path = v1_url("system/caches/runtime"),
    body = list(
      language = language,
      version = version,
      image_name = image_name,
      dry_run = dry_run
    ),
    encode = "json",
    parser = "parsed"
  )

  if (dry_run == TRUE) {
    message(glue::glue(
      "Runtime cache deletion dry run did not encounter any errors ",
      '(language = "{language}", version = "{version}", image_name = "{image_name}")'
    ))
    return(invisible(NULL))
  }

  task_data <- client$GET(v1_url("tasks", res$task_id))
  Task$new(client, task_data)
}
