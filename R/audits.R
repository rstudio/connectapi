#' Check to see if a vanity URL is currently in use
#'
#' \lifecycle{experimental}
#'
#' @param connect A Connect R6 object
#' @param vanity string of the vanity URL to check
#'
#' @return logical indicating if the vanity URL is available.
#'
#' @family audit functions
#' @export
vanity_is_available <- function(connect, vanity) {
  current_vanities <- connect$GET(v1_url("vanities"))
  current_vanity_paths <- purrr::map_chr(current_vanities, "path")

  # In case a full URL has been given, prune it down to just the path
  # and make sure it has a leading and trailing slash
  vanity <- trim_vanity(vanity, connect$server)

  !(vanity %in% current_vanity_paths)
}

trim_vanity <- function(url, server_path) {
  parsed_url <- httr::parse_url(url)
  if (nchar(server_path) > 0) {
    # remove the trailing slash
    server_path <- base::sub("^(.*)/$", "\\1", server_path)
    vanity <- sub(server_path, "", parsed_url$path)
  } else {
    vanity <- parsed_url$path
  }

  # ensure leading and trailing slash
  base::sub("^/?(.*[^/])/?$", "/\\1/", vanity)
}


#' Audit R Versions
#'
#' \lifecycle{experimental}
#'
#' @param content `data.frame` of content information, as from [get_content()]
#'
#' @return A plot that shows the R version used by content over time and in
#'   aggregate.
#' @family audit functions
#' @export
audit_r_versions <- function(content) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("gridExtra is required for this function")
  }

  timeline <- content[!is.na(content$r_version), c("r_version", "last_deployed_time")]

  # histogram
  p1 <- ggplot2::ggplot(timeline) +
    ggplot2::geom_bar(ggplot2::aes(r_version)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Distribution of Content by R Version",
      x = NULL,
      y = NULL
    )

  # timeline
  p2 <- ggplot2::ggplot(timeline) +
    ggplot2::geom_point(pch = 4, ggplot2::aes(x = last_deployed_time, color = r_version, y = r_version)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Content by Time",
      x = "Last Updated",
      y = NULL,
      color = ""
    )

  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

#' Audit Run As Settings
#'
#' \lifecycle{experimental}
#'
#' @param content `data.frame` of content information, as from [get_content()]
#'
#' @return A data frame with the app name and the Run As user if the Run As user
#'   is not the default
#' @family audit functions
#' @export
audit_runas <- function(content) {
  content$run_as <- ifelse(content$run_as_current_user, "current user", content$run_as)
  content <- content[!is.na(content$run_as), c("name", "run_as")]
  names(content) <- c("app_name", "run_as_user")
  content
}

# type can be all, logged_in, acl
#' Audit Access Controls
#'
#' \lifecycle{experimental}
#'
#' @param content `data.frame` of content information, as from [get_content()]
#' @param type One of "all" or "logged_in". If "all", return a list of apps
#'   whose access control is set to "Everyone". If "logged_in", return a list of
#'   apps whose access control is set to "All logged in users"
#'
#' @family audit functions
#' @export
audit_access_open <- function(content, type = "all") {
  content$name[content$access_type == type]
}
