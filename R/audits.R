get_field <- function(apps, field, include_null = FALSE) {
  all <- lapply(apps, function(x) {
    x[[field]]
  })
  empty <- sapply(all, is.null)

  if (!include_null) {
    return(all[!empty])
  }
  all
}

#' Get information on all apps for a server
#'
#' \lifecycle{experimental}
#'
#' @param connect A Connect object
#'
#' @return List with application data, to be used by audit functions
#' @family audit functions
#' @export
cache_apps <- function(connect) {
  apps <- connect$get_apps()
  apps
}

#' Audit Vanity URLs
#'
#' \lifecycle{experimental}
#'
#' @param apps App list, see [cache_apps()]
#' @param server_url Base url for the Connect server
#' @param vanity Optional, see details
#'
#' @details If `vanity` is not provided, returns a list of all the vanity
#'   urls in use. If `vanity` is provided, returns whether or not
#'   `vanity` is eligible as a vanity url.
#'
#' @family audit functions
#' @export
audit_vanity_urls <- function(apps, server_url, vanity = NULL) {
  # TODO: why does vanities not work?
  urls <- get_field(apps, "url")
  parse_server <- httr::parse_url(server_url)
  if (is.null(parse_server$scheme)) {
    stop(glue::glue("ERROR: protocol (i.e. http:// or https://) not defined on server_url={server_url}"))
  }
  content <- sapply(urls, function(u) {
    trim_vanity(u, parse_server$path)
  })

  vanities <- content[!grepl("content/\\d+", content)]

  if (!is.null(vanity)) {
    return(
      ifelse(sprintf("%s/", vanity) %in% vanities, sprintf("%s Not Available", vanity), sprintf("%s Available", vanity))
    )
  }
  vanities
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
