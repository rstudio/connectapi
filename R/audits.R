get_field <- function(apps, field, include_null = FALSE) {
  all <- lapply(apps, function(x){x[[field]]})
  empty <- sapply(all, is.null)

  if (!include_null) {
    return(all[!empty])
  }
  all
}

#' Get information on all apps for a server
#'
#' @param connect A Connect object
#'
#' @return List with application data, to be used by audit functions
#' @export
cache_apps <- function(connect){
  apps <- connect$get_apps()
  apps
}

#' Audit Vanity URLs
#'
#' @param apps App list, see `cache_apps`
#' @param server_url Base url for the Connect server
#' @param vanity Optional, see details
#'
#' @details If `vanity` is not provided, returns a list of all the vanity
#'   urls in use. If `vanity` is provided, returns whether or not
#'   `vanity` is eligible as a vanity url.
#'
#' @export
audit_vanity_urls <- function(apps, server_url, vanity = NULL) {

  # TODO: why does vanities not work?
  urls <- get_field(apps, 'url')
  content <- sapply(urls, function(u){
    sub(server_url, '', u, fixed = )
  })

  vanities <- content[!grepl('content/\\d+', content)]

  if (!is.null(vanity)) {
    return(
      ifelse(sprintf('%s/',vanity) %in% vanities, sprintf('%s Not Available', vanity), sprintf('%s Available', vanity))
    )
  }
  vanities
}


#' Audit R Versions
#'
#' @param apps App list, see `cache_apps`
#'
#' @return A plot that shows the R version used by content over time and in
#'   aggregate.
#' @export
audit_r_versions <- function(apps) {
   r_versions <- get_field(apps, 'r_version', TRUE)
   published <- get_field(apps, 'last_deployed_time', TRUE)

   # TODO: this is not pretty
   timeline <- data.frame(
     stringsAsFactors = FALSE,
     r_version = unlist(r_versions),
     published = do.call(c,unname(  # this flattens the list while preserving the date time
                     lapply(published[!sapply(r_versions, is.null)],  # filter out records w/o r version
                            function(d){lubridate::ymd_hms(d)})       # convert to date time
                     )
                  )
    )

   # histogram
   p1 <- ggplot2::ggplot(timeline) +
     ggplot2::geom_histogram(ggplot2::aes(r_version), stat = "count") +
     ggplot2::theme_minimal() +
     ggplot2::labs(
        title = "Distribution of Content by R Version",
        x = NULL,
        y = NULL
     )

   # timeline
   p2 <- ggplot2::ggplot(timeline) +
     ggplot2::geom_point(pch = 4, ggplot2::aes(x = published, color = r_version, y = r_version)) +
     ggplot2::theme_minimal() +
     ggplot2::labs(
       title = "Content by Time",
       x = 'Last Updated',
       y = NULL,
       color = ""
     )

   gridExtra::grid.arrange(p1, p2, ncol=2)

}

#' Audit Run As Settings
#'
#' @param apps App list, see `cache_apps`
#'
#' @return A data frame with the app name and the Run As user if the Run As user
#'   is not the default
#' @export
audit_runas <- function(apps) {
  name   <- get_field(apps, 'name', TRUE)
  run_as <- get_field(apps, 'run_as', TRUE)
  set <- !sapply(run_as, is.null)

  run_as <- data.frame(
    stringsAsFactors = FALSE,
    app_name = unlist(name[set]),
    run_as_user = unlist(run_as)
  )

  run_as_current <- get_field(apps, 'run_as_current_user', TRUE)
  set <- sapply(run_as_current, function(x){x == TRUE})

  if (sum(set > 0)) {
    run_as_current <- data.frame(
      stringsAsFactors = FALSE,
      app_name = unlist(name[set]),
      run_as_user = "current user"
    )
  } else {
    run_as_current <- NULL
  }


  return(
    rbind(run_as, run_as_current)
  )
}

# type can be all, logged_in, acl
#' Audit Access Controls
#'
#' @param apps App list, see `cache_apps`
#' @param type One of "all" or "logged_in". If "all", return a list of apps
#'   whose access control is set to "Everyone". If "logged_in", return a list of
#'   apps whose access control is set to "All logged in users"
#'
#' @export
audit_access_open <- function(apps, type = 'all') {
  access <- get_field(apps, 'access_type', TRUE)
  name <- get_field(apps, 'name', TRUE)
  acl_set <- !sapply(access, is.null)

  access <- data.frame(
    stringsAsFactors = FALSE,
    name = unlist(name[acl_set]),
    access = unlist(access)
  )

  return(access$name[access$access == type])

}
