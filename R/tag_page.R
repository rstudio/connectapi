#' Generate a landing page for a specific RStudio Connect tag
#' 
#' \lifecycle{experimental}
#' Note that in order to show a content item in a page, you must have "Viewer"
#' priveleges to that content.
#'
#' @param connect A Connect object
#' @param tag The name of the targeted tag. NOTE: tag names are not necessarily unique
#' @param description A description of the tag, placed at the top of the landing
#'   page
#'
#' @return A list with LANDING_PAGE which is the path to the html file and APPS
#'   which is a list containing information on the apps and screenshots, useful
#'   if you'd rather use your own template
#' @export
tag_page <- function(connect,
                     tag,
                     description = NULL) {

  warn_experimental("tag_page")

  apps <- get_apps_by_tag(connect = connect, tag = tag)

  if (is.null(description)) {
    description <- sprintf('Content on %s tagged with %s', connect$host, tag)
  }

  dir <- fs::dir_create(sprintf('./%s-screenshots', tag))
  if (!fs::dir_exists(dir)) {
    stop(sprintf('Error creating directory for screenshots'))
  }

  apps <- lapply(apps, function(a) {
    a$screenshot <- take_screenshot(a, tag, connect$api_key)
    a
  })

  template <- system.file('tag_page', 'tag_page_template.Rmd', package = "connectapi")
  out_file <- sprintf('%s.html', tag)
  out_dir <- getwd()
  rmarkdown::render(template,
    output_dir = out_dir,
    output_file = out_file
  )

  list(
    LANDING_PAGE  = normalizePath(sprintf('%s/%s', out_dir, out_file)),
    APPS = apps
  )

}

take_screenshot <- function(app, tag, server_key) {
  fname <- sprintf('%s-screenshots/%s.png', tag, app$name)
  if (fs::file_exists(fname)) {
    message(glue::glue("{fname} already exists. Using cached file."))
  } else {
    webshot::webshot(app$url,
              file = fname,
              vwidth = 800,
              vheight = 600,
              cliprect = "viewport",
              key = server_key)
  }
  fname
}

#' Tag Page iframe
#' 
#' \lifecycle{experimental}
#' Unfortunately, iframes do not pass cookies as cleanly as I would like... I am having
#' trouble even getting this to work in the "same-origin" setup that _should_ work (if
#' the iframe uses a different origin from the parent)
#' 
#' @references
#' \itemize{
#'   \item{\url{https://security.stackexchange.com/questions/182518/how-to-confirm-that-an-embedded-iframe-can-read-cookies-from-parent}}
#'   \item{\url{https://www.html5rocks.com/en/tutorials/security/sandboxed-iframes/}}
#'   \item{\url{https://stackoverflow.com/questions/31184505/sandboxing-iframe-and-allow-same-origin}}
#'   \item{\url{https://stackoverflow.com/questions/2117248/setting-cookie-in-iframe-different-domain}}
#'   \item{\url{https://stackoverflow.com/questions/13432821/is-it-possible-to-add-request-headers-to-an-iframe-src-request}}
#' }
#' 
#' @param connect A Connect object
#' @param tag A tag to search for. NOTE: tag names are not necessarily unique
#' @param metadata A metadata object to be used for enriching application data
#' 
#' @examples 
#' app_metadata <- list(
#'   list(guid = "abcde-abcd-abcd-abcde", content_group = "Group 1"),
#'   list(guid = "fghij-fghi-fghi-fghij", content_group = "Group 2")
#' )
#' 
#' @keywords internal
tag_page_iframe <- function(connect, tag, metadata = NULL) {
  warn_experimental("tag_page_iframe")

  apps <- get_apps_by_tag(connect = connect, tag = tag)
 
  # set metadata (only content_group is used right now)
  apps <- purrr::reduce(metadata, apply_metadata, .init = apps)

  template <- system.file('tag_page', 'tag_page_iframe.Rmd', package = "connectapi")
 
  out_file <- sprintf('%s.html', tag)
  out_dir <- getwd()
 
  # render
  tmp_environment <- new.env()
  assign("apps", apps, tmp_environment)
  assign("tag", tag, tmp_environment)
  rmarkdown::render(template,
    output_dir = out_dir,
    output_file = out_file,
    envir = tmp_environment
  )
 
  list(
    LANDING_PAGE  = normalizePath(sprintf('%s/%s', out_dir, out_file)),
    APPS = apps
  )
}

# helper function to set app metadata (i.e. content_group)
apply_metadata <- function(all_apps, single_metadata) {
  which_match <- purrr::map_lgl(all_apps, ~ .x$guid == single_metadata$guid)
  all_apps[which_match] <- purrr::map(
    all_apps[which_match], 
    ~ purrr::list_modify(.x, !!!single_metadata)
    )
  return(all_apps)
}

get_apps_by_tag <- function(connect, tag) {
  tag_id <- connect$get_tag_id(tag)
  
  if (length(tag_id) > 1) {
    warning(glue::glue("More than one tag found with identifier: {tag}. This could cause problems finding applications."))
  }
  apps <- connect$get_apps(filter = list(tag = as.character(tag_id)))

  if (length(apps) < 1) {
    stop(sprintf('No applications found on %s matching tag %s', connect$host, tag))
  }
  
  return(apps)
}
