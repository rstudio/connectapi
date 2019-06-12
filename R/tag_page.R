#' Generate a landing page for a specific RStudio Connect tag
#'
#' @param connect A Connect object
#' @param tag The name of the targetted tag
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
  tag_id <- connect$get_tag_id(tag)
  
  if (length(tag_id) > 1) {
    warning(glue::glue("More than one tag found with identifier: {tag}. This could cause problems finding applications."))
  }
  apps <- connect$get_apps(filter = list(tag = as.character(tag_id)))

  if (length(apps) < 1) {
    stop(sprintf('No applications found on %s matching tag %s', connect$host, tag))
  }

  if (is.null(description)) {
    description <- sprintf('Content on %s tagged with %s', connect$host, tag)
  }

  dir <- dir.create(sprintf('./%s-screenshots', tag))
  if (!dir) {
    stop(sprintf('Error creating directory for screenshots'))
  }

  apps <- lapply(apps, function(a) {
    a$screenshot <- take_screenshot(a, tag, connect$api_key)
    a
  })

  template <- system.file('tag_page_template.Rmd', package = "connectapi")
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
  webshot::webshot(app$url,
            file = fname,
            vwidth = 800,
            vheight = 600,
            cliprect = "viewport",
            key = server_key)
  fname
}
