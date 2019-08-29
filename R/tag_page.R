#' Generate a landing page for a specific RStudio Connect tag
#'
#' @param server Url for the Connect server
#' @param server_key An API key for the Connect server. To get screen shots of
#'   each app, the owner of the API key must be a viewer for each piece of
#'   content.
#' @param tag The name of the targeted tag
#' @param description A description of the tag, placed at the top of the landing
#'   page
#'
#' @return A list with LANDING_PAGE which is the path to the html file and APPS
#'   which is a list containing information on the apps and screenshots, useful
#'   if you'd rather use your own template
#' @export
tag_page <- function(server,
                     server_key,
                     tag,
                     description = NULL) {

  client <- Connect$new(server, server_key)
  tag_id <- client$get_tag_id(tag)
  apps <- client$get_apps(filter = list(tag = as.character(tag_id)))

  if (length(apps) < 1) {
    stop(sprintf('No applications found on %s matching tag %s', server, tag))
  }

  if (is.null(description)) {
    description <- sprintf('Content on %s tagged with %s', server, tag)
  }

  dir <- dir.create(sprintf('./%s-screenshots', tag))
  if (!dir) {
    stop(sprintf('Error creating directory for screenshots'))
  }

  apps <- lapply(apps, function(a) {
    a$screenshot <- take_screenshot(a, tag, server_key)
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
