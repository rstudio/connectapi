#' Generate a landing page for a specific RStudio Connect tag
#'
#' @param connect A Connect object. Must be a viewer of all of the content
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

  client <- connect
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

# unfortunately iframes do not seem to pass cookies cleanly...
# i can't even get this to work in the "same-origin" setup (where the iframe uses a different origin from the parent)
# https://security.stackexchange.com/questions/182518/how-to-confirm-that-an-embedded-iframe-can-read-cookies-from-parent
# https://www.html5rocks.com/en/tutorials/security/sandboxed-iframes/
# https://stackoverflow.com/questions/31184505/sandboxing-iframe-and-allow-same-origin
# https://stackoverflow.com/questions/2117248/setting-cookie-in-iframe-different-domain
# https://stackoverflow.com/questions/13432821/is-it-possible-to-add-request-headers-to-an-iframe-src-request
tag_page_iframe <- function(connect, tag) {
  client <- connect
  tag_id <- client$get_tag_id(tag)
  apps <- client$get_apps(filter = list(tag = as.character(tag_id)))
  
  if (length(apps) < 1) {
    stop(sprintf('No applications found on %s matching tag %s', server, tag))
  }
  
  # for prototyping
  if (length(apps) > 2) {
    apps[[1]]$content_group <- "Group 1"
    apps[[2]]$content_group <- "Group 1"
  }
  if (length(apps) > 5) {
    apps[[4]]$content_group <- "Group 2"
    apps[[5]]$content_group <- "Group 2"
  }

  template <- system.file('tag_page_iframe.Rmd', package = "connectapi")
  
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
