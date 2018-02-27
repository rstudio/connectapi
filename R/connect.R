#'
#' Class representing a Connect API client
#'
#' @name Connect
#'
#' @section Usage:
#' \preformatted{
#' client <- Connect$new(host = 'connect.example.com',
#'   apiKey = 'mysecretkey')
#' client$get_apps()
#' client$get_tags()
#' }
#'
#' @section Details:
#'
#' This class allows a user to interact with a Connect server via the Connect
#' API. Authentication is done by providing an API key.
#'
NULL

#' @export

Connect <- R6::R6Class(
  'Connect',

  public = list(
    host = NULL,
    api_key = NULL,
    tags = NULL,
    tag_map = NULL,

    initialize = function(host = NA, api_key = NA) {
      self$host = host
      self$api_key = api_key
    },

    raise_error = function(res) {
      if (httr::http_error(res)) {
        err <- sprintf('%s request failed with %s',
                       res$request$url,
                       httr::http_status(res)$message)
        stop(err)
      }
    },

    GET = function(path, writer = httr::write_memory(), parser = 'parsed') {
      req <- paste0(self$host, '/__api__/', path)
      res <- httr::GET(req,
               httr::add_headers(Authorization = paste0('Key ', self$api_key)),
               writer)
      self$raise_error(res)
      check_debug(req, res)
      httr::content(res, as = parser)
    },

    POST = function(path, body, encode = 'json') {
      req <- paste0(self$host, '/__api__/', path)
      res <- httr::POST(req,
              httr::add_headers(Authorization = paste0('Key ', self$api_key)),
              body = body,
              encode = encode)
      self$raise_error(res)
      check_debug(req, res)
      httr::content(res, as = 'parsed')
    },

    get_tags = function() {
      if (is.null(self$tags)) {
          self$tags <- self$GET('/tags')
      }
      self$tag_map <- data.frame(
        id = sapply(self$tags, function(x){as.numeric(x$id)}),
        name = sapply(self$tags, function(x){x$name}),
        stringsAsFactors = FALSE
      )
      self$tag_map
    },

    get_tag_id = function(tagname) {
      self$get_tags()
      if (!any(self$tag_map$name == tagname))
        stop(sprintf('Tag %s not found on server %s', tagname, self$host))
      self$tag_map[which(self$tag_map$name == tagname), 'id']
    },

    # filter is a named list, e.g. list(name = 'appname')
    get_apps = function(filter = NULL) {
      if (!is.null(filter)) {
        query <- paste(sapply(1:length(filter), function(i){sprintf('%s:%s',names(filter)[i],filter[[i]])}), collapse = '&')
        path <- paste0('applications?filter=',query)
      } else {
        path <- 'applications'
      }
      self$GET(path)$applications
    },

    download_bundle = function(bundle_id, to_path = tempfile()) {
      path <- sprintf('bundles/%d/download', bundle_id)
      self$GET(path, httr::write_disk(to_path), "raw")
      to_path
    },

    upload_bundle = function(bundle_path, app_id) {
      path <- sprintf('applications/%d/upload', app_id)
      res <- self$POST(path, httr::upload_file(bundle_path), 'raw')
      new_bundle_id <- res$id
      new_bundle_id
    },

    activate_bundle = function(app_id, bundle_id) {
      path <- sprintf('applications/%d/deploy', app_id)
      res <- self$POST(path, list(bundle = bundle_id))
      task_id <- res$id
      task_id
    },

    get_task = function(task_id, start = 0) {
      path = sprintf('tasks/%s?first_status=%d', task_id, start)
      self$GET(path)
    },

    create_app  = function(name) {
      path = sprintf('applications')
      self$POST(path, list(name = name))
    }
  )
)

check_debug <- function(req, res) {
  debug <- getOption('connect.debug')
  if (!is.null(debug) && debug) {
    message(req)
    message(httr::content(res, as = 'text'))
  }
}

