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
    
    PUT = function(path, body, encode = 'json') {
      req <- paste0(self$host, '/__api__/', path)
      res <- httr::PUT(
        req,
        httr::add_headers(Authorization = paste0('Key ', self$api_key)),
        body = body,
        encode = encode
      )
      self$raise_error(res)
      check_debug(req, res)
      httr::content(res, as = 'parsed')
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

    get_n_apps = function() {
      path <- 'applications'
      apps <- self$GET(path)
      apps$total
    },

    # filter is a named list, e.g. list(name = 'appname')
    # this function supports pages
    get_apps = function(filter = NULL) {
      if (!is.null(filter)) {
        query <- paste(sapply(1:length(filter), function(i){sprintf('%s:%s',names(filter)[i],filter[[i]])}), collapse = '&')
        path <- paste0('applications?filter=',query)
        sep <- '&'
      } else {
        path <- 'applications'
        sep <- '?'
      }


      # handle paging
      res <- self$GET(path)
      all <- res$applications
      start <- 26
      while (length(res$applications) > 0) {
        res <- self$GET(sprintf('%s%sstart=%d&cont=%s',path, sep, start, res$continuation))
        for (a in res$applications) {
          all[[length(all) + 1]] <- a
        }
        start <- start + 25
      }
      all
    },

    get_schedule = function(schedule_id) {
      path <- sprintf('schedules/%d', schedule_id)
      self$GET(path)
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

    activate_bundle = function(app_guid, bundle_id) {
      path <- sprintf('experimental/content/%s/deploy', app_guid)
      res <- self$POST(path, list(bundle_id = as.character(bundle_id)))
      task_id <- res$id
      task_id
    },

    get_task = function(task_id, start = 0, wait = 5) {
      path = sprintf('experimental/tasks/%s?first=%d&wait=%d', task_id, start, wait)
      self$GET(path)
    },
    
    get_users = function(page_number = 1){
      path = sprintf('v1/users?page_number=%d', page_number)
      self$GET(path)
    },
    
    get_users_remote = function(prefix) {
      path = sprintf('v1/users/remote?prefix=%s', prefix)
      self$GET(path)
    },
    
    create_users = function(
      email, first_name, last_name,
      password, user_must_set_password, 
      user_role, username
      ) {
      path = sprintf('v1/users')
      self$POST(path = path,
                body = list(
                  email = email,
                  first_name = first_name,
                  last_name = last_name,
                  password = password,
                  user_must_set_password = user_must_set_password,
                  user_role = user_role,
                  username = username
                ))
    },
    
    lock_user = function(user_guid) {
      path = sprintf('v1/users/%s/lock', user_guid)
      message(path)
      self$POST(path = path,
                body = list(locked = TRUE)
                )
    },
    
    unlock_user = function(user_guid) {
      path = sprintf('v1/users/%s/lock', user_guid)
      self$POST(
        path = path,
        body = list(locked = FALSE)
      )
    },
    
    update_user = function(user_guid, email, ...) {
      path = sprintf('v1/users/%s', user_guid)
      self$PUT(
        path = path,
        body = c(list(email = email), rlang::dots_list(...))
      )
    },

    create_app  = function(name, title = name, ...) {
      path = sprintf('experimental/content')
      other_params <- rlang::dots_list(...)
      self$POST(
        path, 
        c(
          list(name = tolower(gsub("\\s","",name)), title = title ),
          other_params
        )
      )
    },
    
    get_docs = function(docs = "api") {
      stopifnot(docs %in% c("admin", "user", "api"))
      utils::browseURL(paste0(self$host, '/__docs__/', docs))
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

