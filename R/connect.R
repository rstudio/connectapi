#' Class representing a Connect API client
#'
#' @name RStudioConnect
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
#' @importFrom utils capture.output
#' @export
Connect <- R6::R6Class(
  'Connect',

  public = list(
    host = NULL,
    api_key = NULL,
    tags = NULL,
    tag_map = NULL,

    get_connect = function() {self},
    
    initialize = function(host, api_key) {
      message(glue::glue("Defining Connect with host: {host}"))
      self$host = base::sub("^(.*)/$", "\\1", host)
      self$api_key = api_key
    },
    
    # helpers ----------------------------------------------------------
    
    print = function(...) {
      cat("RStudio Connect API Client: \n")
      cat("  RStudio Connect Server: ", self$host, "\n", sep = "")
      cat("  RStudio Connect API Key: ", paste0(strrep("*",11), substr(self$api_key, nchar(self$api_key)-3, nchar(self$api_key))), "\n", sep = "")
      # TODO: something about API key... role... ?
      # TODO: point to docs on methods... how to see methods?
      cat("\n")
      invisible(self)
    },

    raise_error = function(res) {
      if (httr::http_error(res)) {
        err <- sprintf('%s request failed with %s',
                       res$request$url,
                       httr::http_status(res)$message)
        message(capture.output(str(httr::content(res))))
        stop(err)
      }
    },
    
    add_auth = function() {
      httr::add_headers(Authorization = paste0('Key ', self$api_key))
    },

    GET = function(path, writer = httr::write_memory(), parser = 'parsed') {
      req <- paste0(self$host, '/__api__/', path)
      self$GET_URL(url = req, writer = writer, parser = parser)
    },
    
    GET_RESULT = function(path, writer = httr::write_memory()) {
      req <- paste0(self$host, '/__api__/', path)
      self$GET_RESULT_URL(url = req, writer = writer)
    },
    
    GET_URL = function(url, writer = httr::write_memory(), parser = 'parsed') {
      res <- self$GET_RESULT_URL(url = url, writer = writer)
      self$raise_error(res)
      httr::content(res, as = parser)
    },
    
    GET_RESULT_URL = function(url, writer = httr::write_memory()) { 
      res <- httr::GET(
        url,
        self$add_auth(),
        writer
      )
      check_debug(url, res)
      return(res)
    },
    
    PUT = function(path, body, encode = 'json') {
      req <- paste0(self$host, '/__api__/', path)
      res <- httr::PUT(
        req,
        self$add_auth(),
        body = body,
        encode = encode
      )
      self$raise_error(res)
      check_debug(req, res)
      httr::content(res, as = 'parsed')
    },

    HEAD = function(path) {
      req <- paste0(self$host, '/__api__/', path)
      res <- httr::HEAD(
        url = req,
        self$add_auth()
      )
      check_debug(req, res)
      return(res)
    },
    
    DELETE = function(path) {
      req <- paste0(self$host, '/__api__/', path)
      res <- httr::DELETE(
        url = req,
        self$add_auth()
      )
      check_debug(req, res)
      return(res)
    },
    
    POST = function(path, body, encode = 'json', prefix = "/__api__/") {
      req <- paste0(self$host, prefix, path)
      res <- httr::POST(req,
              self$add_auth(),
              body = body,
              encode = encode)
      self$raise_error(res)
      check_debug(req, res)
      httr::content(res, as = 'parsed')
    },
    
    me = function() {
      self$GET("me")
    },
    
    # tags ----------------------------------------------------------

    get_tags = function(use_cache = FALSE) {
      if (is.null(self$tags) || !use_cache) {
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
    
    get_tag_tree = function() {
      warn_experimental("get_tag_tree")
      self$GET("tag-tree")
    },
    
    create_tag = function(name, parent_id = NULL) {
      warn_experimental("create_tag")
      dat <- list(
        name = name
      )
      if (!is.null(parent_id)) {
        dat <- c(
          dat,
          parent_id = parent_id
        )
      }
      self$POST(
        "tags",
        body = dat
        )
    },
    
    # content listing ----------------------------------------------------------
    get_n_apps = function() {
      path <- 'applications'
      apps <- self$GET(path)
      apps$total
    },
    
    # filter is a named list, e.g. list(name = 'appname')
    # this function supports pages
    get_apps = function(filter = NULL, .collapse = "&", .limit = Inf, page_size = 25) {
      if (!is.null(filter)) {
        query <- paste(sapply(1:length(filter), function(i){sprintf('%s:%s',names(filter)[i],filter[[i]])}), collapse = .collapse)
        path <- paste0('applications?filter=',query)
        sep <- '&'
      } else {
        path <- 'applications'
        sep <- '?'
      }

      prg <- progress::progress_bar$new(
        format = "downloading page :current (:tick_rate/sec) :elapsedfull",
        total = NA,
        clear = FALSE
      )
      
      if (.limit < page_size) page_size <- .limit

      # handle paging
      prg$tick()
      res <- self$GET(
        sprintf(
          '%s%scount=%d',
          path, sep, page_size
          )
      )
      all <- res$applications
      all_l <- length(all)
      start <- page_size + 1
      while (length(res$applications) > 0 && all_l < .limit) {
        prg$tick()
        
        if ((.limit - all_l) < page_size) page_size <- (.limit - all_l)
        
        res <- self$GET(
          sprintf(
            '%s%scount=%d&start=%d&cont=%s',
            path, sep, page_size, start, res$continuation
            )
          )
        all <- c(all, res$applications)
        all_l <- length(all)
        start <- start + page_size
      }
      all
    },

    get_schedule = function(schedule_id) {
      path <- sprintf('schedules/%d', schedule_id)
      self$GET(path)
    },
    
    # content ----------------------------------------------------------
    
    content_create  = function(name, title = name, ...) {
      path <- sprintf('v1/experimental/content')
      other_params <- rlang::dots_list(...)
      self$POST(
        path, 
        c(
          list(name = tolower(gsub("\\s","",name)), title = title),
          other_params
        )
      )
    },

    download_bundle = function(bundle_id, to_path = tempfile()) {
      path <- glue::glue('v1/experimental/bundles/{bundle_id}/download')
      self$GET(path, httr::write_disk(to_path), "raw")
      to_path
    },

    content_upload = function(bundle_path, guid) {
      # todo : add X-Content-Checksum
      path <- glue::glue('v1/experimental/content/{guid}/upload')
      res <- self$POST(path, httr::upload_file(bundle_path), 'raw')
      return(res)
    },

    content_deploy = function(guid, bundle_id) {
      path <- sprintf('v1/experimental/content/%s/deploy', guid)
      res <- self$POST(path, list(bundle_id = as.character(bundle_id)))
      return(res)
    },
    
    content = function(guid) {
      path <- sprintf("v1/experimental/content/%s", guid)
      res <- self$GET(path)
      return(res)
    },

    task = function(task_id, first = 0, wait = 5) {
      path <- sprintf('v1/experimental/tasks/%s?first=%d&wait=%d', task_id, first, wait)
      self$GET(path)
    },
    
    set_content_tag = function(content_id, tag_id) {
      warn_experimental("set_content_tag")
      self$POST(
        path = glue::glue("applications/{content_id}/tags"),
        body = list(
          id = tag_id
        )
      )
    },
    
    # users -----------------------------------------------
    
    user = function(guid) {
      self$GET(glue::glue("v1/users/{guid}"))
    },
    
    users = function(page_number = 1, prefix = NULL, page_size=20){
      if (page_size > 500) {
        # reset page_size to avoid error
        page_size <- 500
      }
      path <- sprintf('v1/users?page_number=%d&page_size=%d', page_number, page_size)
      if (!is.null(prefix)) {
        path <- paste0(path, "&prefix=", prefix)
      }
      self$GET(path)
    },
    
    users_remote = function(prefix) {
      path <- sprintf('v1/users/remote?prefix=%s', prefix)
      self$GET(path)
    },
    
    users_create = function(
      username,
      email,
      first_name = NULL,
      last_name = NULL,
      password = NULL,
      user_must_set_password = NULL,
      user_role = NULL
      ) {
      path <- sprintf('v1/users')
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
    
    users_lock = function(user_guid) {
      path <- sprintf('v1/users/%s/lock', user_guid)
      message(path)
      self$POST(path = path,
                body = list(locked = TRUE)
                )
    },
    
    users_unlock = function(user_guid) {
      path <- sprintf('v1/users/%s/lock', user_guid)
      self$POST(
        path = path,
        body = list(locked = FALSE)
      )
    },
    
    users_update = function(user_guid, ...) {
      path <- sprintf('v1/users/%s', user_guid)
      self$PUT(
        path = path,
        body = rlang::list2(...)
      )
    },
    
    # groups -----------------------------------------------------
    
    groups = function(page_number = 1, prefix = NULL, page_size=20) {
      if (page_size > 500) {
        # reset page_size to avoid error
        page_size <- 500
      }
      path <- sprintf('v1/groups?page_number=%d&page_size=%d', page_number, page_size)
      if (!is.null(prefix)) {
        path <- paste0(path, "&prefix=", prefix)
      }
      self$GET(path)
    },
    
    group_members = function(guid) {
      path <- glue::glue("v1/groups/{guid}/members")
      self$GET(path)
    },
    
    groups_create = function(
      name
    ) {
      path <- sprintf('v1/groups')
      self$POST(
        path = path,
        body = list(name = name)
        )
    },
    
    # instrumentation --------------------------------------------
    
    inst_content_visits = function(
      content_guid = NULL, 
      min_data_version = NULL,
      from = NULL,
      to = NULL,
      limit = 20,
      previous = NULL,
      nxt = NULL,
      asc_order = TRUE
      ) {
      if (limit > 500) {
        limit <- 500
      }
      path <- glue::glue(
        "v1/instrumentation/content/visits?",
        glue::glue(
          "{safe_query(content_guid, 'content_guid=')}",
          "{safe_query(min_data_version, 'content_guid=')}",
          "{safe_query(make_timestamp(from), 'from=')}",
          "{safe_query(make_timestamp(to), 'to=')}",
          "{safe_query(limit, 'limit=')}",
          "{safe_query(previous, 'previous=')}",
          "{safe_query(nxt, 'next=')}",
          "{safe_query(asc_order, 'asc_order=')}",
          .sep = "&"
        ) %>%
          gsub("^&+","",.) %>%
          gsub("&+", "&", .)
      )
      
      self$GET(path)
    },

    inst_shiny_usage = function(
      content_guid = NULL, 
      min_data_version = NULL,
      from = NULL,
      to = NULL,
      limit = 20,
      previous = NULL,
      nxt = NULL,
      asc_order = TRUE
    ) {
      if (limit > 500) {
        limit <- 500
      }
      path <- glue::glue(
        "v1/instrumentation/shiny/usage?",
        glue::glue(
          "{safe_query(content_guid, 'content_guid=')}",
          "{safe_query(min_data_version, 'content_guid=')}",
          "{safe_query(make_timestamp(from), 'from=')}",
          "{safe_query(make_timestamp(to), 'to=')}",
          "{safe_query(limit, 'limit=')}",
          "{safe_query(previous, 'previous=')}",
          "{safe_query(nxt, 'next=')}",
          "{safe_query(asc_order, 'asc_order=')}",
          .sep = "&"
        ) %>%
          gsub("^&+","",.) %>%
          gsub("&+", "&", .)
      )
      
      self$GET(path)
    },
    
    # misc utilities --------------------------------------------
    
    docs = function(docs = "api", browse = TRUE) {
      stopifnot(docs %in% c("admin", "user", "api"))
      url <- paste0(self$host, '/__docs__/', docs)
      if (browse) utils::browseURL(url)
      return(url)
    },
    
    audit_logs = function(limit = 20L, previous = NULL, nxt = NULL, asc_order = TRUE) {
      if (limit > 500) {
        # reset limit to avoid error
        limit <- 500L
      }
      path <- glue::glue(
        "v1/audit_logs?limit={limit}",
        "{safe_query(previous, '&previous=')}",
        "{safe_query(nxt, '&next=')}",
        "&ascOrder={tolower(as.character(asc_order))}"
        )
      self$GET(
        path = path
      )
    },
    
    server_settings_r = function() {
      path <- "v1/server_settings/r"
      self$GET(
        path = path
      )
    },
    
    server_settings = function() {
      path <- "server_settings"
      self$GET(
        path = path
      )
    }
    
    # end --------------------------------------------------------
  )
)

#' Create a connection to RStudio Connect
#' 
#' Creates a connection to RStudio Connect using the hostname and an api key.
#' Validates the connection and checks that the version of the server is
#' compatible with the current version of the package.
#' 
#' @param host The URL for accessing RStudio Connect. Defaults to environment
#'   variable CONNECT_SERVER
#' @param api_key The API Key to authenticate to RStudio Connect with. Defaults
#'   to environment variable CONNECT_API_KEY
#' @param prefix The prefix used to determine environment variables
#' @return An RStudio Connect R6 object that can be passed along to methods
#' 
#' @rdname connect
#' @export
connect <- function(
  host = Sys.getenv(paste0(prefix, "_SERVER"), NA_character_),
  api_key = Sys.getenv(paste0(prefix, "_API_KEY"), NA_character_),
  prefix = "CONNECT"
) {
  if (
    prefix == "CONNECT" &&
    is.na(host) && is.na(api_key) && 
    !is.na(Sys.getenv("RSTUDIO_CONNECT_SERVER")) &&
    !is.na(Sys.getenv("RSTUDIO_CONNECT_API_KEY"))
  ) {
    stop("RSTUDIO_CONNECT_* environment variables are deprecated. Please specify CONNECT_SERVER and CONNECT_API_KEY instead")
  }
  con <- Connect$new(host = host, api_key = api_key)
  
  check_connect_license(con$host)
  
  # check Connect is accessible
  srv <- tryCatch({
    con$server_settings()
  }, 
  error = function(e){
    message(
      glue::glue("Problem talking to RStudio Connect at {host}/__api__/server_settings")
      )
    stop(e)
  })

  # validate version
  if (is.null(srv$version)) {
    message("Version information is not exposed by this RStudio Connect instance.")
  } else if (nchar(srv$version) == 0) {
    message("Version information is not exposed by this RStudio Connect instance.")
  } else {
    check_connect_version(using_version = srv$version)
  }
  
  con
}

check_debug <- function(req, res) {
  debug <- getOption('connect.debug')
  if (!is.null(debug) && debug) {
    message(req)
    message(httr::content(res, as = 'text'))
  }
}
