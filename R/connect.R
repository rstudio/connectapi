#' Class representing a Connect API client
#'
#' @name PositConnect
#'
#' @section Usage:
#' \preformatted{
#' client <- Connect$new(server = 'connect.example.com',
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
#'
#' @family R6 classes
#'
#' @export
Connect <- R6::R6Class(
  "Connect",
  public = list(
    #' @field server The base URL of your Posit Connect server.
    server = NULL,
    #' @field api_key Your Posit Connect API key.
    api_key = NULL,
    #' @field tags The initial set of tags.
    tags = NULL,
    #' @field tag_map The initial tag map.
    tag_map = NULL,
    #' @field httr_additions An initial set of `httr` configuration added to each HTTP call.
    httr_additions = list(),
    #' @field using_auth Indicates that the API key is added to each HTTP call.
    using_auth = TRUE,

    #' @description Return this connect.
    get_connect = function() {
      self
    },

    #' @description Initialize a new connect.
    #' @param server The base URL of your Posit Connect server.
    #' @param api_key Your Posit Connect API key.
    initialize = function(server, api_key) {
      message(glue::glue("Defining Connect with server: {server}"))
      if (is.null(httr::parse_url(server)$scheme)) {
        stop(glue::glue("ERROR: Please provide a protocol (http / https). You gave: {server}"))
      }
      self$server <- base::sub("^(.*)/$", "\\1", server)
      self$api_key <- api_key
    },

    #' @description Set additional `httr` configuration that is added to each HTTP call.
    #' @param ... Set of httr configurations.
    httr_config = function(...) {
      self$httr_additions <- rlang::list2(...)
      invisible(self)
    },

    # helpers ----------------------------------------------------------

    #' @description Print details about this instance.
    #' @param ... Ignored.
    print = function(...) {
      cat("Posit Connect API Client: \n")
      cat("  Posit Connect Server: ", self$server, "\n", sep = "")
      cat("  Posit Connect API Key: ", paste0(strrep("*", 11), substr(self$api_key, nchar(self$api_key) - 3, nchar(self$api_key))), "\n", sep = "")
      # TODO: something about API key... role... ?
      # TODO: point to docs on methods... how to see methods?
      cat("\n")
      invisible(self)
    },

    #' @description Raise an error when the HTTP result is an HTTP error.
    #' @param res HTTP result.
    raise_error = function(res) {
      if (httr::http_error(res)) {
        err <- sprintf(
          "%s request failed with %s",
          res$request$url,
          httr::http_status(res)$message
        )
        tryCatch(
          {
            message(capture.output(str(httr::content(res))))
          },
          error = function(e) {
            message(e)
          }
        )
        stop(err)
      }
    },

    #' @description Returns HTTP authorization headers, or NULL when none are used.
    add_auth = function() {
      if (self$using_auth) {
        httr::add_headers(Authorization = paste0("Key ", self$api_key))
      } else {
        NULL
      }
    },

    #' @description Perform an HTTP GET request of the named API path. Returns an object parsed from the HTTP response.
    #' @param path API path.
    #' @param writer Controls where the response is written.
    #' @param parser How the response is parsed.
    #' @param ... Arguments to the httr::GET.
    GET = function(path, writer = httr::write_memory(), parser = "parsed", ...) {
      req <- paste0(self$server, "/__api__/", path)
      self$GET_URL(url = req, writer = writer, parser = parser, ...)
    },

    #' @description Perform an HTTP GET request of the named API path. Returns the HTTP response object.
    #' @param path API path.
    #' @param writer Controls where the response is written.
    #' @param ... Arguments to the httr::GET.
    GET_RESULT = function(path, writer = httr::write_memory(), ...) {
      req <- paste0(self$server, "/__api__/", path)
      self$GET_RESULT_URL(url = req, writer = writer, ...)
    },

    #' @description Perform an HTTP GET request of the named URL. Returns an object parsed from the HTTP response.
    #' @param url Target URL.
    #' @param writer Controls where the response is written.
    #' @param parser How the response is parsed.
    #' @param ... Arguments to the httr::GET.
    GET_URL = function(url, writer = httr::write_memory(), parser = "parsed", ...) {
      res <- self$GET_RESULT_URL(url = url, writer = writer, ...)
      self$raise_error(res)
      httr::content(res, as = parser)
    },

    #' @description Perform an HTTP GET request of the named URL. Returns the HTTP response object.
    #' @param url Target URL.
    #' @param writer Controls where the response is written.
    #' @param ... Arguments to the httr::GET.
    GET_RESULT_URL = function(url, writer = httr::write_memory(), ...) {
      params <- rlang::list2(...)
      res <- rlang::exec(
        httr::GET,
        !!!c(
          list(
            url,
            self$add_auth(),
            writer
          ),
          params,
          self$httr_additions
        )
      )
      check_debug(url, res)
      return(res)
    },

    #' @description Perform an HTTP PUT request of the named API path. Returns an object parsed from the HTTP response.
    #' @param path API path.
    #' @param body The HTTP payload.
    #' @param encode How the payload is encoded.
    #' @param .empty_object Indicates that an empty JSON object is sent when the body is empty.
    #' @param ... Arguments to the httr::PUT.
    PUT = function(path, body, encode = "json", ..., .empty_object = TRUE) {
      req <- paste0(self$server, "/__api__/", path)
      params <- rlang::list2(...)
      if (length(body) == 0 && .empty_object) {
        body <- "{}"
      }
      res <- rlang::exec(
        httr::PUT,
        !!!c(
          list(
            req,
            self$add_auth(),
            body = body,
            encode = encode
          ),
          params,
          self$httr_additions
        )
      )
      self$raise_error(res)
      check_debug(req, res)
      httr::content(res, as = "parsed")
    },

    #' @description Perform an HTTP HEAD request of the named API path. Returns the HTTP response object.
    #' @param path API path.
    #' @param ... Arguments to the httr::HEAD.
    HEAD = function(path, ...) {
      req <- paste0(self$server, "/__api__/", path)
      params <- rlang::list2(...)
      res <- rlang::exec(
        httr::HEAD,
        !!!c(
          list(
            url = req,
            self$add_auth()
          ),
          params,
          self$httr_additions
        )
      )
      check_debug(req, res)
      return(res)
    },

    #' @description Perform an HTTP DELETE request of the named API path. Returns the HTTP response object.
    #' @param path API path.
    #' @param ... Arguments to the httr::DELETE.
    DELETE = function(path, ...) {
      req <- paste0(self$server, "/__api__/", path)
      params <- rlang::list2(...)
      res <- rlang::exec(
        httr::DELETE,
        !!!c(
          list(
            url = req,
            self$add_auth()
          ),
          params,
          self$httr_additions
        )
      )
      check_debug(req, res)
      return(res)
    },

    #' @description Perform an HTTP PATCH request of the named API path. Returns an object parsed from the HTTP response.
    #' @param path API path.
    #' @param prefix API path prefix.
    #' @param body The HTTP payload.
    #' @param encode How the payload is encoded.
    #' @param .empty_object Indicates that an empty JSON object is sent when the body is empty.
    #' @param ... Arguments to the httr::PATCH.
    PATCH = function(path, body, encode = "json", prefix = "/__api__/", ..., .empty_object = TRUE) {
      req <- paste0(self$server, prefix, path)
      params <- rlang::list2(...)
      if (length(body) == 0 && .empty_object) {
        body <- "{}"
      }
      res <- rlang::exec(
        httr::PATCH,
        !!!c(
          list(
            req,
            self$add_auth(),
            body = body,
            encode = encode
          ),
          params,
          self$httr_additions
        )
      )
      self$raise_error(res)
      check_debug(req, res)
      httr::content(res, as = "parsed")
    },

    #' @description Perform an HTTP POST request of the named API path. Returns an object parsed from the HTTP response.
    #' @param path API path.
    #' @param prefix API path prefix.
    #' @param body The HTTP payload.
    #' @param encode How the payload is encoded.
    #' @param .empty_object Indicates that an empty JSON object is sent when the body is empty.
    #' @param ... Arguments to the httr::POST.
    POST = function(path, body, encode = "json", prefix = "/__api__/", ..., .empty_object = TRUE) {
      req <- paste0(self$server, prefix, path)
      params <- rlang::list2(...)
      if (length(body) == 0 && .empty_object) {
        body <- "{}"
      }
      res <- rlang::exec(
        httr::POST,
        !!!c(
          list(
            req,
            self$add_auth(),
            body = body,
            encode = encode
          ),
          params,
          self$httr_additions
        )
      )
      check_debug(req, res)
      self$raise_error(res)
      httr::content(res, as = "parsed")
    },

    #' @description Perform an HTTP GET request of the "me" server endpoint.
    me = function() {
      self$GET("me")
    },

    #' @description Return the base URL of the Connect server.
    get_dashboard_url = function() {
      self$server
    },

    # tags ----------------------------------------------------------

    #' @description Return all tags.
    #' @param use_cache Indicates that a cached set of tags is used.
    get_tags = function(use_cache = FALSE) {
      error_if_less_than(self, "1.8.6")
      # TODO: check cache "age"?
      if (is.null(self$tags) || !use_cache) {
        self$tags <- self$tag()
      }
      # TODO: deprecate tag_map? by removing all of the things that use it...
      # caching is hard
      self$tag_map <- data.frame(
        id = sapply(self$tags, function(x) {
          as.character(x$id)
        }),
        name = sapply(self$tags, function(x) {
          x$name
        }),
        stringsAsFactors = FALSE
      )
      self$tag_map
    },

    #' @description Get the identifier for the named tag.
    #' @param tagname The name of the tag.
    get_tag_id = function(tagname) {
      self$get_tags()
      if (!any(self$tag_map$name == tagname)) {
        stop(sprintf("Tag %s not found on server %s", tagname, self$server))
      }
      self$tag_map[which(self$tag_map$name == tagname), "id"]
    },

    #' @description Get the tag tree.
    get_tag_tree = function() {
      raw_tags <- self$tag()
      tag_tree_parse_data(raw_tags)
    },

    #' @description Get the tag tree.
    get_tag_tree_old = function() {
      warn_experimental("get_tag_tree")
      self$GET("tag-tree")
    },

    #' @description Create a tag.
    #' @param name The tag name.
    #' @param parent_id The parent identifier.
    tag_create_safe = function(name, parent_id = NULL) {
      tt <- get_tags(self)

      tag_exists_id <- recursive_find_tag(tt, name, parent_id)
      if (is.na(tag_exists_id)) {
        self$tag_create(name, parent_id)
      } else {
        self$tag(tag_exists_id)
      }
    },

    #' @description Create a tag.
    #' @param name The tag name.
    #' @param parent_id The parent identifier.
    tag_create = function(name, parent_id = NULL) {
      error_if_less_than(self, "1.8.6")
      dat <- list(
        name = name
      )
      if (!is.null(parent_id)) {
        if (is.numeric(parent_id)) {
          warn_once("Converting `tag parent_id` to character", "tag_parent_id")
          parent_id <- as.character(parent_id)
        }
        dat <- c(
          dat,
          parent_id = parent_id
        )
      }
      self$POST(
        "v1/tags",
        body = dat
      )
    },

    #' @description Get a tag.
    #' @param id The tag identifier.
    tag = function(id = NULL) {
      error_if_less_than(self, "1.8.6")
      path <- "v1/tags"
      if (!is.null(id)) {
        path <- glue::glue("{path}/{id}")
      }
      self$GET(path)
    },

    #' @description Delete a tag.
    #' @param id The tag identifier.
    tag_delete = function(id) {
      invisible(self$DELETE(glue::glue("v1/tags/{id}")))
    },

    # content listing ----------------------------------------------------------

    #' @description Get the number of content items.
    get_n_apps = function() {
      path <- "applications"
      apps <- self$GET(path)
      apps$total
    },

    # filter is a named list, e.g. list(name = 'appname')
    # this function supports pages
    #' @description Get content items.
    #' @param filter Named list containing filter conditions.
    #' @param .collapse How multiple filters are combined.
    #' @param .limit The limit.
    #' @param page_size The page size.
    get_apps = function(filter = NULL, .collapse = "&", .limit = Inf, page_size = 25) {
      if (!is.null(filter)) {
        query <- paste(sapply(1:length(filter), function(i) {
          sprintf("%s:%s", names(filter)[i], filter[[i]])
        }), collapse = .collapse)
        path <- paste0("applications?filter=", query)
        sep <- "&"
      } else {
        path <- "applications"
        sep <- "?"
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
          "%s%scount=%d",
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
            "%s%scount=%d&start=%d&cont=%s",
            path, sep, page_size, start, res$continuation
          )
        )
        all <- c(all, res$applications)
        all_l <- length(all)
        start <- start + page_size
      }
      all
    },

    #' @description Get a schedule.
    #' @param schedule_id The schedule identifier.
    get_schedule = function(schedule_id) {
      path <- sprintf("schedules/%d", schedule_id)
      self$GET(path)
    },

    # content ----------------------------------------------------------

    #' @description Create content.
    #' @param name The content name.
    #' @param title The content title.
    #' @param ... Other content fields.
    content_create = function(name, title = name, ...) {
      path <- sprintf("v1/content")
      other_params <- rlang::dots_list(...)

      verify_content_name(name)
      self$POST(
        path,
        c(
          list(name = name, title = title),
          other_params
        )
      )
    },

    #' @description Upload a content bundle.
    #' @param bundle_path The path to the bundle archive.
    #' @param guid The content GUID.
    content_upload = function(bundle_path, guid) {
      # todo : add X-Content-Checksum
      path <- glue::glue("v1/content/{guid}/bundles")
      res <- self$POST(path, httr::upload_file(bundle_path), "raw")
      return(res)
    },

    #' @description Deploy a content bundle.
    #' @param guid The content GUID.
    #' @param bundle_id The bundle identifier.
    content_deploy = function(guid, bundle_id) {
      path <- sprintf("v1/content/%s/deploy", guid)
      res <- self$POST(path, list(bundle_id = as.character(bundle_id)))
      return(res)
    },

    #' @description Get a content item.
    #' @param guid The content GUID.
    #' @param owner_guid The target content owner.
    #' @param name The target name.
    #' @param include Additional response fields.
    content = function(guid = NULL, owner_guid = NULL, name = NULL, include = "tags,owner") {
      if (!is.null(guid)) {
        path <- glue::glue("v1/content/{guid}")
      } else {
        filter_args <- list(owner_guid = owner_guid, name = name, include = include)
        path <- glue::glue(
          "v1/content{query_args(!!!filter_args)}"
        )
      }
      res <- self$GET(path)
      return(res)
    },

    #' @description Get a task.
    #' @param task_id The task identifier.
    #' @param first The initial status position.
    #' @param wait Maximum time to wait for update.
    task = function(task_id, first = 0, wait = 5) {
      path <- sprintf("v1/tasks/%s?first=%d&wait=%d", task_id, first, wait)
      self$GET(path)
    },

    #' @description Set a tag for a content item.
    #' @param content_id The content identifier.
    #' @param tag_id The tag identifier.
    set_content_tag = function(content_id, tag_id) {
      self$POST(
        path = glue::glue("v1/content/{content_id}/tags"),
        body = list(
          tag_id = as.character(tag_id)
        )
      )
    },

    #' @description Remove a tag from a content item.
    #' @param content_id The content identifier.
    #' @param tag_id The tag identifier.
    remove_content_tag = function(content_id, tag_id) {
      invisible(self$DELETE(
        path = glue::glue("v1/content/{content_id}/tags/{tag_id}")
      ))
    },

    # users -----------------------------------------------

    #' @description Get user details.
    #' @param guid The user GUID.
    user = function(guid) {
      self$GET(glue::glue("v1/users/{guid}"))
    },

    #' @description Get users.
    #' @param page_number The page number.
    #' @param prefix The search term.
    #' @param page_size The page size.
    users = function(page_number = 1, prefix = NULL, page_size = 20) {
      if (page_size > 500) {
        # reset page_size to avoid error
        page_size <- 500
      }
      path <- sprintf("v1/users?page_number=%d&page_size=%d", page_number, page_size)
      if (!is.null(prefix)) {
        path <- paste0(path, "&prefix=", prefix)
      }
      self$GET(path)
    },

    #' @description Get remote users.
    #' @param prefix The search term.
    users_remote = function(prefix) {
      path <- sprintf("v1/users/remote?prefix=%s", prefix)
      self$GET(path)
    },

    #' @description Create a user.
    #' @param username The username.
    #' @param email Email address.
    #' @param first_name First name.
    #' @param last_name Last name.
    #' @param password The password.
    #' @param user_must_set_password Indicates that user sets password on first login.
    #' @param user_role Role for user.
    #' @param unique_id Identifier for user.
    users_create = function(username,
                            email,
                            first_name = NULL,
                            last_name = NULL,
                            password = NULL,
                            user_must_set_password = NULL,
                            user_role = NULL,
                            unique_id = NULL) {
      path <- sprintf("v1/users")
      self$POST(
        path = path,
        body = list(
          email = email,
          first_name = first_name,
          last_name = last_name,
          password = password,
          user_must_set_password = user_must_set_password,
          user_role = user_role,
          username = username,
          unique_id = unique_id
        )
      )
    },

    #' @description Create a remote user.
    #' @param temp_ticket Ticket identifying target remote user.
    users_create_remote = function(temp_ticket) {
      path <- "v1/users"
      self$PUT(
        path = path,
        body = list(temp_ticket = temp_ticket)
      )
    },

    #' @description Lock a user.
    #' @param user_guid User GUID.
    users_lock = function(user_guid) {
      path <- sprintf("v1/users/%s/lock", user_guid)
      message(path)
      self$POST(
        path = path,
        body = list(locked = TRUE)
      )
    },

    #' @description Unlock a user.
    #' @param user_guid User GUID.
    users_unlock = function(user_guid) {
      path <- sprintf("v1/users/%s/lock", user_guid)
      self$POST(
        path = path,
        body = list(locked = FALSE)
      )
    },

    #' @description Update a user.
    #' @param user_guid User GUID.
    #' @param ... User fields.
    users_update = function(user_guid, ...) {
      path <- sprintf("v1/users/%s", user_guid)
      self$PUT(
        path = path,
        body = rlang::list2(...)
      )
    },

    # groups -----------------------------------------------------

    #' @description Get groups.
    #' @param page_number The page number.
    #' @param prefix The search term.
    #' @param page_size The page size.
    groups = function(page_number = 1, prefix = NULL, page_size = 20) {
      if (page_size > 500) {
        # reset page_size to avoid error
        page_size <- 500
      }
      path <- sprintf("v1/groups?page_number=%d&page_size=%d", page_number, page_size)
      if (!is.null(prefix)) {
        path <- paste0(path, "&prefix=", prefix)
      }
      self$GET(path)
    },

    #' @description Get group members.
    #' @param guid The group GUID.
    group_members = function(guid) {
      path <- glue::glue("v1/groups/{guid}/members")
      self$GET(path)
    },

    #' @description Add a group member.
    #' @param group_guid The group GUID.
    #' @param user_guid The user GUID.
    group_member_add = function(group_guid, user_guid) {
      path <- glue::glue("v1/groups/{group_guid}/members")
      self$POST(path, list(user_guid = user_guid))
    },

    #' @description Remove a group member.
    #' @param group_guid The group GUID.
    #' @param user_guid The user GUID.
    group_member_remove = function(group_guid, user_guid) {
      path <- glue::glue("v1/groups/{group_guid}/members/{user_guid}")
      self$DELETE(path)
    },

    #' @description Create a group.
    #' @param name The group name.
    groups_create = function(name) {
      path <- sprintf("v1/groups")
      self$POST(
        path = path,
        body = list(name = name)
      )
    },

    #' @description Create a remote group.
    #' @param temp_ticket Ticket identifying target remote group.
    groups_create_remote = function(temp_ticket) {
      path <- "v1/groups"
      self$PUT(
        path = path,
        body = list(temp_ticket = temp_ticket)
      )
    },

    #' @description Get remote groups.
    #' @param prefix The search term.
    #' @param limit The maximal result set size.
    groups_remote = function(prefix = NULL, limit = 20) {
      if (limit > 500) {
        # reset limit to avoid error
        limit <- 500
      }
      path <- glue::glue(
        "v1/groups/remote?",
        glue::glue(
          safe_query(prefix, "prefix="),
          safe_query(limit, "limit="),
          .sep = "&"
        ) %>%
          gsub("^&+", "", .) %>%
          gsub("&+", "&", .)
      )

      self$GET(path)
    },

    # instrumentation --------------------------------------------

    #' @description Get (non-interactive) content visits.
    #' @param content_guid Content GUID.
    #' @param min_data_version Data version for request.
    #' @param from Start of range.
    #' @param to End of range.
    #' @param limit Result set size.
    #' @param previous Previous item.
    #' @param nxt Next item.
    #' @param asc_order Indicates ascending result order.
    inst_content_visits = function(content_guid = NULL,
                                   min_data_version = NULL,
                                   from = NULL,
                                   to = NULL,
                                   limit = 20,
                                   previous = NULL,
                                   nxt = NULL,
                                   asc_order = TRUE) {
      if (limit > 500) {
        limit <- 500
      }
      path <- glue::glue(
        "v1/instrumentation/content/visits?",
        glue::glue(
          "{safe_query(content_guid, 'content_guid=')}",
          "{safe_query(min_data_version, 'min_data_version=')}",
          "{safe_query(make_timestamp(from), 'from=')}",
          "{safe_query(make_timestamp(to), 'to=')}",
          "{safe_query(limit, 'limit=')}",
          "{safe_query(previous, 'previous=')}",
          "{safe_query(nxt, 'next=')}",
          "{safe_query(asc_order, 'asc_order=')}",
          .sep = "&"
        ) %>%
          gsub("^&+", "", .) %>%
          gsub("&+", "&", .)
      )

      self$GET(path)
    },

    #' @description Get interactive content visits.
    #' @description Get (non-interactive) content visits.
    #' @param content_guid Content GUID.
    #' @param min_data_version Data version for request.
    #' @param from Start of range.
    #' @param to End of range.
    #' @param limit Result set size.
    #' @param previous Previous item.
    #' @param nxt Next item.
    #' @param asc_order Indicates ascending result order.
    inst_shiny_usage = function(content_guid = NULL,
                                min_data_version = NULL,
                                from = NULL,
                                to = NULL,
                                limit = 20,
                                previous = NULL,
                                nxt = NULL,
                                asc_order = TRUE) {
      if (limit > 500) {
        limit <- 500
      }
      path <- glue::glue(
        "v1/instrumentation/shiny/usage?",
        glue::glue(
          "{safe_query(content_guid, 'content_guid=')}",
          "{safe_query(min_data_version, 'min_data_version=')}",
          "{safe_query(make_timestamp(from), 'from=')}",
          "{safe_query(make_timestamp(to), 'to=')}",
          "{safe_query(limit, 'limit=')}",
          "{safe_query(previous, 'previous=')}",
          "{safe_query(nxt, 'next=')}",
          "{safe_query(asc_order, 'asc_order=')}",
          .sep = "&"
        ) %>%
          gsub("^&+", "", .) %>%
          gsub("&+", "&", .)
      )

      self$GET(path)
    },

    #' @description Get running processes.
    procs = function() {
      warn_experimental("procs")
      path <- "metrics/procs"
      self$GET(path)
    },

    # repo ------------------------------------------------------

    #' @description Determine if Git repository is associated with authorization.
    #' @param host Repository URL.
    repo_account = function(host) {
      warn_experimental("repo_account")
      parsed_url <- httr::parse_url(host)
      if (is.null(parsed_url$scheme) || is.null(parsed_url$hostname)) {
        stop(glue::glue("Scheme and hostname must be provided (i.e. 'https://github.com'). You provided '{host}'"))
      }
      host <- glue::glue(parsed_url$scheme, "://", parsed_url$hostname)
      self$GET(glue::glue("repo/account?url={host}"))
    },

    #' @description Get Git repository branches.
    #' @param repo Repository URL.
    repo_branches = function(repo) {
      warn_experimental("repo_branches")
      self$GET(glue::glue("repo/branches?url={repo}"))
    },

    #' @description Get Git repository directories.
    #' @param repo Repository URL.
    #' @param branch Repository branch.
    repo_manifest_dirs = function(repo, branch) {
      warn_experimental("repo_manifest_dirs")
      self$GET(glue::glue("repo/manifest-dirs?url={repo}&branch={branch}"))
    },

    # schedule --------------------------------------------------

    #' @description Get schedules.
    #' @param start Starting time.
    #' @param end Ending time.
    #' @param detailed Indicates detailed schedule information.
    schedules = function(start = Sys.time(), end = Sys.time() + 60 * 60 * 24 * 7, detailed = FALSE) {
      warn_experimental("schedules")
      url <- "v1/experimental/schedules"
      query_params <- rlang::list2(
        detailed = tolower(detailed),
        start = datetime_to_rfc3339(start),
        end = datetime_to_rfc3339(end)
      )
      res <- self$GET(url, query = query_params)
      return(res[["schedules"]])
    },

    # misc utilities --------------------------------------------

    #' @description Get documentation.
    #' @param docs Named document.
    #' @param browse Open a browser.
    docs = function(docs = "api", browse = TRUE) {
      stopifnot(docs %in% c("admin", "user", "api"))
      url <- paste0(self$server, "/__docs__/", docs)
      if (browse) utils::browseURL(url)
      return(url)
    },

    #' @description Get auditing.
    #' @param limit Result set size.
    #' @param previous Previous item.
    #' @param nxt Next item.
    #' @param asc_order Indicates ascending result order.
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

    #' @description Get R installations.
    server_settings_r = function() {
      path <- "v1/server_settings/r"
      self$GET(
        path = path
      )
    },

    #' @description Get server settings.
    server_settings = function() {
      path <- "server_settings"
      self$GET(
        path = path
      )
    }

    # end --------------------------------------------------------
  )
)

#' Create a connection to Posit Connect
#'
#' Creates a connection to Posit Connect using the server URL and an api key.
#' Validates the connection and checks that the version of the server is
#' compatible with the current version of the package.
#'
#' @param server The URL for accessing Posit Connect. Defaults to environment
#'   variable CONNECT_SERVER
#' @param api_key The API Key to authenticate to Posit Connect with. Defaults
#'   to environment variable CONNECT_API_KEY
#' @param prefix The prefix used to determine environment variables
#' @param ... Additional arguments. Not used at present
#' @param .check_is_fatal Whether to fail if "check" requests fail. Useful in
#'   rare cases where more http request customization is needed for requests to
#'   succeed.
#' @return A Posit Connect R6 object that can be passed along to methods
#'
#' @rdname connect
#'
#' @examples
#' \dontrun{
#' connect()
#' }
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#'
#' # default is to read CONNECT_SERVER and CONNECT_API_KEY environment variables
#' # this example will read TEST_1_SERVER and TEST_1_API_KEY
#' connect(prefix = "TEST_1")
#'
#' @export
connect <- function(
    server = Sys.getenv(paste0(prefix, "_SERVER"), NA_character_),
    api_key = Sys.getenv(paste0(prefix, "_API_KEY"), NA_character_),
    prefix = "CONNECT",
    ...,
    .check_is_fatal = TRUE) {
  if (
    prefix == "CONNECT" &&
      is.na(server) && is.na(api_key) &&
      nchar(Sys.getenv("RSTUDIO_CONNECT_SERVER")) > 0 &&
      nchar(Sys.getenv("RSTUDIO_CONNECT_API_KEY")) > 0
  ) {
    stop("RSTUDIO_CONNECT_* environment variables are deprecated. Please specify CONNECT_SERVER and CONNECT_API_KEY instead")
  }

  params <- rlang::list2(...)
  if ("host" %in% names(params)) {
    lifecycle::deprecate_warn("0.1.0.9026", "connect(host)", "connect(server)")
    server <- params[["host"]]
  }

  if (is.null(api_key) || is.na(api_key) || nchar(api_key) == 0) {
    msg <- "Invalid (empty) API key. Please provide a valid API key"
    if (.check_is_fatal) {
      stop(glue::glue("ERROR: {msg}"))
    } else {
      message(msg)
    }
  }
  con <- Connect$new(server = server, api_key = api_key)

  tryCatch(
    {
      check_connect_license(con)
      check_connect_version(using_version = safe_server_version(con))
    },
    error = function(err) {
      if (.check_is_fatal) {
        stop(err)
      } else {
        if (inherits(err, "error")) {
          message(err$message)
        } else {
          message(err)
        }
      }
    }
  )

  con
}

check_debug <- function(req, res) {
  debug <- getOption("connect.debug")
  if (!is.null(debug) && debug) {
    message(paste(res[["request"]][["method"]], res[["request"]][["url"]]))
    message(paste("Response", res[["status_code"]]))
    message(httr::content(res, as = "text"))
  }
}
