#' Class representing a Connect API client
#'
#' @name PositConnect
#'
#' @section Usage:
#' \preformatted{
#' client <- Connect$new(server = 'connect.example.com',
#'   api_key = 'mysecretkey')
#'
#' get_content(client)
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

    #' @description Initialize a new connect.
    #' @param server The base URL of your Posit Connect server.
    #' @param api_key Your Posit Connect API key.
    initialize = function(server, api_key) {
      message_if_not_testing(glue::glue(
        "Defining Connect with server: {server}"
      ))
      if (is.null(httr::parse_url(server)$scheme)) {
        stop(glue::glue(
          "ERROR: Please provide a protocol (http / https). You gave: {server}"
        ))
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
      cat(
        "  Posit Connect API Key: ",
        paste0(
          strrep("*", 11),
          substr(self$api_key, nchar(self$api_key) - 3, nchar(self$api_key))
        ),
        "\n",
        sep = ""
      )
      # TODO: something about API key... role... ?
      # TODO: point to docs on methods... how to see methods?
      cat("\n")
      invisible(self)
    },

    #' @description Raise an error when the HTTP result is an HTTP error.
    #' @param res HTTP result.
    raise_error = function(res) {
      if (httr::http_error(res)) {
        connect_error_details <- tryCatch(
          {
            cont <- httr::content(res)
            code <- sprintf("code: %d", cont$code)
            error <- sprintf("error: %s", cont$error)
            if (length(code) == 0 & length(error) == 0) {
              ""
            } else {
              paste0("(", paste0(c(code, error), collapse = ", "), ")")
            }
          },
          error = function(e) ""
        )

        err <- sprintf(
          "%s request failed with %s %s",
          res$request$url,
          httr::http_status(res)$message,
          connect_error_details
        )

        stop(err, call. = FALSE)
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

    #' @description Build a URL relative to the API root
    #' @param ... path segments
    api_url = function(...) {
      self$server_url("__api__", ...)
    },

    #' @description Build a URL relative to the server root
    #' @param ... path segments
    server_url = function(...) {
      paste(self$server, ..., sep = "/")
    },

    #' @description General wrapper around `httr` verbs
    #' @param method HTTP request method
    #' @param url URL to request
    #' @param parser How the response is parsed. If `NULL`, the `httr_response`
    #' will be returned. Otherwise, the argument is forwarded to
    #' `httr::content(res, as = parser)`.
    #' @param simplify Logical; if `TRUE`, JSON arrays of objects are
    #' simplified to data frames by jsonlite. Default `FALSE` preserves
    #' list-of-lists for compatibility with pagination helpers.
    #' @param ... Additional arguments passed to the request function
    request = function(method, url, ..., parser = "parsed", simplify = FALSE) {
      old_opt <- options(scipen = 999)
      on.exit(options(old_opt), add = TRUE)

      httr_verb <- get(method, envir = asNamespace("httr"))
      res <- rlang::exec(
        httr_verb,
        !!!c(
          list(
            url,
            self$add_auth()
          ),
          rlang::list2(...),
          self$httr_additions
        )
      )
      check_debug(res)

      if (is.null(parser)) {
        res
      } else {
        self$raise_error(res)
        if (parser != "parsed") {
          return(httr::content(res, as = parser))
        }
        if (is.null(res$content) || length(res$content) == 0) {
          return(NULL)
        }
        content_text <- httr::content(res, as = "text", encoding = "UTF-8")
        if (is.null(content_text) || nchar(content_text) == 0) {
          return(NULL)
        }
        jsonlite::fromJSON(
          content_text,
          simplifyVector = simplify,
          simplifyDataFrame = simplify
        )
      }
    },

    #' @description Perform an HTTP GET request of the named API path.
    #' @param path API path relative to the server's `/__api__` root.
    #' @param ... Arguments to `httr::GET()`
    #' @param url Target URL. Default uses `path`, but provide `url` to request
    #' a server resource that is not under `/__api__`
    #' @param parser How the response is parsed. If `NULL`, the `httr_response`
    #' will be returned. Otherwise, the argument is forwarded to
    #' `httr::content(res, as = parser)`.
    #' @param simplify Logical; if `TRUE`, JSON arrays of objects are
    #' simplified to data frames by jsonlite. Default `FALSE` preserves
    #' list-of-lists for compatibility with pagination helpers.
    GET = function(path, ..., url = self$api_url(path), parser = "parsed", simplify = FALSE) {
      self$request("GET", url, parser = parser, simplify = simplify, ...)
    },

    #' @description Perform an HTTP PUT request of the named API path.
    #' @param path API path relative to the server's `/__api__` root.
    #' @param body The HTTP payload.
    #' @param ... Arguments to `httr::PUT()`
    #' @param url Target URL. Default uses `path`, but provide `url` to request
    #' a server resource that is not under `/__api__`
    #' @param encode How the payload is encoded.
    #' @param parser How the response is parsed. If `NULL`, the `httr_response`
    #' will be returned. Otherwise, the argument is forwarded to
    #' `httr::content(res, as = parser)`.
    PUT = function(
      path,
      body = "{}",
      ...,
      url = self$api_url(path),
      encode = "json",
      parser = "parsed"
    ) {
      self$request(
        "PUT",
        url,
        parser = parser,
        body = body,
        encode = encode,
        ...
      )
    },

    #' @description Perform an HTTP HEAD request of the named API path.
    #' @param path API path relative to the server's `/__api__` root.
    #' @param ... Arguments to `httr::HEAD()`
    #' @param url Target URL. Default uses `path`, but provide `url` to request
    #' a server resource that is not under `/__api__`
    #' `httr::content(res, as = parser)`.
    HEAD = function(path, ..., url = self$api_url(path)) {
      self$request("HEAD", url, parser = NULL, ...)
    },

    #' @description Perform an HTTP DELETE request of the named API path. Returns the HTTP response object.
    #' @param path API path relative to the server's `/__api__` root.
    #' @param ... Arguments to `httr::DELETE()`
    #' @param url Target URL. Default uses `path`, but provide `url` to request
    #' a server resource that is not under `/__api__`
    #' @param parser How the response is parsed. If `NULL`, the `httr_response`
    #' will be returned. Otherwise, the argument is forwarded to
    #' `httr::content(res, as = parser)`.
    DELETE = function(path, ..., url = self$api_url(path), parser = NULL) {
      self$request("DELETE", url, parser = parser, ...)
    },

    #' @description Perform an HTTP PATCH request of the named API path.
    #' @param path API path relative to the server's `/__api__` root.
    #' @param ... Arguments to `httr::PATCH()`
    #' @param body The HTTP payload.
    #' @param url Target URL. Default uses `path`, but provide `url` to request
    #' a server resource that is not under `/__api__`
    #' @param encode How the payload is encoded.
    #' @param parser How the response is parsed. If `NULL`, the `httr_response`
    #' will be returned. Otherwise, the argument is forwarded to
    #' `httr::content(res, as = parser)`.
    PATCH = function(
      path,
      body = "{}",
      ...,
      url = self$api_url(path),
      encode = "json",
      parser = "parsed"
    ) {
      self$request(
        "PATCH",
        url,
        parser = parser,
        body = body,
        encode = encode,
        ...
      )
    },

    #' @description Perform an HTTP POST request of the named API path.
    #' @param path API path relative to the server's `/__api__` root.
    #' @param ... Arguments to `httr::POST()`
    #' @param body The HTTP payload.
    #' @param url Target URL. Default uses `path`, but provide `url` to request
    #' a server resource that is not under `/__api__`
    #' @param encode How the payload is encoded.
    #' @param parser How the response is parsed. If `NULL`, the `httr_response`
    #' will be returned. Otherwise, the argument is forwarded to
    #' `httr::content(res, as = parser)`.
    POST = function(
      path,
      body = "{}",
      ...,
      url = self$api_url(path),
      encode = "json",
      parser = "parsed"
    ) {
      self$request(
        "POST",
        url,
        parser = parser,
        body = body,
        encode = encode,
        ...
      )
    },

    #' @description Perform an HTTP GET request of the "me" server endpoint.
    me = function() {
      self$GET(unversioned_url("me"))
    },

    #' @description Return the base URL of the Connect server.
    get_dashboard_url = function() {
      self$server
    },

    # tags ----------------------------------------------------------

    #' @description Return all tags.
    #' @param use_cache Indicates that a cached set of tags is used.
    get_tags = function(use_cache = FALSE) {
      error_if_less_than(self$version, "1.8.6")
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
      error_if_less_than(self$version, "1.8.6")
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
        v1_url("tags"),
        body = dat
      )
    },

    #' @description Get a tag.
    #' @param id The tag identifier.
    tag = function(id = NULL) {
      error_if_less_than(self$version, "1.8.6")
      if (is.null(id)) {
        path <- v1_url("tags")
      } else {
        path <- v1_url("tags", id)
      }
      self$GET(path)
    },

    #' @description Delete a tag.
    #' @param id The tag identifier.
    tag_delete = function(id) {
      invisible(self$DELETE(v1_url("tags", id)))
    },

    #' @description Get a schedule.
    #' @param schedule_id The schedule identifier.
    get_schedule = function(schedule_id) {
      path <- unversioned_url("schedules", schedule_id)
      self$GET(path)
    },

    # content ----------------------------------------------------------

    #' @description Create content.
    #' @param name The content name.
    #' @param title The content title.
    #' @param ... Other content fields.
    content_create = function(name, title = name, ...) {
      path <- v1_url("content")
      other_params <- rlang::dots_list(...)

      verify_content_name(name)
      self$POST(
        path,
        body = c(
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
      self$POST(
        v1_url("content", guid, "bundles"),
        body = httr::upload_file(bundle_path),
        encode = "raw"
      )
    },

    #' @description Deploy a content bundle.
    #' @param guid The content GUID.
    #' @param bundle_id The bundle identifier.
    content_deploy = function(guid, bundle_id) {
      path <- v1_url("content", guid, "deploy")
      self$POST(path, body = list(bundle_id = as.character(bundle_id)))
    },

    #' @description Get a content item.
    #' @param guid The content GUID.
    #' @param owner_guid The target content owner.
    #' @param name The target name.
    #' @param include Additional response fields.
    content = function(
      guid = NULL,
      owner_guid = NULL,
      name = NULL,
      include = "tags,owner"
    ) {
      if (!is.null(guid)) {
        return(self$GET(
          v1_url("content", guid),
          query = list(include = include)
        ))
      }

      query <- list(
        owner_guid = owner_guid,
        name = name,
        include = include
      )
      path <- v1_url("content")
      self$GET(path, query = query)
    },

    #' @description Get a task.
    #' @param task_id The task identifier.
    #' @param first The initial status position.
    #' @param wait Maximum time to wait for update.
    task = function(task_id, first = 0, wait = 5) {
      path <- v1_url("tasks", task_id)
      self$GET(path, query = list(first = first, wait = wait))
    },

    #' @description Set a tag for a content item.
    #' @param content_id The content identifier.
    #' @param tag_id The tag identifier.
    set_content_tag = function(content_id, tag_id) {
      self$POST(
        path = v1_url("content", content_id, "tags"),
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
        path = v1_url("content", content_id, "tags", tag_id)
      ))
    },

    # users -----------------------------------------------

    #' @description Get user details.
    #' @param guid The user GUID.
    user = function(guid) {
      self$GET(v1_url("users", guid))
    },

    #' @description Get users.
    #' @param page_number The page number.
    #' @param prefix The search term.
    #' @param page_size The page size.
    #' @param user_role Filter by user role.
    #' @param account_status Filter by account status.
    users = function(
      page_number = 1,
      prefix = NULL,
      page_size = 500,
      user_role = NULL,
      account_status = NULL
    ) {
      path <- v1_url("users")
      if (!is.null(user_role)) {
        user_role <- paste(user_role, collapse = "|")
      }
      if (!is.null(account_status)) {
        account_status <- paste(account_status, collapse = "|")
      }
      query <- list(
        page_number = page_number,
        page_size = valid_page_size(page_size),
        prefix = prefix,
        user_role = user_role,
        account_status = account_status
      )
      self$GET(path, query = query)
    },

    #' @description Get remote users.
    #' @param prefix The search term.
    users_remote = function(prefix) {
      # No pagination here?
      path <- v1_url("users", "remote")
      query <- list(prefix = prefix)
      self$GET(path, query = query)
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
    users_create = function(
      username,
      email,
      first_name = NULL,
      last_name = NULL,
      password = NULL,
      user_must_set_password = NULL,
      user_role = NULL,
      unique_id = NULL
    ) {
      path <- v1_url("users")
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
      path <- v1_url("users")
      self$PUT(
        path = path,
        body = list(temp_ticket = temp_ticket)
      )
    },

    #' @description Lock a user.
    #' @param user_guid User GUID.
    users_lock = function(user_guid) {
      path <- v1_url("users", user_guid, "lock")
      message(path)
      self$POST(
        path = path,
        body = list(locked = TRUE)
      )
    },

    #' @description Unlock a user.
    #' @param user_guid User GUID.
    users_unlock = function(user_guid) {
      path <- v1_url("users", user_guid, "lock")
      self$POST(
        path = path,
        body = list(locked = FALSE)
      )
    },

    #' @description Update a user.
    #' @param user_guid User GUID.
    #' @param ... User fields.
    users_update = function(user_guid, ...) {
      path <- v1_url("users", user_guid)
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
    groups = function(page_number = 1, prefix = NULL, page_size = 500) {
      path <- v1_url("groups")
      query <- list(
        page_number = page_number,
        page_size = valid_page_size(page_size),
        prefix = prefix
      )
      self$GET(path, query = query)
    },

    #' @description Get group members.
    #' @param guid The group GUID.
    group_members = function(guid) {
      path <- v1_url("groups", guid, "members")
      self$GET(path)
    },

    #' @description Add a group member.
    #' @param group_guid The group GUID.
    #' @param user_guid The user GUID.
    group_member_add = function(group_guid, user_guid) {
      path <- v1_url("groups", group_guid, "members")
      self$POST(path, body = list(user_guid = user_guid))
    },

    #' @description Remove a group member.
    #' @param group_guid The group GUID.
    #' @param user_guid The user GUID.
    group_member_remove = function(group_guid, user_guid) {
      path <- v1_url("groups", group_guid, "members", user_guid)
      self$DELETE(path)
    },

    #' @description Create a group.
    #' @param name The group name.
    groups_create = function(name) {
      path <- v1_url("groups")
      self$POST(
        path = path,
        body = list(name = name)
      )
    },

    #' @description Create a remote group.
    #' @param temp_ticket Ticket identifying target remote group.
    groups_create_remote = function(temp_ticket) {
      path <- v1_url("groups")
      self$PUT(
        path = path,
        body = list(temp_ticket = temp_ticket)
      )
    },

    #' @description Get remote groups.
    #' @param prefix The search term.
    #' @param limit The maximal result set size.
    groups_remote = function(prefix = NULL, limit = 500) {
      path <- v1_url("groups", "remote")
      query <- list(
        limit = valid_page_size(limit),
        prefix = prefix
      )
      self$GET(path, query = query)
    },

    #' @description Get content to which a group has access
    #' @param guid The group GUID.
    group_content = function(guid) {
      path <- v1_url("experimental", "groups", guid, "content")
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
    inst_content_visits = function(
      content_guid = NULL,
      min_data_version = NULL,
      from = NULL,
      to = NULL,
      limit = 500,
      previous = NULL,
      nxt = NULL,
      asc_order = TRUE
    ) {
      path <- v1_url("instrumentation", "content", "visits")
      query <- list(
        content_guid = content_guid,
        min_data_version = min_data_version,
        from = make_timestamp(from),
        to = make_timestamp(to),
        limit = valid_page_size(limit),
        previous = previous,
        asc_order = tolower(as.character(asc_order))
      )
      if (length(content_guid)) {
        query$content_guid <- paste(content_guid, collapse = "|")
      }
      # This is funky because next is a reserved word in R
      query[["next"]] <- nxt
      self$GET(path, query = query)
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
    inst_shiny_usage = function(
      content_guid = NULL,
      min_data_version = NULL,
      from = NULL,
      to = NULL,
      limit = 500,
      previous = NULL,
      nxt = NULL,
      asc_order = TRUE
    ) {
      path <- v1_url("instrumentation", "shiny", "usage")
      query <- list(
        content_guid = content_guid,
        min_data_version = min_data_version,
        from = make_timestamp(from),
        to = make_timestamp(to),
        limit = valid_page_size(limit),
        previous = previous,
        asc_order = tolower(as.character(asc_order))
      )
      if (length(content_guid)) {
        query$content_guid <- paste(content_guid, collapse = "|")
      }
      # This is funky because next is a reserved word in R
      query[["next"]] <- nxt
      self$GET(path, query = query)
    },

    #' @description Get running processes.
    procs = function() {
      warn_experimental("procs")
      path <- unversioned_url("metrics", "procs")
      self$GET(path)
    },

    # repo ------------------------------------------------------

    #' @description Determine if Git repository is associated with authorization.
    #' @param host Repository URL.
    repo_account = function(host) {
      warn_experimental("repo_account")
      parsed_url <- httr::parse_url(host)
      if (is.null(parsed_url$scheme) || is.null(parsed_url$hostname)) {
        stop(glue::glue(
          "Scheme and hostname must be provided (i.e. 'https://github.com'). You provided '{host}'"
        ))
      }
      host <- glue::glue(parsed_url$scheme, "://", parsed_url$hostname)
      path <- unversioned_url("repo", "account")
      self$GET(path, query = list(url = host))
    },

    #' @description Get Git repository branches.
    #' @param repo Repository URL.
    repo_branches = function(repo) {
      warn_experimental("repo_branches")
      path <- unversioned_url("repo", "branches")
      self$GET(path, query = list(url = repo))
    },

    #' @description Get Git repository directories.
    #' @param repo Repository URL.
    #' @param branch Repository branch.
    repo_manifest_dirs = function(repo, branch) {
      warn_experimental("repo_manifest_dirs")
      path <- unversioned_url("repo", "manifest-dirs")
      self$GET(path, query = list(url = repo, branch = branch))
    },

    # schedule --------------------------------------------------

    #' @description Get schedules.
    #' @param start Starting time.
    #' @param end Ending time.
    #' @param detailed Indicates detailed schedule information.
    schedules = function(
      start = Sys.time(),
      end = Sys.time() + 60 * 60 * 24 * 7,
      detailed = FALSE
    ) {
      warn_experimental("schedules")
      url <- v1_url("experimental", "schedules")
      query_params <- list(
        detailed = tolower(detailed),
        start = datetime_to_rfc3339(start),
        end = datetime_to_rfc3339(end)
      )
      res <- self$GET(url, query = query_params)
      return(res[["schedules"]])
    },

    # packages --------------------------------------------------

    #' @description Get packages. This endpoint is paginated.
    #' @param name The package name to filter by.
    #' @param page_number Page number.
    #' @param page_size Page size, default 100000.
    packages = function(name = NULL, page_number = 1, page_size = 100000) {
      url <- v1_url("packages")
      query_params <- list(
        name = name,
        page_number = page_number,
        page_size = page_size
      )
      self$GET(url, query = query_params)
    },

    # misc utilities --------------------------------------------

    #' @description Get documentation.
    #' @param docs Named document.
    #' @param browse Open a browser.
    docs = function(docs = "api", browse = TRUE) {
      stopifnot(docs %in% c("admin", "user", "api"))
      url <- paste0(self$server, "/__docs__/", docs)
      if (browse) {
        utils::browseURL(url)
      }
      url
    },

    #' @description Get auditing.
    #' @param limit Result set size.
    #' @param previous Previous item.
    #' @param nxt Next item.
    #' @param asc_order Indicates ascending result order.
    audit_logs = function(
      limit = 500,
      previous = NULL,
      nxt = NULL,
      asc_order = TRUE
    ) {
      path <- v1_url("audit_logs")
      query <- list(
        limit = valid_page_size(limit),
        previous = previous,
        ascOrder = tolower(as.character(asc_order))
      )
      # This is funky because next is a reserved word in R
      query[["next"]] <- nxt
      self$GET(path = path, query = query)
    },

    #' @description Get all vanity URLs
    vanities = function() {
      path <- v1_url("vanities")
      self$GET(path = path)
    },

    #' @description Get server settings.
    server_settings = function() {
      self$GET(unversioned_url("server_settings"))
    }

    # end --------------------------------------------------------
  ),
  private = list(
    .version = NULL,
    .timezones = NULL
  ),
  active = list(
    #' @field version The server version.
    version = function() {
      if (is.null(private$.version)) {
        private$.version <- safe_server_version(self)
      }
      private$.version
    },
    #' @field timezones The server timezones.
    timezones = function() {
      if (is.null(private$.timezones)) {
        private$.timezones <- tryCatch(
          self$GET(v1_url("timezones")),
          error = function(e) {
            self$GET(unversioned_fallback_url("timezones"))
          }
        )
      }
      private$.timezones
    }
  )
)

#' Create a connection to Posit Connect
#'
#' Creates a connection to Posit Connect using the server URL and an API key.
#' Validates the connection and checks that the version of the server is
#' compatible with the current version of the package.
#'
#' When running on Connect, the client's environment will contain default
#' `CONNECT_SERVER` and `CONNECT_API_KEY` variables. The API key's permissions
#' are scoped to the publishing user's account.
#'
#' To create a client with permissions scoped to the content visitor's account,
#' call `connect()` passing a user session token from content session headers
#' to the `token` argument. To do this, you must first add a Connect API
#' integration in your published content's Access sidebar.
#'
#' @param server The URL for accessing Posit Connect. Defaults to environment
#'   variable CONNECT_SERVER
#' @param api_key The API Key to authenticate to Posit Connect with. Defaults
#'   to environment variable CONNECT_API_KEY
#' @param token Optional. A user session token. When running on a Connect server,
#'   creates a client using the content visitor's account. Running locally, the
#'   created client uses the provided API key.
#' @param token_local_testing_key Optional. Only used when not running on
#'   Connect and a `token` is provided. By default, the function returns a
#'   `Connect` object using the `api_key`. By providing a different
#'   key here you can test a visitor client with differently-scoped
#'   permissions.
#' @param audience Optional. The GUID of a Connect API integration associated with this piece of content.
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
#' client <- connect()
#'
#' # Running in Connect, create a client using the content visitor's account.
#' # This example assumes code is being executed in a Shiny app's `server`
#' # function with a `session` object available.
#' token <- session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
#' client <- connect(token = token)
#'
#' # Test locally with an API key using a different role.
#' fallback_key <- Sys.getenv("VIEWER_ROLE_API_KEY")
#' client <- connect(token = token, token_local_testing_key = fallback_key)
#' }
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#'
#' # default is to read CONNECT_SERVER and CONNECT_API_KEY environment variables
#' connect()
#'
#' @export
connect <- function(
  server = Sys.getenv("CONNECT_SERVER"),
  api_key = Sys.getenv("CONNECT_API_KEY"),
  token,
  token_local_testing_key = api_key,
  audience = NULL,
  ...,
  .check_is_fatal = TRUE
) {
  if (
    missing(token) && is.null(api_key) || is.na(api_key) || nchar(api_key) == 0
  ) {
    msg <- "Invalid (empty) API key. Please provide a valid API key"
    if (.check_is_fatal) {
      stop(glue::glue("ERROR: {msg}"))
    } else {
      message(msg)
    }
  }
  con <- Connect$new(server = server, api_key = api_key)

  if (!missing(token)) {
    error_if_less_than(con$version, "2025.01.0")
    if (on_connect() || on_connect_cloud()) {
      visitor_creds <- get_oauth_credentials(
        con,
        user_session_token = token,
        requested_token_type = "urn:posit:connect:api-key",
        audience = audience
      )
      con <- connect(server = server, api_key = visitor_creds$access_token)
    } else {
      con <- connect(server = server, api_key = token_local_testing_key)
      message(paste0(
        "Called with `token` but not running on Connect or Connect Cloud. ",
        "Continuing with fallback API key."
      ))
    }
  }

  tryCatch(
    {
      check_connect_license(con)
      warn_untested_connect(using_version = safe_server_version(con))
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

check_debug <- function(res) {
  # Check for deprecation warnings from the server.
  # You might get these if you've upgraded the Connect server but not connectapi.
  # connectapi will make the right request based on the version of the server,
  # but if you have an old version of the package, it won't know the new URL
  # to request.
  if ("X-Deprecated-Endpoint" %in% names(httr::headers(res))) {
    msg <- paste(
      res[["request"]][["url"]],
      "is deprecated and will be removed in a future version of Connect.",
      "Please upgrade `connectapi` in order to use the new APIs."
    )
    warn_once(msg, id = "X-Deprecated-Endpoint", class = "deprecatedWarning")
  }
  if (getOption("connect.debug", FALSE)) {
    message(paste(res[["request"]][["method"]], res[["request"]][["url"]]))
    message(paste("Response", res[["status_code"]]))
    message(httr::content(res, as = "text"))
  }
}
