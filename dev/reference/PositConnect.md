# Class representing a Connect API client

Class representing a Connect API client

## Usage


    client <- Connect$new(server = 'connect.example.com',
      api_key = 'mysecretkey')

    get_content(client)
    client$get_tags()

## Details

This class allows a user to interact with a Connect server via the
Connect API. Authentication is done by providing an API key.

## See also

Other R6 classes:
[`Bundle`](https://posit-dev.github.io/connectapi/dev/reference/Bundle.md),
[`Content`](https://posit-dev.github.io/connectapi/dev/reference/Content.md),
[`ContentTask`](https://posit-dev.github.io/connectapi/dev/reference/ContentTask.md),
[`Environment`](https://posit-dev.github.io/connectapi/dev/reference/EnvironmentR6.md),
[`Task`](https://posit-dev.github.io/connectapi/dev/reference/Task.md),
[`Vanity`](https://posit-dev.github.io/connectapi/dev/reference/Vanity.md),
[`Variant`](https://posit-dev.github.io/connectapi/dev/reference/VariantR6.md),
[`VariantSchedule`](https://posit-dev.github.io/connectapi/dev/reference/VariantSchedule.md),
[`VariantTask`](https://posit-dev.github.io/connectapi/dev/reference/VariantTask.md)

## Public fields

- `server`:

  The base URL of your Posit Connect server.

- `api_key`:

  Your Posit Connect API key.

- `tags`:

  The initial set of tags.

- `tag_map`:

  The initial tag map.

- `httr_additions`:

  An initial set of `httr` configuration added to each HTTP call.

- `using_auth`:

  Indicates that the API key is added to each HTTP call.

## Active bindings

- `version`:

  The server version.

- `timezones`:

  The server timezones.

## Methods

### Public methods

- [`Connect$new()`](#method-Connect-initialize)

- [`Connect$httr_config()`](#method-Connect-httr_config)

- [`Connect$print()`](#method-Connect-print)

- [`Connect$raise_error()`](#method-Connect-raise_error)

- [`Connect$add_auth()`](#method-Connect-add_auth)

- [`Connect$api_url()`](#method-Connect-api_url)

- [`Connect$server_url()`](#method-Connect-server_url)

- [`Connect$request()`](#method-Connect-request)

- [`Connect$GET()`](#method-Connect-GET)

- [`Connect$PUT()`](#method-Connect-PUT)

- [`Connect$HEAD()`](#method-Connect-HEAD)

- [`Connect$DELETE()`](#method-Connect-DELETE)

- [`Connect$PATCH()`](#method-Connect-PATCH)

- [`Connect$POST()`](#method-Connect-POST)

- [`Connect$me()`](#method-Connect-me)

- [`Connect$get_dashboard_url()`](#method-Connect-get_dashboard_url)

- [`Connect$get_tags()`](#method-Connect-get_tags)

- [`Connect$get_tag_id()`](#method-Connect-get_tag_id)

- [`Connect$get_tag_tree()`](#method-Connect-get_tag_tree)

- [`Connect$tag_create_safe()`](#method-Connect-tag_create_safe)

- [`Connect$tag_create()`](#method-Connect-tag_create)

- [`Connect$tag()`](#method-Connect-tag)

- [`Connect$tag_delete()`](#method-Connect-tag_delete)

- [`Connect$get_schedule()`](#method-Connect-get_schedule)

- [`Connect$content_create()`](#method-Connect-content_create)

- [`Connect$content_upload()`](#method-Connect-content_upload)

- [`Connect$content_deploy()`](#method-Connect-content_deploy)

- [`Connect$content()`](#method-Connect-content)

- [`Connect$task()`](#method-Connect-task)

- [`Connect$set_content_tag()`](#method-Connect-set_content_tag)

- [`Connect$remove_content_tag()`](#method-Connect-remove_content_tag)

- [`Connect$user()`](#method-Connect-user)

- [`Connect$users()`](#method-Connect-users)

- [`Connect$users_remote()`](#method-Connect-users_remote)

- [`Connect$users_create()`](#method-Connect-users_create)

- [`Connect$users_create_remote()`](#method-Connect-users_create_remote)

- [`Connect$users_lock()`](#method-Connect-users_lock)

- [`Connect$users_unlock()`](#method-Connect-users_unlock)

- [`Connect$users_update()`](#method-Connect-users_update)

- [`Connect$groups()`](#method-Connect-groups)

- [`Connect$group_members()`](#method-Connect-group_members)

- [`Connect$group_member_add()`](#method-Connect-group_member_add)

- [`Connect$group_member_remove()`](#method-Connect-group_member_remove)

- [`Connect$groups_create()`](#method-Connect-groups_create)

- [`Connect$groups_create_remote()`](#method-Connect-groups_create_remote)

- [`Connect$groups_remote()`](#method-Connect-groups_remote)

- [`Connect$group_content()`](#method-Connect-group_content)

- [`Connect$inst_content_visits()`](#method-Connect-inst_content_visits)

- [`Connect$inst_shiny_usage()`](#method-Connect-inst_shiny_usage)

- [`Connect$procs()`](#method-Connect-procs)

- [`Connect$repo_account()`](#method-Connect-repo_account)

- [`Connect$repo_branches()`](#method-Connect-repo_branches)

- [`Connect$repo_manifest_dirs()`](#method-Connect-repo_manifest_dirs)

- [`Connect$schedules()`](#method-Connect-schedules)

- [`Connect$packages()`](#method-Connect-packages)

- [`Connect$docs()`](#method-Connect-docs)

- [`Connect$audit_logs()`](#method-Connect-audit_logs)

- [`Connect$vanities()`](#method-Connect-vanities)

- [`Connect$server_settings()`](#method-Connect-server_settings)

- [`Connect$clone()`](#method-Connect-clone)

------------------------------------------------------------------------

### `Connect$new()`

Initialize a new connect.

#### Usage

    Connect$new(server, api_key)

#### Arguments

- `server`:

  The base URL of your Posit Connect server.

- `api_key`:

  Your Posit Connect API key.

------------------------------------------------------------------------

### `Connect$httr_config()`

Set additional `httr` configuration that is added to each HTTP call.

#### Usage

    Connect$httr_config(...)

#### Arguments

- `...`:

  Set of httr configurations.

------------------------------------------------------------------------

### `Connect$print()`

Print details about this instance.

#### Usage

    Connect$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `Connect$raise_error()`

Raise an error when the HTTP result is an HTTP error.

#### Usage

    Connect$raise_error(res)

#### Arguments

- `res`:

  HTTP result.

------------------------------------------------------------------------

### `Connect$add_auth()`

Returns HTTP authorization headers, or NULL when none are used.

#### Usage

    Connect$add_auth()

------------------------------------------------------------------------

### `Connect$api_url()`

Build a URL relative to the API root

#### Usage

    Connect$api_url(...)

#### Arguments

- `...`:

  path segments

------------------------------------------------------------------------

### `Connect$server_url()`

Build a URL relative to the server root

#### Usage

    Connect$server_url(...)

#### Arguments

- `...`:

  path segments

------------------------------------------------------------------------

### `Connect$request()`

General wrapper around `httr` verbs

#### Usage

    Connect$request(method, url, ..., parser = "parsed", simplify = FALSE)

#### Arguments

- `method`:

  HTTP request method

- `url`:

  URL to request

- `...`:

  Additional arguments passed to the request function

- `parser`:

  How the response is parsed. If `NULL`, the `httr_response` will be
  returned. Otherwise, the argument is forwarded to
  `httr::content(res, as = parser)`.

- `simplify`:

  Logical; if `TRUE`, JSON arrays of objects are simplified to data
  frames by jsonlite. Default `FALSE` preserves list-of-lists for
  compatibility with pagination helpers.

------------------------------------------------------------------------

### `Connect$GET()`

Perform an HTTP GET request of the named API path.

#### Usage

    Connect$GET(
      path,
      ...,
      url = self$api_url(path),
      parser = "parsed",
      simplify = FALSE
    )

#### Arguments

- `path`:

  API path relative to the server's `/__api__` root.

- `...`:

  Arguments to
  [`httr::GET()`](https://httr.r-lib.org/reference/GET.html)

- `url`:

  Target URL. Default uses `path`, but provide `url` to request a server
  resource that is not under `/__api__`

- `parser`:

  How the response is parsed. If `NULL`, the `httr_response` will be
  returned. Otherwise, the argument is forwarded to
  `httr::content(res, as = parser)`.

- `simplify`:

  Logical; if `TRUE`, JSON arrays of objects are simplified to data
  frames by jsonlite. Default `FALSE` preserves list-of-lists for
  compatibility with pagination helpers.

------------------------------------------------------------------------

### `Connect$PUT()`

Perform an HTTP PUT request of the named API path.

#### Usage

    Connect$PUT(
      path,
      body = "{}",
      ...,
      url = self$api_url(path),
      encode = "json",
      parser = "parsed"
    )

#### Arguments

- `path`:

  API path relative to the server's `/__api__` root.

- `body`:

  The HTTP payload.

- `...`:

  Arguments to
  [`httr::PUT()`](https://httr.r-lib.org/reference/PUT.html)

- `url`:

  Target URL. Default uses `path`, but provide `url` to request a server
  resource that is not under `/__api__`

- `encode`:

  How the payload is encoded.

- `parser`:

  How the response is parsed. If `NULL`, the `httr_response` will be
  returned. Otherwise, the argument is forwarded to
  `httr::content(res, as = parser)`.

------------------------------------------------------------------------

### `Connect$HEAD()`

Perform an HTTP HEAD request of the named API path.

#### Usage

    Connect$HEAD(path, ..., url = self$api_url(path))

#### Arguments

- `path`:

  API path relative to the server's `/__api__` root.

- `...`:

  Arguments to
  [`httr::HEAD()`](https://httr.r-lib.org/reference/HEAD.html)

- `url`:

  Target URL. Default uses `path`, but provide `url` to request a server
  resource that is not under `/__api__`
  `httr::content(res, as = parser)`.

------------------------------------------------------------------------

### `Connect$DELETE()`

Perform an HTTP DELETE request of the named API path. Returns the HTTP
response object.

#### Usage

    Connect$DELETE(path, ..., url = self$api_url(path), parser = NULL)

#### Arguments

- `path`:

  API path relative to the server's `/__api__` root.

- `...`:

  Arguments to
  [`httr::DELETE()`](https://httr.r-lib.org/reference/DELETE.html)

- `url`:

  Target URL. Default uses `path`, but provide `url` to request a server
  resource that is not under `/__api__`

- `parser`:

  How the response is parsed. If `NULL`, the `httr_response` will be
  returned. Otherwise, the argument is forwarded to
  `httr::content(res, as = parser)`.

------------------------------------------------------------------------

### `Connect$PATCH()`

Perform an HTTP PATCH request of the named API path.

#### Usage

    Connect$PATCH(
      path,
      body = "{}",
      ...,
      url = self$api_url(path),
      encode = "json",
      parser = "parsed"
    )

#### Arguments

- `path`:

  API path relative to the server's `/__api__` root.

- `body`:

  The HTTP payload.

- `...`:

  Arguments to
  [`httr::PATCH()`](https://httr.r-lib.org/reference/PATCH.html)

- `url`:

  Target URL. Default uses `path`, but provide `url` to request a server
  resource that is not under `/__api__`

- `encode`:

  How the payload is encoded.

- `parser`:

  How the response is parsed. If `NULL`, the `httr_response` will be
  returned. Otherwise, the argument is forwarded to
  `httr::content(res, as = parser)`.

------------------------------------------------------------------------

### `Connect$POST()`

Perform an HTTP POST request of the named API path.

#### Usage

    Connect$POST(
      path,
      body = "{}",
      ...,
      url = self$api_url(path),
      encode = "json",
      parser = "parsed"
    )

#### Arguments

- `path`:

  API path relative to the server's `/__api__` root.

- `body`:

  The HTTP payload.

- `...`:

  Arguments to
  [`httr::POST()`](https://httr.r-lib.org/reference/POST.html)

- `url`:

  Target URL. Default uses `path`, but provide `url` to request a server
  resource that is not under `/__api__`

- `encode`:

  How the payload is encoded.

- `parser`:

  How the response is parsed. If `NULL`, the `httr_response` will be
  returned. Otherwise, the argument is forwarded to
  `httr::content(res, as = parser)`.

------------------------------------------------------------------------

### `Connect$me()`

Perform an HTTP GET request of the "me" server endpoint.

#### Usage

    Connect$me()

------------------------------------------------------------------------

### `Connect$get_dashboard_url()`

Return the base URL of the Connect server.

#### Usage

    Connect$get_dashboard_url()

------------------------------------------------------------------------

### `Connect$get_tags()`

Return all tags.

#### Usage

    Connect$get_tags(use_cache = FALSE)

#### Arguments

- `use_cache`:

  Indicates that a cached set of tags is used.

------------------------------------------------------------------------

### `Connect$get_tag_id()`

Get the identifier for the named tag.

#### Usage

    Connect$get_tag_id(tagname)

#### Arguments

- `tagname`:

  The name of the tag.

------------------------------------------------------------------------

### `Connect$get_tag_tree()`

Get the tag tree.

#### Usage

    Connect$get_tag_tree()

------------------------------------------------------------------------

### `Connect$tag_create_safe()`

Create a tag.

#### Usage

    Connect$tag_create_safe(name, parent_id = NULL)

#### Arguments

- `name`:

  The tag name.

- `parent_id`:

  The parent identifier.

------------------------------------------------------------------------

### `Connect$tag_create()`

Create a tag.

#### Usage

    Connect$tag_create(name, parent_id = NULL)

#### Arguments

- `name`:

  The tag name.

- `parent_id`:

  The parent identifier.

------------------------------------------------------------------------

### `Connect$tag()`

Get a tag.

#### Usage

    Connect$tag(id = NULL)

#### Arguments

- `id`:

  The tag identifier.

------------------------------------------------------------------------

### `Connect$tag_delete()`

Delete a tag.

#### Usage

    Connect$tag_delete(id)

#### Arguments

- `id`:

  The tag identifier.

------------------------------------------------------------------------

### `Connect$get_schedule()`

Get a schedule.

#### Usage

    Connect$get_schedule(schedule_id)

#### Arguments

- `schedule_id`:

  The schedule identifier.

------------------------------------------------------------------------

### `Connect$content_create()`

Create content.

#### Usage

    Connect$content_create(name, title = name, ...)

#### Arguments

- `name`:

  The content name.

- `title`:

  The content title.

- `...`:

  Other content fields.

------------------------------------------------------------------------

### `Connect$content_upload()`

Upload a content bundle.

#### Usage

    Connect$content_upload(bundle_path, guid)

#### Arguments

- `bundle_path`:

  The path to the bundle archive.

- `guid`:

  The content GUID.

------------------------------------------------------------------------

### `Connect$content_deploy()`

Deploy a content bundle.

#### Usage

    Connect$content_deploy(guid, bundle_id)

#### Arguments

- `guid`:

  The content GUID.

- `bundle_id`:

  The bundle identifier.

------------------------------------------------------------------------

### `Connect$content()`

Get a content item.

#### Usage

    Connect$content(
      guid = NULL,
      owner_guid = NULL,
      name = NULL,
      include = "tags,owner"
    )

#### Arguments

- `guid`:

  The content GUID.

- `owner_guid`:

  The target content owner.

- `name`:

  The target name.

- `include`:

  Additional response fields.

------------------------------------------------------------------------

### `Connect$task()`

Get a task.

#### Usage

    Connect$task(task_id, first = 0, wait = 5)

#### Arguments

- `task_id`:

  The task identifier.

- `first`:

  The initial status position.

- `wait`:

  Maximum time to wait for update.

------------------------------------------------------------------------

### `Connect$set_content_tag()`

Set a tag for a content item.

#### Usage

    Connect$set_content_tag(content_id, tag_id)

#### Arguments

- `content_id`:

  The content identifier.

- `tag_id`:

  The tag identifier.

------------------------------------------------------------------------

### `Connect$remove_content_tag()`

Remove a tag from a content item.

#### Usage

    Connect$remove_content_tag(content_id, tag_id)

#### Arguments

- `content_id`:

  The content identifier.

- `tag_id`:

  The tag identifier.

------------------------------------------------------------------------

### `Connect$user()`

Get user details.

#### Usage

    Connect$user(guid)

#### Arguments

- `guid`:

  The user GUID.

------------------------------------------------------------------------

### `Connect$users()`

Get users.

#### Usage

    Connect$users(
      page_number = 1,
      prefix = NULL,
      page_size = 500,
      user_role = NULL,
      account_status = NULL
    )

#### Arguments

- `page_number`:

  The page number.

- `prefix`:

  The search term.

- `page_size`:

  The page size.

- `user_role`:

  Filter by user role.

- `account_status`:

  Filter by account status.

------------------------------------------------------------------------

### `Connect$users_remote()`

Get remote users.

#### Usage

    Connect$users_remote(prefix)

#### Arguments

- `prefix`:

  The search term.

------------------------------------------------------------------------

### `Connect$users_create()`

Create a user.

#### Usage

    Connect$users_create(
      username,
      email,
      first_name = NULL,
      last_name = NULL,
      password = NULL,
      user_must_set_password = NULL,
      user_role = NULL,
      unique_id = NULL
    )

#### Arguments

- `username`:

  The username.

- `email`:

  Email address.

- `first_name`:

  First name.

- `last_name`:

  Last name.

- `password`:

  The password.

- `user_must_set_password`:

  Indicates that user sets password on first login.

- `user_role`:

  Role for user.

- `unique_id`:

  Identifier for user.

------------------------------------------------------------------------

### `Connect$users_create_remote()`

Create a remote user.

#### Usage

    Connect$users_create_remote(temp_ticket)

#### Arguments

- `temp_ticket`:

  Ticket identifying target remote user.

------------------------------------------------------------------------

### `Connect$users_lock()`

Lock a user.

#### Usage

    Connect$users_lock(user_guid)

#### Arguments

- `user_guid`:

  User GUID.

------------------------------------------------------------------------

### `Connect$users_unlock()`

Unlock a user.

#### Usage

    Connect$users_unlock(user_guid)

#### Arguments

- `user_guid`:

  User GUID.

------------------------------------------------------------------------

### `Connect$users_update()`

Update a user.

#### Usage

    Connect$users_update(user_guid, ...)

#### Arguments

- `user_guid`:

  User GUID.

- `...`:

  User fields.

------------------------------------------------------------------------

### `Connect$groups()`

Get groups.

#### Usage

    Connect$groups(page_number = 1, prefix = NULL, page_size = 500)

#### Arguments

- `page_number`:

  The page number.

- `prefix`:

  The search term.

- `page_size`:

  The page size.

------------------------------------------------------------------------

### `Connect$group_members()`

Get group members.

#### Usage

    Connect$group_members(guid)

#### Arguments

- `guid`:

  The group GUID.

------------------------------------------------------------------------

### `Connect$group_member_add()`

Add a group member.

#### Usage

    Connect$group_member_add(group_guid, user_guid)

#### Arguments

- `group_guid`:

  The group GUID.

- `user_guid`:

  The user GUID.

------------------------------------------------------------------------

### `Connect$group_member_remove()`

Remove a group member.

#### Usage

    Connect$group_member_remove(group_guid, user_guid)

#### Arguments

- `group_guid`:

  The group GUID.

- `user_guid`:

  The user GUID.

------------------------------------------------------------------------

### `Connect$groups_create()`

Create a group.

#### Usage

    Connect$groups_create(name)

#### Arguments

- `name`:

  The group name.

------------------------------------------------------------------------

### `Connect$groups_create_remote()`

Create a remote group.

#### Usage

    Connect$groups_create_remote(temp_ticket)

#### Arguments

- `temp_ticket`:

  Ticket identifying target remote group.

------------------------------------------------------------------------

### `Connect$groups_remote()`

Get remote groups.

#### Usage

    Connect$groups_remote(prefix = NULL, limit = 500)

#### Arguments

- `prefix`:

  The search term.

- `limit`:

  The maximal result set size.

------------------------------------------------------------------------

### `Connect$group_content()`

Get content to which a group has access

#### Usage

    Connect$group_content(guid)

#### Arguments

- `guid`:

  The group GUID.

------------------------------------------------------------------------

### `Connect$inst_content_visits()`

Get (non-interactive) content visits.

#### Usage

    Connect$inst_content_visits(
      content_guid = NULL,
      min_data_version = NULL,
      from = NULL,
      to = NULL,
      limit = 500,
      previous = NULL,
      nxt = NULL,
      asc_order = TRUE
    )

#### Arguments

- `content_guid`:

  Content GUID.

- `min_data_version`:

  Data version for request.

- `from`:

  Start of range.

- `to`:

  End of range.

- `limit`:

  Result set size.

- `previous`:

  Previous item.

- `nxt`:

  Next item.

- `asc_order`:

  Indicates ascending result order.

------------------------------------------------------------------------

### `Connect$inst_shiny_usage()`

Get interactive content visits.

Get (non-interactive) content visits.

#### Usage

    Connect$inst_shiny_usage(
      content_guid = NULL,
      min_data_version = NULL,
      from = NULL,
      to = NULL,
      limit = 500,
      previous = NULL,
      nxt = NULL,
      asc_order = TRUE
    )

#### Arguments

- `content_guid`:

  Content GUID.

- `min_data_version`:

  Data version for request.

- `from`:

  Start of range.

- `to`:

  End of range.

- `limit`:

  Result set size.

- `previous`:

  Previous item.

- `nxt`:

  Next item.

- `asc_order`:

  Indicates ascending result order.

------------------------------------------------------------------------

### `Connect$procs()`

Get running processes.

#### Usage

    Connect$procs()

------------------------------------------------------------------------

### `Connect$repo_account()`

Determine if Git repository is associated with authorization.

#### Usage

    Connect$repo_account(host)

#### Arguments

- `host`:

  Repository URL.

------------------------------------------------------------------------

### `Connect$repo_branches()`

Get Git repository branches.

#### Usage

    Connect$repo_branches(repo)

#### Arguments

- `repo`:

  Repository URL.

------------------------------------------------------------------------

### `Connect$repo_manifest_dirs()`

Get Git repository directories.

#### Usage

    Connect$repo_manifest_dirs(repo, branch)

#### Arguments

- `repo`:

  Repository URL.

- `branch`:

  Repository branch.

------------------------------------------------------------------------

### `Connect$schedules()`

Get schedules.

#### Usage

    Connect$schedules(
      start = Sys.time(),
      end = Sys.time() + 60 * 60 * 24 * 7,
      detailed = FALSE
    )

#### Arguments

- `start`:

  Starting time.

- `end`:

  Ending time.

- `detailed`:

  Indicates detailed schedule information.

------------------------------------------------------------------------

### `Connect$packages()`

Get packages. This endpoint is paginated.

#### Usage

    Connect$packages(name = NULL, page_number = 1, page_size = 1e+05)

#### Arguments

- `name`:

  The package name to filter by.

- `page_number`:

  Page number.

- `page_size`:

  Page size, default 100000.

------------------------------------------------------------------------

### `Connect$docs()`

Get documentation.

#### Usage

    Connect$docs(docs = "api", browse = TRUE)

#### Arguments

- `docs`:

  Named document.

- `browse`:

  Open a browser.

------------------------------------------------------------------------

### `Connect$audit_logs()`

Get auditing.

#### Usage

    Connect$audit_logs(limit = 500, previous = NULL, nxt = NULL, asc_order = TRUE)

#### Arguments

- `limit`:

  Result set size.

- `previous`:

  Previous item.

- `nxt`:

  Next item.

- `asc_order`:

  Indicates ascending result order.

------------------------------------------------------------------------

### `Connect$vanities()`

Get all vanity URLs

#### Usage

    Connect$vanities()

------------------------------------------------------------------------

### `Connect$server_settings()`

Get server settings.

#### Usage

    Connect$server_settings()

------------------------------------------------------------------------

### `Connect$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Connect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
