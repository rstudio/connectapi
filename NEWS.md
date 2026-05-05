# connectapi (development version)

- Improved performance of API response parsing and pagination. Data frames
  returned by getter functions now include all columns from the server, so new
  fields added in future Connect releases will appear automatically. Column
  names and types are now determined by the server response rather than a
  hardcoded schema, so they may vary across Connect versions.
  
# connectapi 0.12.1

- Fixed `repo_check_branches()` and `repo_check_branches_ref()` to handle the
  updated response format from newer versions of Connect (#524).

# connectapi 0.12.0

- Apps running on Connect Cloud are now supported for OAuth integrations via the
  new `on_connect_cloud()` helper. The `POSIT_PRODUCT` environment variable is
  now recognized as an alternative to `RSTUDIO_PRODUCT` for detecting Connect
  (#522).
- When using integrations, prefer to read from
  `CONNECT_CONTENT_SESSION_TOKEN_FILE` to find the session token. This helps
  long-running processes ensure that they can maintain fresh credentials (#518).

# connectapi 0.11.1

- `get_usage()` now returns the id column as a character to match other parts of the API (#512).

# connectapi 0.11.0

- `get_usage()` now allows for filtering by content GUID with the `content_guid`
  argument. This is only available on Connect server versions 2026.01 and later.
- The `activate` argument to `set_schedule_*()` functions is deprecated and
  no longer has any effect, due to changes in the Connect API. It will be
  removed in a future release. (#500)
- Added a single retry to `content_restart()` to more robustly clean up
  temporary environment variables. (#498)
- Improved performance of `page_cursor()`. (#501)

## Breaking changes

- Removed `get_image`, `delete_image`, `has_image`, `set_image_path`,
  `set_image_url`, `set_image_websthot`, and `Connect$server_settings_r`, all of
  which were deprecated since version 0.3.1.

# connectapi 0.10.0

- Removed `prefix` argument to `connect()` function, which was a convenience utility for testing. (#477)

## Breaking changes

- Removed `get_job()`, which was deprecated in 0.6.0. Instead, use `get_jobs()`
  to get a data frame of jobs, use `get_job_list()` to get a list of job
  objects, and use `get_log()` to get the log for a job (#491).
- Removed `swap_vanity_url()`, which was deprecated in 0.6.0. Use
  `swap_vanity_urls()` instead (#493).

# connectapi 0.9.0

- New `set_integrations()` function to set the OAuth integration
  associations for a content item. (#414)
- New `get_integration()` function to retrieve details of a specific OAuth integration
  from a Connect server. (#431)
- `get_integrations()` can now be passed a `Content` class object to retrieve a
  list of integrations associated with that piece of content. (#432)
- New functions allow you to manage the OAuth integrations on your Connect
  server: `create_integration()`, `update_integration()` and
  `delete_integration()`. (#434)
- New `search_content()` function which lets you search and filter content items
  on the Connect server. (#272, #447)
- New `lock_content()` and `unlock_content()` functions for locking and unlocking
  content items. (#453)
- Updated git-backed deployment functions to use v1 APIs (#459)

# connectapi 0.8.0

## Breaking changes

- `get_apps()` has been removed in favor of `get_content()`.
  `dashboard_url_chr()` has been removed. (#415)

## New features

- New `get_usage()` function returns content usage data from Connect's `GET
  v1/instrumentation/content/hits` endpoint on Connect v2025.04.0 and higher.
  (#390)
- The `get_oauth_credentials()` and `connect()` functions have a new `audience`
  parameter. On Connect v2025.07.0 and higher, pass the GUID of an integration
  to this parameter to specify which OAuth integration you wish to use. (#423)
- New `get_integrations()` function lists all OAuth integrations available on the
  Connect server from the `GET v1/oauth/integrations` endpoint on Connect v2024.12.0
  and higher. (#413)

## Enhancements and fixes

- `get_groups()` now paginates through all results when a `prefix` is provided,
  if the Connect server API version supports pagination. (#328)
- Timestamps from the Connect server are now displayed in your local time zone,
  rather than in UTC. (#400)
- `get_content()` now includes vanity URLs in the returned data frame on Connect
  v2024.06.0 and later. (#398)
- Removed unnecessary null check with default from `get_oauth_credentials()` and
  `get_oauth_content_credentials()` functions for `requested_token_type` parameter
  that was causing issues with Connect < 2025.03.0. (#407)
- `Connect$content()` now respects the `include` argument when a `guid` is
  provided. (#411)

# connectapi 0.7.0

## New features

- New `get_packages()` function to get a data frame of all packages on a Connect
  server. (#374)
- New `get_content_packages()` function to get a data frame of all package
  dependencies for a content item. (#374)
- New `get_aws_credentials()` and `get_aws_content_credentials` functions for interacting with Connect's
  `/v1/oauth/integrations/credentials` endpoint. This endpoint allows content running on Posit Connect to
  obtain temporary AWS credentials for an AWS IAM role specified by the system administrator.
  (#380).

# connectapi 0.6.0

## New features

- New `get_log()` function which lets you get the log for a given job.
  (#341)
- New `get_job_list()` function returns a list of jobs for a content item.
  (#341)
- New `token` parameter to `connect()` function allows you to create a Connect
  client with permissions scoped to the content visitor when running on a Connect
  server. (#362)

## Enhancements and fixes

- `swap_vanity_urls()` can correctly perform a swap involving a content item
  with no vanity URL. (#360)
- `swap_vanity_urls()` handles permissions errors gracefully, attempting to roll
  back any changes made. (#360)
- `get_jobs()` and `get_job_list()` return objects contain `content_id` and
  `content_guid` fields. These contain the same values as `app_id` and
  `app_guid`, which are deprecated and will be removed in a future update.

## Newly deprecated

- `get_job()` (singular) is now deprecated, its functionality taken care of by
  other functions, including `get_log()`.
- `swap_vanity_url(old, new)` has been deprecated and renamed to
  `swap_vanity_urls(content_a, content_b)`.

# connectapi 0.5.0

## Breaking changes

- `get_jobs()` now uses the public `v1` jobs endpoint when available, and as a
  result, the data returned by this function has changed. In particular, the
  `finalized` column is no longer present, replaced by `status`. `status == 0`
  corresponds to `isFALSE(finalized)`. See `?get_jobs()` for more details about
  the new return data format. (#340)

## New features

- `get_users()` has new parameters for filtering users by `account_status` and
  `user_role`. This allows you to find, for example, all licensed users on a
  Connect server. (#311)
- The new `get_group_content()` function lets you view the content that groups
  have permission to access. (#334)
- The new `terminate_jobs()` function lets you terminate processes associated
  with a content item. (#332)
- The new `get_vanity_urls()` function lets you get all vanity URLs on a Connect
  server (#333)
- Added new `get_oauth_content_credentials()` function for interacting with
  Connect's `/v1/oauth/integrations/credentials` endpoint. This endpoint allows
  content running on Posit Connect to obtain an OAuth access token using the
  client credentials flow. (#344).

## Minor improvements and fixes

- The task returned by `content_render()` now has the expected `task_id`
  parameter and is able to be polled. (#338)

# connectapi 0.4.0

## New features

- New functions `set_thumbnail()`, `get_thumbnail()`, `delete_thumbnail()` and
  `has_thumbnail()` let you interact with content thumbnails, replacing older
  `*_image()` functions. (#294, #304)
- `groups_create_remote()` now has an `exact` parameter. Setting `exact` causes
  the function to consider only exact group name matches when searching for
  remote groups and checking for existing local groups. (#216)
- New functions to let you view and delete runtime caches on a Connect server:
  `get_runtime_caches()`, `delete_runtime_cache()` (#312)
- New `get_runtimes()` lets you view available runtimes and versions on a
  Connect server. (#311)

## Lifecycle changes

### Newly deprecated

- `set_image_path()`, `set_image_url()`, and `set_image_webshot()` have been
  deprecated and will be removed in a future update. They have been replaced by
  `set_thumbnail()`, which works both with local file paths and remote URLs to
  images. Likewise, `has_image()` and `delete_image()` have been deprecated in
  favor of `has_thumbnail()` and `delete_thumbnail()`. (#294, #304)
- `Connect$server_settings_r()` has been deprecated in favor of
  `get_runtimes(client, "r")`. (#311)

## Minor improvements and fixes

- Upgrade `pkgdown` to bootstrap 5 to enable search (@fh-mthomson, #275)
- The `get_timezones()` function now uses the `v1/timezones` endpoint if
  available. (#300)
- `connect$DELETE()` now respects the `parser` argument rather than always using
  `NULL`.
- `get_groups()` no longer hangs when a search `prefix` is provided. (#319)

# connectapi 0.3.0

## Breaking changes

- `GET_URL()`, `GET_RESULT()`, and `GET_RESULT_URL()` have been eliminated in
  favor of `GET()` (#274).
- The `.empty_object` argument has been removed from `PUT`/`PATCH`/`POST` (#274).

## New features

- New `content_render()` and `content_restart()` functions. `content_render()`
  allows you to programmatically re-render content such as Quarto and R Markdown
  reports and Jupyter notebooks, optionally passing in a `variant_key` to render
  a specific variant of parameterized content. `content_restart()` lets you
  restart interactive content, such as Shiny applications, APIs, or dashboards
  (#283, #289)
- New `get_oauth_credentials()` function for interacting with Connect's
  `/v1/oauth/integrations/credentials` endpoint. This endpoint allows content
  running on Posit Connect to obtain the content viewer's OAuth access token
  (#297).

## Minor improvements and fixes

- Timestamps with non-zero offsets received from Connect no longer parse as `NA`
  (#290).
- Timestamps sent to Connect are now correctly converted to UTC, instead of
  simply being labeled as GMT (#291).
- Functions to render variants and email reports now contain the request
  query that Connect expects (#277).
- HTTP verb functions can take any URL, not just one relative to API root, and
  can optionally return the `httr_response` object (#274).

# connectapi 0.2.0

## Breaking changes

- All previously deprecated functions are now removed.
- The functions `Connect$download_bundle()` and
  `Connect$bundle_delete()` have been removed. Use `Content$bundle_download()`
  and `Content$bundle_delete()` instead.
- `audit_vanity_urls()` has been removed. To check if a vanity URL is in use,
  use `vanity_is_available()` instead.
- Other `audit_*` functions have been modified to accept the result of `get_content()` rather than `cache_apps()` (which is now removed). They are faster as a result.
- dplyr is no longer a required dependency. If you use `tbl_connect()`,
  you will need to install dplyr and dbplyr explicitly. (#246)

## Enhancements and fixes

- The package is now tested against many versions of Connect, back to 1.8.8.2 (May 2021).
  There are now fewer warnings about version mismatches: you should only see a warning if your Connect server is older than that. (#244)
- Now correctly provides methods for `tbl_connect`, rather than `tbl_lazy`,
  preventing problems when also using dplyr (#177).
- `progress` is now an optional dependency. To show pretty progress bars, install
  the package explicitly. (#269)
- `Content$tag_delete()` removes the tag from the target content item rather
  than removing the tag entirely. (#194)
- `audit_r_versions()` returns a bar chart instead of a histogram (#179)
- Fix issue with `NULL` or `length 1` job outputs (#193)
- Timestamp parsing now correctly preserves time components (#259)

# connectapi 0.1.3.1

- Fix generated documentation HTML for CRAN submission

# connectapi 0.1.3

- Rebrand RStudio to Posit
  - `RStudioConnect` documentation is now at `PositConnect`
- Fix `purrr` deprecated changes

# connectapi 0.1.2

- Update docs to illustrate customizing HTTP requests
(#168)
- Fix issue with HTML documentation to retain on CRAN
(#164)
- Fix typo in `min_data_version` parameter for usage data functions
(#166)
- Bump Connect tested version to 2022.09.0
  (#170)

# connectapi 0.1.1.1

### BREAKING

- BREAKING: the following functions now require RStudio Connect 1.8.6 or later
(because they are no longer experimental, as of that release).
(#128)
    - `set_vanity_url()`, `get_vanity_url()`, `swap_vanity_url()`
    - `get_tags()`, `get_tag_data()`, `get_content_tags()`, `create_tag()`,
    `create_tag_tree()`, `delete_tag()`, `get_content_tags()`,
    `set_content_tags()`, `set_content_tag_tree()`, `filter_tag_tree_id()`,
    `filter_tag_tree_chr()`, `set_environment_new()`, `get_environment()`,
    `set_environment_remove()`, `download_bundle()`
    - `tag id`s are now character strings (of integers) instead of integers
- BREAKING: `Connect$new()` now takes a `server` argument (instead of `host`)
    - The same is true of the `connect()` function, although we warn about
    argument deprecation in that case.
    (#125)
- BREAKING: `set_environment_new()` and `set_environment_remove()` no longer
  take a `.version` argument, and output data structure is a bit different (a
  list of names). They now use the public API, which changes the interface a bit.
  Also, intricacies of how to set / remove environment variables are changed (i.e.
  setting `VAR=NA` will remove `VAR`).
  (#141)
- BREAKING: `get_vanity_url()` and `set_vanity_url()` are now no longer
  experimental functions.
  (#113) However:
    - `get_vanity_url()` now returns a character string representing the vanity
    url in use (or NULL if not defined)
    - `set_vanity_url()` still returns a `Vanity` R6 object, but
    `vanity$get_vanity()$path_prefix` is now `vanity$get_vanity()$path`
- BREAKING: `tag_page()` and `tag_page_iframe()` have been removed. Similar functions belong
  in the [`connectwidgets`](https://rstudio.github.io/connectwidgets/) package in the future.
- BREAKING: Several `content_*` and other APIs have moved from experimental to
  "v1" variants. This means they have stabilized, but with several subtle breaking
  changes that could impact your scripts.
  (#115)
    - i.e. `bundle_id` has become `id` in some response data. In others, `url` has become `content_url`.
    - The R6 method `content$get_bundles()` no longer takes a `page_number`
    argument, and the `get_bundles(limit)` argument is now deprecated
    (#129)
    - `Connect$download_bundle` is now deprecated in favor of
    `Content$bundle_download()`, and `delete_bundle()` now takes a `Content`
    item instead of `Connect`.
    (#153)
- BREAKING: `acl_*()` functions are deprecated in favor of
  `get_content_permissions()`, `content_add_user()`, and friends.
- BREAKING: `Connect$PUT()`, `Connect$POST()` and `Connect$PATCH()` endpoints
  now presume that an empty list is really a "map"/"object" (like `{}` instead
  of `[]`). This can break some endpoints that expect a list. Set
  `.empty_object=FALSE` to avoid this behavior.

### Other Changes

- `users_create_remote()` gains an `exact` argument to simplify complex cases
  (#135). Long term, we should
  solicit feedback on whether this function attempts to do too much.
- Add helpers for common content modification actions: `content_update()`,
  `content_update_access_type()` and `content_update_owner()`
- Fix an issue with relative paths in `bundle_dir()`
  ([`@slodge`](https://github.com/slodge))
  (#118,
  #121)
- Add `overwrite=` parameter to `download_bundle()`
- Add HTTP request customization options, and related documentation
  (#101)
- Add git deployment (#112)
- Switch `Task` class to `ContentTask`
    - R6 does not support multiple inheritance, so we keep the `Task` interface up-to-date on
      `ContentTask` and `VariantTask` manually
- Improve several print methods
(#18,
#19)
- Protect against bad bundles
(#13)
- Error if an empty API key is defined (#16)
- Add a few `content_list_*` helpers
  (#130):
  - `content_list_with_permissions` returns a `content_list` with a "permission"
    column that includes who has access
  - `content_list_by_tag` allows fetching just a `content_list` for a particular tag
  - `content_list_guid_has_access` filters a "content list with permission" by
    whether a user or group GUID has access
- Add a `user_guid_from_username()` function to convert `session$user` or other
  usernames to a user GUID
  (#130)

# connectapi 0.1.0.9018

- Add a `client$PATCH` verb
- Switch `Content$update()` to use `PATCH` (which depends on RStudio Connect 1.8.6+)
- Add error messaging for new API endpoints when using older versions of Connect
- Fail more gracefully if/when protocol `http`/`https` is not defined

# connectapi 0.1.0.9017

BREAKING:
* Switch from `RSTUDIO_CONNECT_*` variables to `CONNECT_*` variables
* Rename a handful of functions:
  - `connect$activate_bundle` to `connect$content_deploy`
  - `connect$create_app` to `connect$content_create`
  - `connect$upload_bundle` to `connect$content_upload`
  - `connect$get_users` to `connect$users`
* Change some return types to be consistent with the API
  - `connect$content_upload` returns the response instead of `bundle_id`
  - `connect$content_deploy` returns the response instead of `task_id`
* Switch endpoints from using `app_id` to `guid`
* `get_task$start` renamed to `get_task$first`
* `promote$app_name` renamed to `promote$name`
* rename the package to `connectapi`
* change functions to take a `Connect` object instead of server / api key
  - `cache_apps`
  - `tag_page`

OTHER:
* Add some endpoints:
  - `content`
  - `audit_logs`
  - `server_settings`
  - `server_settings_r`
  - `inst_shiny_usage`
  - `inst_content_visits`
* Add some helper functions:
  - `swap_vanity_url`, deployment functions
  - `browse_` family of functions
  - `users_create_remote` and `groups_create_remote` for remote users/groups
* Update `Connect` R6 object to be compatible with Connect 1.7.0+ APIs
* Added a `NEWS.md` file to track changes to the package.
* Add integration testing to protect against regressions
* Add `tbl_connect()` as a `lazy_tbl` for querying Connect API endpoints
* Add `get_*` functions as alternatives to `lazy_tbl`

# connectapi 0.1.0

* Initial package version
* Create a `Connect` R6 object
