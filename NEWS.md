# Unreleased

## New features

- New functions `set_thumbnail()`, `get_thumbnail()`, `delete_thumbnail()` and
  `has_thumbnail()` let you interact with content thumbnails, replacing older
  `*_image()` functions.

## Lifecycle changes

- `connectapi` is not tested against Connect versions older than 2021.09.0.

### Newly deprecated

- `set_image_path()`, `set_image_url()`, and `set_image_webshot()` have been
  deprecated and will be removed in a future update. They have been replaced by
  `set_thumbnail()`, which works both with local file paths and remote URLs to
  images. Likewise, `has_image()` and `delete_image()` have been deprecated in
  favor of `has_thumbnail()` and `delete_thumbnail()`.

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
- Fix issue with `NULL` or `length 1` job outputs ([#193](https://github.com/rstudio/connectapi/issues/193))
- Timestamp parsing now correctly preserves time components (#259)

# connectapi 0.1.3.1

- Fix generated documentation HTML for CRAN submission

# connectapi 0.1.3

- Rebrand RStudio to Posit
  - `RStudioConnect` documentation is now at `PositConnect`
- Fix `purrr` deprecated changes

# connectapi 0.1.2

- Update docs to illustrate customizing HTTP requests
([#168](https://github.com/rstudio/connectapi/pull/168))
- Fix issue with HTML documentation to retain on CRAN
([#164](https://github.com/rstudio/connectapi/pull/164))
- Fix typo in `min_data_version` parameter for usage data functions
([#166](https://github.com/rstudio/connectapi/pull/166))
- Bump Connect tested version to 2022.09.0
  ([#170](https://github.com/rstudio/connectapi/pull/170))

# connectapi 0.1.1.1

### BREAKING

- BREAKING: the following functions now require RStudio Connect 1.8.6 or later
(because they are no longer experimental, as of that release).
([#128](https://github.com/rstudio/connectapi/pulls/12))
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
    ([#125](https://github.com/rstudio/connectapi/pulls/125))
- BREAKING: `set_environment_new()` and `set_environment_remove()` no longer
  take a `.version` argument, and output data structure is a bit different (a
  list of names). They now use the public API, which changes the interface a bit.
  Also, intricacies of how to set / remove environment variables are changed (i.e.
  setting `VAR=NA` will remove `VAR`).
  ([#141](https://github.com/rstudio/connectapi/pull/141))
- BREAKING: `get_vanity_url()` and `set_vanity_url()` are now no longer
  experimental functions.
  ([#113](https://github.com/rstudio/connectapi/pulls/113)) However:
    - `get_vanity_url()` now returns a character string representing the vanity
    url in use (or NULL if not defined)
    - `set_vanity_url()` still returns a `Vanity` R6 object, but
    `vanity$get_vanity()$path_prefix` is now `vanity$get_vanity()$path`
- BREAKING: `tag_page()` and `tag_page_iframe()` have been removed. Similar functions belong
  in the [`connectwidgets`](https://rstudio.github.io/connectwidgets/) package in the future.
- BREAKING: Several `content_*` and other APIs have moved from experimental to
  "v1" variants. This means they have stabilized, but with several subtle breaking
  changes that could impact your scripts.
  ([#115](https://github.com/rstudio/connectapi/pulls/115))
    - i.e. `bundle_id` has become `id` in some response data. In others, `url` has become `content_url`.
    - The R6 method `content$get_bundles()` no longer takes a `page_number`
    argument, and the `get_bundles(limit)` argument is now deprecated
    ([#129](https://github.com/rstudio/connectapi/pulls/129))
    - `Connect$download_bundle` is now deprecated in favor of
    `Content$bundle_download()`, and `delete_bundle()` now takes a `Content`
    item instead of `Connect`.
    ([#153](https://github.com/rstudio/connectapi/pull/153))
- BREAKING: `acl_*()` functions are deprecated in favor of
  `get_content_permissions()`, `content_add_user()`, and friends.
- BREAKING: `Connect$PUT()`, `Connect$POST()` and `Connect$PATCH()` endpoints
  now presume that an empty list is really a "map"/"object" (like `{}` instead
  of `[]`). This can break some endpoints that expect a list. Set
  `.empty_object=FALSE` to avoid this behavior.

### Other Changes

- `users_create_remote()` gains an `exact` argument to simplify complex cases
  ([#135](https://github.com/rstudio/connectapi/issues/135)). Long term, we should
  solicit feedback on whether this function attempts to do too much.
- Add helpers for common content modification actions: `content_update()`,
  `content_update_access_type()` and `content_update_owner()`
- Fix an issue with relative paths in `bundle_dir()`
  ([`@slodge`](https://github.com/slodge))
  ([#118](https://github.com/rstudio/connectapi/issues/118),
  [#121](https://github.com/rstudio/connectapi/issues/121))
- Add `overwrite=` parameter to `download_bundle()`
- Add HTTP request customization options, and related documentation
  ([#101](https://github.com/rstudio/connectapi/pull/101))
- Add git deployment ([#112](https://github.com/rstudio/connectapi/issues/112))
- Switch `Task` class to `ContentTask`
    - R6 does not support multiple inheritance, so we keep the `Task` interface up-to-date on
      `ContentTask` and `VariantTask` manually
- Improve several print methods
([#18](https://github.com/rstudio/connectapi/issues/18),
[#19](https://github.com/rstudio/connectapi/issues/19))
- Protect against bad bundles
([#13](https://github.com/rstudio/connectapi/issues/13))
- Error if an empty API key is defined ([#16](https://github.com/rstudio/connectapi/issues/16))
- Add a few `content_list_*` helpers
  ([#130](https://github.com/rstudio/connectapi/pulls/130)):
  - `content_list_with_permissions` returns a `content_list` with a "permission"
    column that includes who has access
  - `content_list_by_tag` allows fetching just a `content_list` for a particular tag
  - `content_list_guid_has_access` filters a "content list with permission" by
    whether a user or group GUID has access
- Add a `user_guid_from_username()` function to convert `session$user` or other
  usernames to a user GUID
  ([#130](https://github.com/rstudio/connectapi/pulls/130))

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
