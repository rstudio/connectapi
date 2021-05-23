# connectapi (development version)

- BREAKING: the following functions now require RStudio Connect 1.8.6 or later (because they are no longer experimental, as of that release).
    - `set_vanity_url()`, `get_vanity_url()`, `swap_vanity_url()`
    - `get_tags()`, `get_tag_data`, `get_content_tags()`
- BREAKING: `Connect$new()` now takes a `server` argument (instead of `host`)
    - The same is true of the `connect()` function, although we warn
      about argument deprecation in that case.
- BREAKING: `get_vanity_url()` and `set_vanity_url()` are now no longer experimental functions. However:
    - `get_vanity_url()` now returns a character string representing the vanity
    url in use (or NULL if not defined)
    - `set_vanity_url()` still returns a `Vanity` R6 object, but
    `vanity$get_vanity()$path_prefix` is now `vanity$get_vanity()$path`
- Fix an issue with relative paths in `bundle_dir()`
  ([`@slodge`](https://github.com/slodge))
  ([#118](https://github.com/rstudio/connectapi/issues/118),
  [#121](https://github.com/rstudio/connectapi/issues/121))
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
