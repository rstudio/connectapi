# Changelog

## connectapi (development version)

- Improved performance of API response parsing and pagination. Data
  frames returned by getter functions now include all columns from the
  server, so new fields added in future Connect releases will appear
  automatically. Column names and types are now determined by the server
  response rather than a hardcoded schema, so they may vary across
  Connect versions.

## connectapi 0.12.1

CRAN release: 2026-04-29

- Fixed
  [`repo_check_branches()`](https://posit-dev.github.io/connectapi/dev/reference/git.md)
  and
  [`repo_check_branches_ref()`](https://posit-dev.github.io/connectapi/dev/reference/git.md)
  to handle the updated response format from newer versions of Connect
  ([\#524](https://github.com/posit-dev/connectapi/issues/524)).

## connectapi 0.12.0

CRAN release: 2026-04-13

- Apps running on Connect Cloud are now supported for OAuth integrations
  via the new `on_connect_cloud()` helper. The `POSIT_PRODUCT`
  environment variable is now recognized as an alternative to
  `RSTUDIO_PRODUCT` for detecting Connect
  ([\#522](https://github.com/posit-dev/connectapi/issues/522)).
- When using integrations, prefer to read from
  `CONNECT_CONTENT_SESSION_TOKEN_FILE` to find the session token. This
  helps long-running processes ensure that they can maintain fresh
  credentials
  ([\#518](https://github.com/posit-dev/connectapi/issues/518)).

## connectapi 0.11.1

CRAN release: 2026-03-21

- [`get_usage()`](https://posit-dev.github.io/connectapi/dev/reference/get_usage.md)
  now returns the id column as a character to match other parts of the
  API ([\#512](https://github.com/posit-dev/connectapi/issues/512)).

## connectapi 0.11.0

CRAN release: 2026-02-25

- [`get_usage()`](https://posit-dev.github.io/connectapi/dev/reference/get_usage.md)
  now allows for filtering by content GUID with the `content_guid`
  argument. This is only available on Connect server versions 2026.01
  and later.
- The `activate` argument to `set_schedule_*()` functions is deprecated
  and no longer has any effect, due to changes in the Connect API. It
  will be removed in a future release.
  ([\#500](https://github.com/posit-dev/connectapi/issues/500))
- Added a single retry to
  [`content_restart()`](https://posit-dev.github.io/connectapi/dev/reference/content_restart.md)
  to more robustly clean up temporary environment variables.
  ([\#498](https://github.com/posit-dev/connectapi/issues/498))
- Improved performance of
  [`page_cursor()`](https://posit-dev.github.io/connectapi/dev/reference/paging.md).
  ([\#501](https://github.com/posit-dev/connectapi/issues/501))

### Breaking changes

- Removed `get_image`, `delete_image`, `has_image`, `set_image_path`,
  `set_image_url`, `set_image_websthot`, and
  `Connect$server_settings_r`, all of which were deprecated since
  version 0.3.1.

## connectapi 0.10.0

CRAN release: 2026-01-16

- Removed `prefix` argument to
  [`connect()`](https://posit-dev.github.io/connectapi/dev/reference/connect.md)
  function, which was a convenience utility for testing.
  ([\#477](https://github.com/posit-dev/connectapi/issues/477))

### Breaking changes

- Removed `get_job()`, which was deprecated in 0.6.0. Instead, use
  [`get_jobs()`](https://posit-dev.github.io/connectapi/dev/reference/get_jobs.md)
  to get a data frame of jobs, use
  [`get_job_list()`](https://posit-dev.github.io/connectapi/dev/reference/get_jobs.md)
  to get a list of job objects, and use
  [`get_log()`](https://posit-dev.github.io/connectapi/dev/reference/get_log.md)
  to get the log for a job
  ([\#491](https://github.com/posit-dev/connectapi/issues/491)).
- Removed `swap_vanity_url()`, which was deprecated in 0.6.0. Use
  [`swap_vanity_urls()`](https://posit-dev.github.io/connectapi/dev/reference/swap_vanity_urls.md)
  instead ([\#493](https://github.com/posit-dev/connectapi/issues/493)).

## connectapi 0.9.0

CRAN release: 2025-10-30

- New
  [`set_integrations()`](https://posit-dev.github.io/connectapi/dev/reference/set_integrations.md)
  function to set the OAuth integration associations for a content item.
  ([\#414](https://github.com/posit-dev/connectapi/issues/414))
- New
  [`get_integration()`](https://posit-dev.github.io/connectapi/dev/reference/get_integration.md)
  function to retrieve details of a specific OAuth integration from a
  Connect server.
  ([\#431](https://github.com/posit-dev/connectapi/issues/431))
- [`get_integrations()`](https://posit-dev.github.io/connectapi/dev/reference/get_integrations.md)
  can now be passed a `Content` class object to retrieve a list of
  integrations associated with that piece of content.
  ([\#432](https://github.com/posit-dev/connectapi/issues/432))
- New functions allow you to manage the OAuth integrations on your
  Connect server:
  [`create_integration()`](https://posit-dev.github.io/connectapi/dev/reference/create_integration.md),
  [`update_integration()`](https://posit-dev.github.io/connectapi/dev/reference/update_integration.md)
  and
  [`delete_integration()`](https://posit-dev.github.io/connectapi/dev/reference/delete_integration.md).
  ([\#434](https://github.com/posit-dev/connectapi/issues/434))
- New
  [`search_content()`](https://posit-dev.github.io/connectapi/dev/reference/search_content.md)
  function which lets you search and filter content items on the Connect
  server. ([\#272](https://github.com/posit-dev/connectapi/issues/272),
  [\#447](https://github.com/posit-dev/connectapi/issues/447))
- New
  [`lock_content()`](https://posit-dev.github.io/connectapi/dev/reference/lock_content.md)
  and
  [`unlock_content()`](https://posit-dev.github.io/connectapi/dev/reference/lock_content.md)
  functions for locking and unlocking content items.
  ([\#453](https://github.com/posit-dev/connectapi/issues/453))
- Updated git-backed deployment functions to use v1 APIs
  ([\#459](https://github.com/posit-dev/connectapi/issues/459))

## connectapi 0.8.0

CRAN release: 2025-07-30

### Breaking changes

- `get_apps()` has been removed in favor of
  [`get_content()`](https://posit-dev.github.io/connectapi/dev/reference/get_content.md).
  `dashboard_url_chr()` has been removed.
  ([\#415](https://github.com/posit-dev/connectapi/issues/415))

### New features

- New
  [`get_usage()`](https://posit-dev.github.io/connectapi/dev/reference/get_usage.md)
  function returns content usage data from Connect’s
  `GET v1/instrumentation/content/hits` endpoint on Connect v2025.04.0
  and higher.
  ([\#390](https://github.com/posit-dev/connectapi/issues/390))
- The
  [`get_oauth_credentials()`](https://posit-dev.github.io/connectapi/dev/reference/get_oauth_credentials.md)
  and
  [`connect()`](https://posit-dev.github.io/connectapi/dev/reference/connect.md)
  functions have a new `audience` parameter. On Connect v2025.07.0 and
  higher, pass the GUID of an integration to this parameter to specify
  which OAuth integration you wish to use.
  ([\#423](https://github.com/posit-dev/connectapi/issues/423))
- New
  [`get_integrations()`](https://posit-dev.github.io/connectapi/dev/reference/get_integrations.md)
  function lists all OAuth integrations available on the Connect server
  from the `GET v1/oauth/integrations` endpoint on Connect v2024.12.0
  and higher.
  ([\#413](https://github.com/posit-dev/connectapi/issues/413))

### Enhancements and fixes

- [`get_groups()`](https://posit-dev.github.io/connectapi/dev/reference/get_groups.md)
  now paginates through all results when a `prefix` is provided, if the
  Connect server API version supports pagination.
  ([\#328](https://github.com/posit-dev/connectapi/issues/328))
- Timestamps from the Connect server are now displayed in your local
  time zone, rather than in UTC.
  ([\#400](https://github.com/posit-dev/connectapi/issues/400))
- [`get_content()`](https://posit-dev.github.io/connectapi/dev/reference/get_content.md)
  now includes vanity URLs in the returned data frame on Connect
  v2024.06.0 and later.
  ([\#398](https://github.com/posit-dev/connectapi/issues/398))
- Removed unnecessary null check with default from
  [`get_oauth_credentials()`](https://posit-dev.github.io/connectapi/dev/reference/get_oauth_credentials.md)
  and
  [`get_oauth_content_credentials()`](https://posit-dev.github.io/connectapi/dev/reference/get_oauth_content_credentials.md)
  functions for `requested_token_type` parameter that was causing issues
  with Connect \< 2025.03.0.
  ([\#407](https://github.com/posit-dev/connectapi/issues/407))
- `Connect$content()` now respects the `include` argument when a `guid`
  is provided.
  ([\#411](https://github.com/posit-dev/connectapi/issues/411))

## connectapi 0.7.0

CRAN release: 2025-03-27

### New features

- New
  [`get_packages()`](https://posit-dev.github.io/connectapi/dev/reference/get_packages.md)
  function to get a data frame of all packages on a Connect server.
  ([\#374](https://github.com/posit-dev/connectapi/issues/374))
- New
  [`get_content_packages()`](https://posit-dev.github.io/connectapi/dev/reference/get_content_packages.md)
  function to get a data frame of all package dependencies for a content
  item. ([\#374](https://github.com/posit-dev/connectapi/issues/374))
- New
  [`get_aws_credentials()`](https://posit-dev.github.io/connectapi/dev/reference/get_aws_credentials.md)
  and `get_aws_content_credentials` functions for interacting with
  Connect’s `/v1/oauth/integrations/credentials` endpoint. This endpoint
  allows content running on Posit Connect to obtain temporary AWS
  credentials for an AWS IAM role specified by the system administrator.
  ([\#380](https://github.com/posit-dev/connectapi/issues/380)).

## connectapi 0.6.0

CRAN release: 2025-02-11

### New features

- New
  [`get_log()`](https://posit-dev.github.io/connectapi/dev/reference/get_log.md)
  function which lets you get the log for a given job.
  ([\#341](https://github.com/posit-dev/connectapi/issues/341))
- New
  [`get_job_list()`](https://posit-dev.github.io/connectapi/dev/reference/get_jobs.md)
  function returns a list of jobs for a content item.
  ([\#341](https://github.com/posit-dev/connectapi/issues/341))
- New `token` parameter to
  [`connect()`](https://posit-dev.github.io/connectapi/dev/reference/connect.md)
  function allows you to create a Connect client with permissions scoped
  to the content visitor when running on a Connect server.
  ([\#362](https://github.com/posit-dev/connectapi/issues/362))

### Enhancements and fixes

- [`swap_vanity_urls()`](https://posit-dev.github.io/connectapi/dev/reference/swap_vanity_urls.md)
  can correctly perform a swap involving a content item with no vanity
  URL. ([\#360](https://github.com/posit-dev/connectapi/issues/360))
- [`swap_vanity_urls()`](https://posit-dev.github.io/connectapi/dev/reference/swap_vanity_urls.md)
  handles permissions errors gracefully, attempting to roll back any
  changes made.
  ([\#360](https://github.com/posit-dev/connectapi/issues/360))
- [`get_jobs()`](https://posit-dev.github.io/connectapi/dev/reference/get_jobs.md)
  and
  [`get_job_list()`](https://posit-dev.github.io/connectapi/dev/reference/get_jobs.md)
  return objects contain `content_id` and `content_guid` fields. These
  contain the same values as `app_id` and `app_guid`, which are
  deprecated and will be removed in a future update.

### Newly deprecated

- `get_job()` (singular) is now deprecated, its functionality taken care
  of by other functions, including
  [`get_log()`](https://posit-dev.github.io/connectapi/dev/reference/get_log.md).
- `swap_vanity_url(old, new)` has been deprecated and renamed to
  `swap_vanity_urls(content_a, content_b)`.

## connectapi 0.5.0

CRAN release: 2024-12-18

### Breaking changes

- [`get_jobs()`](https://posit-dev.github.io/connectapi/dev/reference/get_jobs.md)
  now uses the public `v1` jobs endpoint when available, and as a
  result, the data returned by this function has changed. In particular,
  the `finalized` column is no longer present, replaced by `status`.
  `status == 0` corresponds to `isFALSE(finalized)`. See `?get_jobs()`
  for more details about the new return data format.
  ([\#340](https://github.com/posit-dev/connectapi/issues/340))

### New features

- [`get_users()`](https://posit-dev.github.io/connectapi/dev/reference/get_users.md)
  has new parameters for filtering users by `account_status` and
  `user_role`. This allows you to find, for example, all licensed users
  on a Connect server.
  ([\#311](https://github.com/posit-dev/connectapi/issues/311))
- The new
  [`get_group_content()`](https://posit-dev.github.io/connectapi/dev/reference/get_group_content.md)
  function lets you view the content that groups have permission to
  access. ([\#334](https://github.com/posit-dev/connectapi/issues/334))
- The new
  [`terminate_jobs()`](https://posit-dev.github.io/connectapi/dev/reference/terminate_jobs.md)
  function lets you terminate processes associated with a content item.
  ([\#332](https://github.com/posit-dev/connectapi/issues/332))
- The new
  [`get_vanity_urls()`](https://posit-dev.github.io/connectapi/dev/reference/get_vanity_urls.md)
  function lets you get all vanity URLs on a Connect server
  ([\#333](https://github.com/posit-dev/connectapi/issues/333))
- Added new
  [`get_oauth_content_credentials()`](https://posit-dev.github.io/connectapi/dev/reference/get_oauth_content_credentials.md)
  function for interacting with Connect’s
  `/v1/oauth/integrations/credentials` endpoint. This endpoint allows
  content running on Posit Connect to obtain an OAuth access token using
  the client credentials flow.
  ([\#344](https://github.com/posit-dev/connectapi/issues/344)).

### Minor improvements and fixes

- The task returned by
  [`content_render()`](https://posit-dev.github.io/connectapi/dev/reference/content_render.md)
  now has the expected `task_id` parameter and is able to be polled.
  ([\#338](https://github.com/posit-dev/connectapi/issues/338))

## connectapi 0.4.0

CRAN release: 2024-11-08

### New features

- New functions
  [`set_thumbnail()`](https://posit-dev.github.io/connectapi/dev/reference/set_thumbnail.md),
  [`get_thumbnail()`](https://posit-dev.github.io/connectapi/dev/reference/get_thumbnail.md),
  [`delete_thumbnail()`](https://posit-dev.github.io/connectapi/dev/reference/delete_thumbnail.md)
  and
  [`has_thumbnail()`](https://posit-dev.github.io/connectapi/dev/reference/has_thumbnail.md)
  let you interact with content thumbnails, replacing older `*_image()`
  functions.
  ([\#294](https://github.com/posit-dev/connectapi/issues/294),
  [\#304](https://github.com/posit-dev/connectapi/issues/304))
- [`groups_create_remote()`](https://posit-dev.github.io/connectapi/dev/reference/groups_create_remote.md)
  now has an `exact` parameter. Setting `exact` causes the function to
  consider only exact group name matches when searching for remote
  groups and checking for existing local groups.
  ([\#216](https://github.com/posit-dev/connectapi/issues/216))
- New functions to let you view and delete runtime caches on a Connect
  server:
  [`get_runtime_caches()`](https://posit-dev.github.io/connectapi/dev/reference/get_runtime_caches.md),
  [`delete_runtime_cache()`](https://posit-dev.github.io/connectapi/dev/reference/delete_runtime_cache.md)
  ([\#312](https://github.com/posit-dev/connectapi/issues/312))
- New
  [`get_runtimes()`](https://posit-dev.github.io/connectapi/dev/reference/get_runtimes.md)
  lets you view available runtimes and versions on a Connect server.
  ([\#311](https://github.com/posit-dev/connectapi/issues/311))

### Lifecycle changes

#### Newly deprecated

- `set_image_path()`, `set_image_url()`, and `set_image_webshot()` have
  been deprecated and will be removed in a future update. They have been
  replaced by
  [`set_thumbnail()`](https://posit-dev.github.io/connectapi/dev/reference/set_thumbnail.md),
  which works both with local file paths and remote URLs to images.
  Likewise, `has_image()` and `delete_image()` have been deprecated in
  favor of
  [`has_thumbnail()`](https://posit-dev.github.io/connectapi/dev/reference/has_thumbnail.md)
  and
  [`delete_thumbnail()`](https://posit-dev.github.io/connectapi/dev/reference/delete_thumbnail.md).
  ([\#294](https://github.com/posit-dev/connectapi/issues/294),
  [\#304](https://github.com/posit-dev/connectapi/issues/304))
- `Connect$server_settings_r()` has been deprecated in favor of
  `get_runtimes(client, "r")`.
  ([\#311](https://github.com/posit-dev/connectapi/issues/311))

### Minor improvements and fixes

- Upgrade `pkgdown` to bootstrap 5 to enable search
  ([@fh-mthomson](https://github.com/fh-mthomson),
  [\#275](https://github.com/posit-dev/connectapi/issues/275))
- The
  [`get_timezones()`](https://posit-dev.github.io/connectapi/dev/reference/get_timezones.md)
  function now uses the `v1/timezones` endpoint if available.
  ([\#300](https://github.com/posit-dev/connectapi/issues/300))
- `connect$DELETE()` now respects the `parser` argument rather than
  always using `NULL`.
- [`get_groups()`](https://posit-dev.github.io/connectapi/dev/reference/get_groups.md)
  no longer hangs when a search `prefix` is provided.
  ([\#319](https://github.com/posit-dev/connectapi/issues/319))

## connectapi 0.3.0

CRAN release: 2024-09-05

### Breaking changes

- `GET_URL()`, `GET_RESULT()`, and `GET_RESULT_URL()` have been
  eliminated in favor of
  [`GET()`](https://httr.r-lib.org/reference/GET.html)
  ([\#274](https://github.com/posit-dev/connectapi/issues/274)).
- The `.empty_object` argument has been removed from
  `PUT`/`PATCH`/`POST`
  ([\#274](https://github.com/posit-dev/connectapi/issues/274)).

### New features

- New
  [`content_render()`](https://posit-dev.github.io/connectapi/dev/reference/content_render.md)
  and
  [`content_restart()`](https://posit-dev.github.io/connectapi/dev/reference/content_restart.md)
  functions.
  [`content_render()`](https://posit-dev.github.io/connectapi/dev/reference/content_render.md)
  allows you to programmatically re-render content such as Quarto and R
  Markdown reports and Jupyter notebooks, optionally passing in a
  `variant_key` to render a specific variant of parameterized content.
  [`content_restart()`](https://posit-dev.github.io/connectapi/dev/reference/content_restart.md)
  lets you restart interactive content, such as Shiny applications,
  APIs, or dashboards
  ([\#283](https://github.com/posit-dev/connectapi/issues/283),
  [\#289](https://github.com/posit-dev/connectapi/issues/289))
- New
  [`get_oauth_credentials()`](https://posit-dev.github.io/connectapi/dev/reference/get_oauth_credentials.md)
  function for interacting with Connect’s
  `/v1/oauth/integrations/credentials` endpoint. This endpoint allows
  content running on Posit Connect to obtain the content viewer’s OAuth
  access token
  ([\#297](https://github.com/posit-dev/connectapi/issues/297)).

### Minor improvements and fixes

- Timestamps with non-zero offsets received from Connect no longer parse
  as `NA` ([\#290](https://github.com/posit-dev/connectapi/issues/290)).
- Timestamps sent to Connect are now correctly converted to UTC, instead
  of simply being labeled as GMT
  ([\#291](https://github.com/posit-dev/connectapi/issues/291)).
- Functions to render variants and email reports now contain the request
  query that Connect expects
  ([\#277](https://github.com/posit-dev/connectapi/issues/277)).
- HTTP verb functions can take any URL, not just one relative to API
  root, and can optionally return the `httr_response` object
  ([\#274](https://github.com/posit-dev/connectapi/issues/274)).

## connectapi 0.2.0

CRAN release: 2024-06-06

### Breaking changes

- All previously deprecated functions are now removed.
- The functions `Connect$download_bundle()` and
  `Connect$bundle_delete()` have been removed. Use
  `Content$bundle_download()` and `Content$bundle_delete()` instead.
- `audit_vanity_urls()` has been removed. To check if a vanity URL is in
  use, use
  [`vanity_is_available()`](https://posit-dev.github.io/connectapi/dev/reference/vanity_is_available.md)
  instead.
- Other `audit_*` functions have been modified to accept the result of
  [`get_content()`](https://posit-dev.github.io/connectapi/dev/reference/get_content.md)
  rather than `cache_apps()` (which is now removed). They are faster as
  a result.
- dplyr is no longer a required dependency. If you use
  [`tbl_connect()`](https://posit-dev.github.io/connectapi/dev/reference/tbl_connect.md),
  you will need to install dplyr and dbplyr explicitly.
  ([\#246](https://github.com/posit-dev/connectapi/issues/246))

### Enhancements and fixes

- The package is now tested against many versions of Connect, back to
  1.8.8.2 (May 2021). There are now fewer warnings about version
  mismatches: you should only see a warning if your Connect server is
  older than that.
  ([\#244](https://github.com/posit-dev/connectapi/issues/244))
- Now correctly provides methods for `tbl_connect`, rather than
  `tbl_lazy`, preventing problems when also using dplyr
  ([\#177](https://github.com/posit-dev/connectapi/issues/177)).
- `progress` is now an optional dependency. To show pretty progress
  bars, install the package explicitly.
  ([\#269](https://github.com/posit-dev/connectapi/issues/269))
- `Content$tag_delete()` removes the tag from the target content item
  rather than removing the tag entirely.
  ([\#194](https://github.com/posit-dev/connectapi/issues/194))
- [`audit_r_versions()`](https://posit-dev.github.io/connectapi/dev/reference/audit_r_versions.md)
  returns a bar chart instead of a histogram
  ([\#179](https://github.com/posit-dev/connectapi/issues/179))
- Fix issue with `NULL` or `length 1` job outputs
  ([\#193](https://github.com/posit-dev/connectapi/issues/193))
- Timestamp parsing now correctly preserves time components
  ([\#259](https://github.com/posit-dev/connectapi/issues/259))

## connectapi 0.1.3.1

CRAN release: 2023-02-02

- Fix generated documentation HTML for CRAN submission

## connectapi 0.1.3

- Rebrand RStudio to Posit
  - `RStudioConnect` documentation is now at `PositConnect`
- Fix `purrr` deprecated changes

## connectapi 0.1.2

CRAN release: 2022-09-30

- Update docs to illustrate customizing HTTP requests
  ([\#168](https://github.com/posit-dev/connectapi/issues/168))
- Fix issue with HTML documentation to retain on CRAN
  ([\#164](https://github.com/posit-dev/connectapi/issues/164))
- Fix typo in `min_data_version` parameter for usage data functions
  ([\#166](https://github.com/posit-dev/connectapi/issues/166))
- Bump Connect tested version to 2022.09.0
  ([\#170](https://github.com/posit-dev/connectapi/issues/170))

## connectapi 0.1.1.1

CRAN release: 2022-07-21

#### BREAKING

- BREAKING: the following functions now require RStudio Connect 1.8.6 or
  later (because they are no longer experimental, as of that release).
  ([\#128](https://github.com/posit-dev/connectapi/issues/128))
  - [`set_vanity_url()`](https://posit-dev.github.io/connectapi/dev/reference/set_vanity_url.md),
    [`get_vanity_url()`](https://posit-dev.github.io/connectapi/dev/reference/get_vanity_url.md),
    `swap_vanity_url()`
  - [`get_tags()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`get_tag_data()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`get_content_tags()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`create_tag()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`create_tag_tree()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`delete_tag()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`get_content_tags()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`set_content_tags()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`set_content_tag_tree()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`filter_tag_tree_id()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`filter_tag_tree_chr()`](https://posit-dev.github.io/connectapi/dev/reference/tags.md),
    [`set_environment_new()`](https://posit-dev.github.io/connectapi/dev/reference/environment.md),
    [`get_environment()`](https://posit-dev.github.io/connectapi/dev/reference/environment.md),
    [`set_environment_remove()`](https://posit-dev.github.io/connectapi/dev/reference/environment.md),
    [`download_bundle()`](https://posit-dev.github.io/connectapi/dev/reference/download_bundle.md)
  - `tag id`s are now character strings (of integers) instead of
    integers
- BREAKING: `Connect$new()` now takes a `server` argument (instead of
  `host`)
  - The same is true of the
    [`connect()`](https://posit-dev.github.io/connectapi/dev/reference/connect.md)
    function, although we warn about argument deprecation in that case.
    ([\#125](https://github.com/posit-dev/connectapi/issues/125))
- BREAKING:
  [`set_environment_new()`](https://posit-dev.github.io/connectapi/dev/reference/environment.md)
  and
  [`set_environment_remove()`](https://posit-dev.github.io/connectapi/dev/reference/environment.md)
  no longer take a `.version` argument, and output data structure is a
  bit different (a list of names). They now use the public API, which
  changes the interface a bit. Also, intricacies of how to set / remove
  environment variables are changed (i.e. setting `VAR=NA` will remove
  `VAR`). ([\#141](https://github.com/posit-dev/connectapi/issues/141))
- BREAKING:
  [`get_vanity_url()`](https://posit-dev.github.io/connectapi/dev/reference/get_vanity_url.md)
  and
  [`set_vanity_url()`](https://posit-dev.github.io/connectapi/dev/reference/set_vanity_url.md)
  are now no longer experimental functions.
  ([\#113](https://github.com/posit-dev/connectapi/issues/113)) However:
  - [`get_vanity_url()`](https://posit-dev.github.io/connectapi/dev/reference/get_vanity_url.md)
    now returns a character string representing the vanity url in use
    (or NULL if not defined)
  - [`set_vanity_url()`](https://posit-dev.github.io/connectapi/dev/reference/set_vanity_url.md)
    still returns a `Vanity` R6 object, but
    `vanity$get_vanity()$path_prefix` is now `vanity$get_vanity()$path`
- BREAKING: `tag_page()` and `tag_page_iframe()` have been removed.
  Similar functions belong in the
  [`connectwidgets`](https://rstudio.github.io/connectwidgets/) package
  in the future.
- BREAKING: Several `content_*` and other APIs have moved from
  experimental to “v1” variants. This means they have stabilized, but
  with several subtle breaking changes that could impact your scripts.
  ([\#115](https://github.com/posit-dev/connectapi/issues/115))
  - i.e. `bundle_id` has become `id` in some response data. In others,
    `url` has become `content_url`.
  - The R6 method `content$get_bundles()` no longer takes a
    `page_number` argument, and the `get_bundles(limit)` argument is now
    deprecated
    ([\#129](https://github.com/posit-dev/connectapi/issues/129))
  - `Connect$download_bundle` is now deprecated in favor of
    `Content$bundle_download()`, and
    [`delete_bundle()`](https://posit-dev.github.io/connectapi/dev/reference/get_bundles.md)
    now takes a `Content` item instead of `Connect`.
    ([\#153](https://github.com/posit-dev/connectapi/issues/153))
- BREAKING: `acl_*()` functions are deprecated in favor of
  [`get_content_permissions()`](https://posit-dev.github.io/connectapi/dev/reference/permissions.md),
  [`content_add_user()`](https://posit-dev.github.io/connectapi/dev/reference/permissions.md),
  and friends.
- BREAKING: `Connect$PUT()`, `Connect$POST()` and `Connect$PATCH()`
  endpoints now presume that an empty list is really a “map”/“object”
  (like [`{}`](https://rdrr.io/r/base/Paren.html) instead of `[]`). This
  can break some endpoints that expect a list. Set `.empty_object=FALSE`
  to avoid this behavior.

#### Other Changes

- [`users_create_remote()`](https://posit-dev.github.io/connectapi/dev/reference/users_create_remote.md)
  gains an `exact` argument to simplify complex cases
  ([\#135](https://github.com/posit-dev/connectapi/issues/135)). Long
  term, we should solicit feedback on whether this function attempts to
  do too much.
- Add helpers for common content modification actions:
  [`content_update()`](https://posit-dev.github.io/connectapi/dev/reference/content_update.md),
  [`content_update_access_type()`](https://posit-dev.github.io/connectapi/dev/reference/content_update.md)
  and
  [`content_update_owner()`](https://posit-dev.github.io/connectapi/dev/reference/content_update.md)
- Fix an issue with relative paths in
  [`bundle_dir()`](https://posit-dev.github.io/connectapi/dev/reference/bundle_dir.md)
  ([`@slodge`](https://github.com/slodge))
  ([\#118](https://github.com/posit-dev/connectapi/issues/118),
  [\#121](https://github.com/posit-dev/connectapi/issues/121))
- Add `overwrite=` parameter to
  [`download_bundle()`](https://posit-dev.github.io/connectapi/dev/reference/download_bundle.md)
- Add HTTP request customization options, and related documentation
  ([\#101](https://github.com/posit-dev/connectapi/issues/101))
- Add git deployment
  ([\#112](https://github.com/posit-dev/connectapi/issues/112))
- Switch `Task` class to `ContentTask`
  - R6 does not support multiple inheritance, so we keep the `Task`
    interface up-to-date on `ContentTask` and `VariantTask` manually
- Improve several print methods
  ([\#18](https://github.com/posit-dev/connectapi/issues/18),
  [\#19](https://github.com/posit-dev/connectapi/issues/19))
- Protect against bad bundles
  ([\#13](https://github.com/posit-dev/connectapi/issues/13))
- Error if an empty API key is defined
  ([\#16](https://github.com/posit-dev/connectapi/issues/16))
- Add a few `content_list_*` helpers
  ([\#130](https://github.com/posit-dev/connectapi/issues/130)):
  - `content_list_with_permissions` returns a `content_list` with a
    “permission” column that includes who has access
  - `content_list_by_tag` allows fetching just a `content_list` for a
    particular tag
  - `content_list_guid_has_access` filters a “content list with
    permission” by whether a user or group GUID has access
- Add a
  [`user_guid_from_username()`](https://posit-dev.github.io/connectapi/dev/reference/user.md)
  function to convert `session$user` or other usernames to a user GUID
  ([\#130](https://github.com/posit-dev/connectapi/issues/130))

## connectapi 0.1.0.9018

- Add a `client$PATCH` verb
- Switch `Content$update()` to use `PATCH` (which depends on RStudio
  Connect 1.8.6+)
- Add error messaging for new API endpoints when using older versions of
  Connect
- Fail more gracefully if/when protocol `http`/`https` is not defined

## connectapi 0.1.0.9017

BREAKING: \* Switch from `RSTUDIO_CONNECT_*` variables to `CONNECT_*`
variables \* Rename a handful of functions: - `connect$activate_bundle`
to `connect$content_deploy` - `connect$create_app` to
`connect$content_create` - `connect$upload_bundle` to
`connect$content_upload` - `connect$get_users` to `connect$users` \*
Change some return types to be consistent with the API -
`connect$content_upload` returns the response instead of `bundle_id` -
`connect$content_deploy` returns the response instead of `task_id` \*
Switch endpoints from using `app_id` to `guid` \* `get_task$start`
renamed to `get_task$first` \* `promote$app_name` renamed to
`promote$name` \* rename the package to `connectapi` \* change functions
to take a `Connect` object instead of server / api key - `cache_apps` -
`tag_page`

OTHER: \* Add some endpoints: - `content` - `audit_logs` -
`server_settings` - `server_settings_r` - `inst_shiny_usage` -
`inst_content_visits` \* Add some helper functions: - `swap_vanity_url`,
deployment functions - `browse_` family of functions -
`users_create_remote` and `groups_create_remote` for remote users/groups
\* Update `Connect` R6 object to be compatible with Connect 1.7.0+ APIs
\* Added a `NEWS.md` file to track changes to the package. \* Add
integration testing to protect against regressions \* Add
[`tbl_connect()`](https://posit-dev.github.io/connectapi/dev/reference/tbl_connect.md)
as a `lazy_tbl` for querying Connect API endpoints \* Add `get_*`
functions as alternatives to `lazy_tbl`

## connectapi 0.1.0

- Initial package version
- Create a `Connect` R6 object
