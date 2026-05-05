# Content

An R6 class that represents content.

## See also

Other R6 classes:
[`Bundle`](https://posit-dev.github.io/connectapi/dev/reference/Bundle.md),
[`ContentTask`](https://posit-dev.github.io/connectapi/dev/reference/ContentTask.md),
[`Environment`](https://posit-dev.github.io/connectapi/dev/reference/EnvironmentR6.md),
[`PositConnect`](https://posit-dev.github.io/connectapi/dev/reference/PositConnect.md),
[`Task`](https://posit-dev.github.io/connectapi/dev/reference/Task.md),
[`Vanity`](https://posit-dev.github.io/connectapi/dev/reference/Vanity.md),
[`Variant`](https://posit-dev.github.io/connectapi/dev/reference/VariantR6.md),
[`VariantSchedule`](https://posit-dev.github.io/connectapi/dev/reference/VariantSchedule.md),
[`VariantTask`](https://posit-dev.github.io/connectapi/dev/reference/VariantTask.md)

## Public fields

- `connect`:

  An R6 Connect object.

- `content`:

  The content details from Posit Connect. Properties are described in
  [`get_content()`](https://posit-dev.github.io/connectapi/dev/reference/get_content.md).

## Active bindings

- `default_variant`:

  The default variant for this object.

- `is_rendered`:

  TRUE if this is a rendered content type, otherwise FALSE.

- `is_interactive`:

  TRUE if this is a rendered content type, otherwise FALSE.

## Methods

### Public methods

- [`Content$new()`](#method-Content-initialize)

- [`Content$get_content_remote()`](#method-Content-get_content_remote)

- [`Content$get_bundles()`](#method-Content-get_bundles)

- [`Content$bundle_download()`](#method-Content-bundle_download)

- [`Content$bundle_delete()`](#method-Content-bundle_delete)

- [`Content$update()`](#method-Content-update)

- [`Content$danger_delete()`](#method-Content-danger_delete)

- [`Content$get_url()`](#method-Content-get_url)

- [`Content$get_dashboard_url()`](#method-Content-get_dashboard_url)

- [`Content$jobs()`](#method-Content-jobs)

- [`Content$register_job_kill_order()`](#method-Content-register_job_kill_order)

- [`Content$variants()`](#method-Content-variants)

- [`Content$tag_set()`](#method-Content-tag_set)

- [`Content$tag_delete()`](#method-Content-tag_delete)

- [`Content$tags()`](#method-Content-tags)

- [`Content$permissions_add()`](#method-Content-permissions_add)

- [`Content$permissions_update()`](#method-Content-permissions_update)

- [`Content$permissions_delete()`](#method-Content-permissions_delete)

- [`Content$permissions()`](#method-Content-permissions)

- [`Content$environment()`](#method-Content-environment)

- [`Content$environment_set()`](#method-Content-environment_set)

- [`Content$environment_all()`](#method-Content-environment_all)

- [`Content$deploy()`](#method-Content-deploy)

- [`Content$repository()`](#method-Content-repository)

- [`Content$repo_enable()`](#method-Content-repo_enable)

- [`Content$repo_set()`](#method-Content-repo_set)

- [`Content$packages()`](#method-Content-packages)

- [`Content$print()`](#method-Content-print)

- [`Content$clone()`](#method-Content-clone)

------------------------------------------------------------------------

### `Content$new()`

Initialize this content.

#### Usage

    Content$new(connect, content)

#### Arguments

- `connect`:

  The `Connect` instance.

- `content`:

  The content data.

------------------------------------------------------------------------

### `Content$get_content_remote()`

Obtain the content data from the Connect server.

#### Usage

    Content$get_content_remote()

------------------------------------------------------------------------

### `Content$get_bundles()`

Return the set of content bundles.

#### Usage

    Content$get_bundles()

------------------------------------------------------------------------

### `Content$bundle_download()`

Download the source archive for a content bundle.

#### Usage

    Content$bundle_download(
      bundle_id,
      filename = tempfile(pattern = "bundle", fileext = ".tar.gz"),
      overwrite = FALSE
    )

#### Arguments

- `bundle_id`:

  The bundle identifer.

- `filename`:

  Where to write the result.

- `overwrite`:

  Overwrite an existing filename.

------------------------------------------------------------------------

### `Content$bundle_delete()`

Delete a content bundle.

#### Usage

    Content$bundle_delete(bundle_id)

#### Arguments

- `bundle_id`:

  The bundle identifer.

------------------------------------------------------------------------

### `Content$update()`

Update this content item.

#### Usage

    Content$update(...)

#### Arguments

- `...`:

  Content fields.

------------------------------------------------------------------------

### `Content$danger_delete()`

Delete this content item.

#### Usage

    Content$danger_delete()

------------------------------------------------------------------------

### `Content$get_url()`

Return the URL for this content.

#### Usage

    Content$get_url()

------------------------------------------------------------------------

### `Content$get_dashboard_url()`

Return the URL for this content in the Posit Connect dashboard.

#### Usage

    Content$get_dashboard_url(pane = "")

#### Arguments

- `pane`:

  The pane in the dashboard to link to.

------------------------------------------------------------------------

### `Content$jobs()`

Return the jobs for this content

#### Usage

    Content$jobs()

------------------------------------------------------------------------

### `Content$register_job_kill_order()`

Terminate a single job for this content item.

#### Usage

    Content$register_job_kill_order(key)

#### Arguments

- `key`:

  The job key.

------------------------------------------------------------------------

### `Content$variants()`

Return the variants for this content.

#### Usage

    Content$variants()

------------------------------------------------------------------------

### `Content$tag_set()`

Set a tag for this content.

#### Usage

    Content$tag_set(tag_id)

#### Arguments

- `tag_id`:

  The tag identifier.

------------------------------------------------------------------------

### `Content$tag_delete()`

Remove a tag for this content.

#### Usage

    Content$tag_delete(tag_id)

#### Arguments

- `tag_id`:

  The tag identifier.

------------------------------------------------------------------------

### `Content$tags()`

The tags for this content.

#### Usage

    Content$tags()

------------------------------------------------------------------------

### `Content$permissions_add()`

Add a principal to the ACL for this content.

#### Usage

    Content$permissions_add(principal_guid, principal_type, role)

#### Arguments

- `principal_guid`:

  GUID for the target user or group.

- `principal_type`:

  Acting on user or group.

- `role`:

  The kind of content access.

------------------------------------------------------------------------

### `Content$permissions_update()`

Alter a principal in the ACL for this content.

#### Usage

    Content$permissions_update(id, principal_guid, principal_type, role)

#### Arguments

- `id`:

  The target identifier.

- `principal_guid`:

  GUID for the target user or group.

- `principal_type`:

  Acting on user or group.

- `role`:

  The kind of content access.

------------------------------------------------------------------------

### `Content$permissions_delete()`

Remove an entry from the ACL for this content.

#### Usage

    Content$permissions_delete(id)

#### Arguments

- `id`:

  The target identifier.

------------------------------------------------------------------------

### `Content$permissions()`

Obtain some or all of the ACL for this content.

#### Usage

    Content$permissions(id = NULL, add_owner = FALSE)

#### Arguments

- `id`:

  The target identifier.

- `add_owner`:

  Include the content owner in the result set.

------------------------------------------------------------------------

### `Content$environment()`

Return the environment variables set for this content.

#### Usage

    Content$environment()

------------------------------------------------------------------------

### `Content$environment_set()`

Adjust the environment variables set for this content.

#### Usage

    Content$environment_set(...)

#### Arguments

- `...`:

  Environment variable names and values. Use `NA` as the value to unset
  variables.

------------------------------------------------------------------------

### `Content$environment_all()`

Overwrite the environment variables set for this content.

#### Usage

    Content$environment_all(...)

#### Arguments

- `...`:

  Environment variable names and values.

------------------------------------------------------------------------

### `Content$deploy()`

Deploy this content

#### Usage

    Content$deploy(bundle_id = NULL)

#### Arguments

- `bundle_id`:

  Target bundle identifier.

------------------------------------------------------------------------

### `Content$repository()`

Get Git repository details

#### Usage

    Content$repository()

#### Returns

NULL if no repo is set, otherwise a list with fields:

- repository

- branch

- directory

- polling

- last_error

- last_known_commit

------------------------------------------------------------------------

### `Content$repo_enable()`

Adjust Git polling.

#### Usage

    Content$repo_enable(polling = TRUE)

#### Arguments

- `polling`:

  Polling enabled.

------------------------------------------------------------------------

### `Content$repo_set()`

Adjust Git repository

#### Usage

    Content$repo_set(repository, branch = "main", directory = ".", polling = FALSE)

#### Arguments

- `repository`:

  Git repository URL

- `branch`:

  Git repository branch

- `directory`:

  Git repository directory

- `polling`:

  Whether to check for updates

------------------------------------------------------------------------

### `Content$packages()`

Get package dependencies

#### Usage

    Content$packages()

------------------------------------------------------------------------

### `Content$print()`

Print this object.

#### Usage

    Content$print(...)

#### Arguments

- `...`:

  Unused.

------------------------------------------------------------------------

### `Content$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Content$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
