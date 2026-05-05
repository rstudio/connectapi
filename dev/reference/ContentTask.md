# ContentTask

An R6 class that represents a Task for a piece of Content

## See also

Other R6 classes:
[`Bundle`](https://posit-dev.github.io/connectapi/dev/reference/Bundle.md),
[`Content`](https://posit-dev.github.io/connectapi/dev/reference/Content.md),
[`Environment`](https://posit-dev.github.io/connectapi/dev/reference/EnvironmentR6.md),
[`PositConnect`](https://posit-dev.github.io/connectapi/dev/reference/PositConnect.md),
[`Task`](https://posit-dev.github.io/connectapi/dev/reference/Task.md),
[`Vanity`](https://posit-dev.github.io/connectapi/dev/reference/Vanity.md),
[`Variant`](https://posit-dev.github.io/connectapi/dev/reference/VariantR6.md),
[`VariantSchedule`](https://posit-dev.github.io/connectapi/dev/reference/VariantSchedule.md),
[`VariantTask`](https://posit-dev.github.io/connectapi/dev/reference/VariantTask.md)

## Super class

[`Content`](https://posit-dev.github.io/connectapi/dev/reference/Content.md)
-\> `ContentTask`

## Public fields

- `task`:

  The task.

- `data`:

  The task data.

## Methods

### Public methods

- [`ContentTask$new()`](#method-ContentTask-initialize)

- [`ContentTask$get_task()`](#method-ContentTask-get_task)

- [`ContentTask$add_data()`](#method-ContentTask-add_data)

- [`ContentTask$get_data()`](#method-ContentTask-get_data)

- [`ContentTask$print()`](#method-ContentTask-print)

- [`ContentTask$clone()`](#method-ContentTask-clone)

Inherited methods

- [`Content$bundle_delete()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-bundle_delete)
- [`Content$bundle_download()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-bundle_download)
- [`Content$danger_delete()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-danger_delete)
- [`Content$deploy()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-deploy)
- [`Content$environment()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-environment)
- [`Content$environment_all()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-environment_all)
- [`Content$environment_set()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-environment_set)
- [`Content$get_bundles()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-get_bundles)
- [`Content$get_content_remote()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-get_content_remote)
- [`Content$get_dashboard_url()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-get_dashboard_url)
- [`Content$get_url()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-get_url)
- [`Content$jobs()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-jobs)
- [`Content$packages()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-packages)
- [`Content$permissions()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-permissions)
- [`Content$permissions_add()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-permissions_add)
- [`Content$permissions_delete()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-permissions_delete)
- [`Content$permissions_update()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-permissions_update)
- [`Content$register_job_kill_order()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-register_job_kill_order)
- [`Content$repo_enable()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-repo_enable)
- [`Content$repo_set()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-repo_set)
- [`Content$repository()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-repository)
- [`Content$tag_delete()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-tag_delete)
- [`Content$tag_set()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-tag_set)
- [`Content$tags()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-tags)
- [`Content$update()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-update)
- [`Content$variants()`](https://posit-dev.github.io/connectapi/dev/reference/Content.html#method-variants)

------------------------------------------------------------------------

### `ContentTask$new()`

Initialize this task.

#### Usage

    ContentTask$new(connect, content, task)

#### Arguments

- `connect`:

  The `Connect` instance.

- `content`:

  The `Content` instance.

- `task`:

  The task data.

------------------------------------------------------------------------

### `ContentTask$get_task()`

Return the underlying task.

#### Usage

    ContentTask$get_task()

------------------------------------------------------------------------

### `ContentTask$add_data()`

Set the data.

#### Usage

    ContentTask$add_data(data)

#### Arguments

- `data`:

  The data.

------------------------------------------------------------------------

### `ContentTask$get_data()`

Get the data.

#### Usage

    ContentTask$get_data()

------------------------------------------------------------------------

### `ContentTask$print()`

Print this object.

#### Usage

    ContentTask$print(...)

#### Arguments

- `...`:

  Unused.

------------------------------------------------------------------------

### `ContentTask$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ContentTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
