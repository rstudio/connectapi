# VariantTask

An R6 class that represents a Variant Task

## See also

Other R6 classes:
[`Bundle`](https://posit-dev.github.io/connectapi/dev/reference/Bundle.md),
[`Content`](https://posit-dev.github.io/connectapi/dev/reference/Content.md),
[`ContentTask`](https://posit-dev.github.io/connectapi/dev/reference/ContentTask.md),
[`Environment`](https://posit-dev.github.io/connectapi/dev/reference/EnvironmentR6.md),
[`PositConnect`](https://posit-dev.github.io/connectapi/dev/reference/PositConnect.md),
[`Task`](https://posit-dev.github.io/connectapi/dev/reference/Task.md),
[`Vanity`](https://posit-dev.github.io/connectapi/dev/reference/Vanity.md),
[`Variant`](https://posit-dev.github.io/connectapi/dev/reference/VariantR6.md),
[`VariantSchedule`](https://posit-dev.github.io/connectapi/dev/reference/VariantSchedule.md)

## Super classes

[`Content`](https://posit-dev.github.io/connectapi/dev/reference/Content.md)
-\>
[`Variant`](https://posit-dev.github.io/connectapi/dev/reference/VariantR6.md)
-\> `VariantTask`

## Public fields

- `task`:

  The task.

- `data`:

  The variant data.

## Methods

### Public methods

- [`VariantTask$new()`](#method-VariantTask-initialize)

- [`VariantTask$get_task()`](#method-VariantTask-get_task)

- [`VariantTask$add_data()`](#method-VariantTask-add_data)

- [`VariantTask$get_data()`](#method-VariantTask-get_data)

- [`VariantTask$print()`](#method-VariantTask-print)

- [`VariantTask$clone()`](#method-VariantTask-clone)

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
- [`Variant$add_subscribers()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-add_subscribers)
- [`Variant$get_dashboard_url()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-get_dashboard_url)
- [`Variant$get_schedule()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-get_schedule)
- [`Variant$get_schedule_remote()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-get_schedule_remote)
- [`Variant$get_subscribers()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-get_subscribers)
- [`Variant$get_url()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-get_url)
- [`Variant$get_url_rev()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-get_url_rev)
- [`Variant$get_variant_remote()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-get_variant_remote)
- [`Variant$jobs()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-jobs)
- [`Variant$remove_subscriber()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-remove_subscriber)
- [`Variant$render()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-render)
- [`Variant$renderings()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-renderings)
- [`Variant$send_mail()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-send_mail)
- [`Variant$update_variant()`](https://posit-dev.github.io/connectapi/dev/reference/Variant.html#method-update_variant)

------------------------------------------------------------------------

### `VariantTask$new()`

Initialize this variant task.

#### Usage

    VariantTask$new(connect, content, key, task)

#### Arguments

- `connect`:

  The `Connect` instance.

- `content`:

  The `Content` instance.

- `key`:

  The variant key.

- `task`:

  The task data.

------------------------------------------------------------------------

### `VariantTask$get_task()`

Return the underlying task.

#### Usage

    VariantTask$get_task()

------------------------------------------------------------------------

### `VariantTask$add_data()`

Set the data.

#### Usage

    VariantTask$add_data(data)

#### Arguments

- `data`:

  The data.

------------------------------------------------------------------------

### `VariantTask$get_data()`

Get the data.

#### Usage

    VariantTask$get_data()

------------------------------------------------------------------------

### `VariantTask$print()`

Print this object.

#### Usage

    VariantTask$print(...)

#### Arguments

- `...`:

  Unused.

------------------------------------------------------------------------

### `VariantTask$clone()`

The objects of this class are cloneable with this method.

#### Usage

    VariantTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
