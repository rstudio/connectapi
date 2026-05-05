# Variant

An R6 class that represents a Variant

## See also

Other R6 classes:
[`Bundle`](https://posit-dev.github.io/connectapi/dev/reference/Bundle.md),
[`Content`](https://posit-dev.github.io/connectapi/dev/reference/Content.md),
[`ContentTask`](https://posit-dev.github.io/connectapi/dev/reference/ContentTask.md),
[`Environment`](https://posit-dev.github.io/connectapi/dev/reference/EnvironmentR6.md),
[`PositConnect`](https://posit-dev.github.io/connectapi/dev/reference/PositConnect.md),
[`Task`](https://posit-dev.github.io/connectapi/dev/reference/Task.md),
[`Vanity`](https://posit-dev.github.io/connectapi/dev/reference/Vanity.md),
[`VariantSchedule`](https://posit-dev.github.io/connectapi/dev/reference/VariantSchedule.md),
[`VariantTask`](https://posit-dev.github.io/connectapi/dev/reference/VariantTask.md)

## Super class

[`Content`](https://posit-dev.github.io/connectapi/dev/reference/Content.md)
-\> `Variant`

## Public fields

- `key`:

  The variant key.

- `variant`:

  The variant.

## Methods

### Public methods

- [`Variant$get_variant_remote()`](#method-Variant-get_variant_remote)

- [`Variant$new()`](#method-Variant-initialize)

- [`Variant$send_mail()`](#method-Variant-send_mail)

- [`Variant$get_schedule()`](#method-Variant-get_schedule)

- [`Variant$get_schedule_remote()`](#method-Variant-get_schedule_remote)

- [`Variant$get_subscribers()`](#method-Variant-get_subscribers)

- [`Variant$remove_subscriber()`](#method-Variant-remove_subscriber)

- [`Variant$add_subscribers()`](#method-Variant-add_subscribers)

- [`Variant$render()`](#method-Variant-render)

- [`Variant$renderings()`](#method-Variant-renderings)

- [`Variant$update_variant()`](#method-Variant-update_variant)

- [`Variant$jobs()`](#method-Variant-jobs)

- [`Variant$get_url()`](#method-Variant-get_url)

- [`Variant$get_url_rev()`](#method-Variant-get_url_rev)

- [`Variant$get_dashboard_url()`](#method-Variant-get_dashboard_url)

- [`Variant$print()`](#method-Variant-print)

- [`Variant$clone()`](#method-Variant-clone)

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

------------------------------------------------------------------------

### `Variant$get_variant_remote()`

Get the underlying variant data.

Get and store the (remote) variant data.

#### Usage

    Variant$get_variant_remote()

------------------------------------------------------------------------

### `Variant$new()`

Initialize this variant.

#### Usage

    Variant$new(connect, content, key)

#### Arguments

- `connect`:

  The `Connect` instance.

- `content`:

  The `Content` instance.

- `key`:

  The variant key.

------------------------------------------------------------------------

### `Variant$send_mail()`

Mail previously rendered content.

#### Usage

    Variant$send_mail(to = c("me", "collaborators", "collaborators_viewers"))

#### Arguments

- `to`:

  Targeting.

------------------------------------------------------------------------

### `Variant$get_schedule()`

Get the (remote) schedule data.

#### Usage

    Variant$get_schedule()

------------------------------------------------------------------------

### `Variant$get_schedule_remote()`

Get the (remote) schedule data.

#### Usage

    Variant$get_schedule_remote()

------------------------------------------------------------------------

### `Variant$get_subscribers()`

Get the subscribers.

#### Usage

    Variant$get_subscribers()

------------------------------------------------------------------------

### `Variant$remove_subscriber()`

Remove a named subscriber.

#### Usage

    Variant$remove_subscriber(guid)

#### Arguments

- `guid`:

  User GUID.

------------------------------------------------------------------------

### `Variant$add_subscribers()`

Add named subscribers.

#### Usage

    Variant$add_subscribers(guids)

#### Arguments

- `guids`:

  User GUIDs.

------------------------------------------------------------------------

### `Variant$render()`

Render this variant.

#### Usage

    Variant$render()

------------------------------------------------------------------------

### `Variant$renderings()`

List the renderings of this variant.

#### Usage

    Variant$renderings()

------------------------------------------------------------------------

### `Variant$update_variant()`

Update this variant.

#### Usage

    Variant$update_variant(...)

#### Arguments

- `...`:

  Target fields and values.

------------------------------------------------------------------------

### `Variant$jobs()`

Jobs for this variant.

#### Usage

    Variant$jobs()

------------------------------------------------------------------------

### `Variant$get_url()`

Return the URL for this variant.

#### Usage

    Variant$get_url()

------------------------------------------------------------------------

### `Variant$get_url_rev()`

Return the URL associated with one rendering for this variant.

#### Usage

    Variant$get_url_rev(rev)

#### Arguments

- `rev`:

  Rendering identifier.

------------------------------------------------------------------------

### `Variant$get_dashboard_url()`

Return the URL for this variant in the Posit Connect dashboard.

#### Usage

    Variant$get_dashboard_url(pane = "access")

#### Arguments

- `pane`:

  The pane in the dashboard to link to.

------------------------------------------------------------------------

### `Variant$print()`

Print this object.

#### Usage

    Variant$print(...)

#### Arguments

- `...`:

  Unused.

------------------------------------------------------------------------

### `Variant$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Variant$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
