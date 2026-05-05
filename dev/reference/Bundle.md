# Bundle

An R6 class that represents a bundle

## See also

Other R6 classes:
[`Content`](https://posit-dev.github.io/connectapi/dev/reference/Content.md),
[`ContentTask`](https://posit-dev.github.io/connectapi/dev/reference/ContentTask.md),
[`Environment`](https://posit-dev.github.io/connectapi/dev/reference/EnvironmentR6.md),
[`PositConnect`](https://posit-dev.github.io/connectapi/dev/reference/PositConnect.md),
[`Task`](https://posit-dev.github.io/connectapi/dev/reference/Task.md),
[`Vanity`](https://posit-dev.github.io/connectapi/dev/reference/Vanity.md),
[`Variant`](https://posit-dev.github.io/connectapi/dev/reference/VariantR6.md),
[`VariantSchedule`](https://posit-dev.github.io/connectapi/dev/reference/VariantSchedule.md),
[`VariantTask`](https://posit-dev.github.io/connectapi/dev/reference/VariantTask.md)

## Public fields

- `path`:

  The bundle path on disk.

- `size`:

  The size of the bundle.

## Methods

### Public methods

- [`Bundle$new()`](#method-Bundle-initialize)

- [`Bundle$print()`](#method-Bundle-print)

- [`Bundle$clone()`](#method-Bundle-clone)

------------------------------------------------------------------------

### `Bundle$new()`

Initialize this content bundle.

#### Usage

    Bundle$new(path)

#### Arguments

- `path`:

  The bundle path on disk.

------------------------------------------------------------------------

### `Bundle$print()`

Print this object.

#### Usage

    Bundle$print(...)

#### Arguments

- `...`:

  Unused.

------------------------------------------------------------------------

### `Bundle$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Bundle$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
