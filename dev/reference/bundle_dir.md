# Define a bundle from a Directory

Creates a bundle from a target directory.

## Usage

``` r
bundle_dir(
  path = ".",
  filename = fs::file_temp(pattern = "bundle", ext = ".tar.gz")
)
```

## Arguments

- path:

  The path to the directory to be bundled

- filename:

  The output bundle path

## Value

Bundle A bundle object

## See also

Other deployment functions:
[`bundle_path()`](https://posit-dev.github.io/connectapi/dev/reference/bundle_path.md),
[`bundle_static()`](https://posit-dev.github.io/connectapi/dev/reference/bundle_static.md),
[`deploy()`](https://posit-dev.github.io/connectapi/dev/reference/deploy.md),
[`download_bundle()`](https://posit-dev.github.io/connectapi/dev/reference/download_bundle.md),
[`poll_task()`](https://posit-dev.github.io/connectapi/dev/reference/poll_task.md)

## Examples

``` r

bundle_dir(system.file("tests/testthat/examples/shiny/", package = "connectapi"))
#> Error in bundle_dir(system.file("tests/testthat/examples/shiny/", package = "connectapi")): fs::dir_exists(path) is not TRUE
```
