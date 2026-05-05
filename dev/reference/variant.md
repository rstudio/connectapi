# Get Variant

**\[experimental\]** Work with variants

## Usage

``` r
get_variants(content)

get_variant(content, key)

get_variant_default(content)
```

## Arguments

- content:

  An R6 Content object. Returned from
  [`content_item()`](https://posit-dev.github.io/connectapi/dev/reference/content_item.md)

- key:

  The Variant key for a specific variant

## Details

- `get_variants()` returns a `tibble` with variant data for a
  `content_item`

- `get_variant_default()` returns the default variant for a
  `content_item`

- `get_variant()` returns a specific variant for a `content_item`
  (specified by `key`)

## See also

Other variant functions:
[`get_variant_renderings()`](https://posit-dev.github.io/connectapi/dev/reference/variant_render.md)
