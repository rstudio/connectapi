# Lifecycle Statuses

``` r

lifecycle::pkg_lifecycle_statuses("connectapi") %>%
  distinct(package, fun, lifecycle) %>%
  rmarkdown::paged_table()
```
