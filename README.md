# Connect API Utils - **WIP**

This package is an **experimental WIP**. The package is designed to provide an R client for the RStudio Connect API as well as helpful functions that utilize the client. The client is based off a similar client in the `rsconnnect` package, but is publicly exported to be easier to use, is extensible via an R6 class, and is separated from the `rsconnect` package for easier support and maintenance. 

## Installation

To get started:

```r
devtools::install_github('slopp/connectApiUtils')
```

## Client

To create a client:

```r
library(connectApiUtils)
client <- Connect$new(
  host = 'https://connect.example.com',
  api_key = '<SUPER SECRET API KEY>'
)
``` 

Once a client is created there are a handful of useful functions:

- `get_apps` Returns a list with information on available applications
- `get_apps(filter = list(name = app_name))` Filters the list of applications by the `key=value` pair supplied to `filter` as a named list, e.g: `list(tag = tag_id)`
- `get_tags` Returns a data frame with columns `tag_id` and `tag_name`

To see all the functions, visit the documentation for the `Connect` class. **More functions will be added**.

## Utilities

There are also a handful of higher-level utility functions that use the client API. For example:

- `promote` Provides a method to programatically promote content from one Connect server to another, e.g. going from Dev to Prod.

- `tag_page` Creates an html landing page for content given a `tag` based on a simple template. This html page can be deployed to Connect or hosted on a different CMS portal.

- `audit_*` Functions for auditing different aspects of the
Connect server including: content that has open permissions,
what R versions are in use, what vanity urls are available, 
and non-default RunAs settings.

*More utilities will be added.*

