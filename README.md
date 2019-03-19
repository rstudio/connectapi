# connectapi - **WIP**

This package is an **experimental WIP**. The package is designed to provide an R
client for the RStudio Connect API as well as helpful functions that utilize the
client. The client is based off a similar client in the `rsconnnect` package,
but is publicly exported to be easier to use, is extensible via an R6 class, and
is separated from the `rsconnect` package for easier support and maintenance.

## Installation

To get started:

```r
devtools::install_github('rstudio/connectapi')
```

## Client

To create a client:

```r
library(connectapi)
client <- connect(
  host = 'https://connect.example.com',
  api_key = '<SUPER SECRET API KEY>'
)
```

You can also define the following environment variables (in a `.Renviron` file, for instance):

```
RSTUDIO_CONNECT_SERVER=https://connect.example.com
RSTUDIO_CONNECT_API_KEY=my-secret-api-key
```

These values will be used automatically if defined in your R session.

```r
library(connectapi)
client <- connect()
```

## Getting Started

Once a client is defined, you can use it to interact with RStudio Connect. For instance:

```r
# deploying content
bundle <- bundle_dir("./path/to/directory")
content <- client %>% deploy_bundle(bundle) %>% poll_task()

# set image for content
content %>% set_image_path("./my/local/image.png")
content %>% set_image_url("http://url.example.com/image.png")
```

## Utilities

There are also a handful of higher-level utility functions that use the client API. For example:

- `promote` Provides a method to programatically promote content from one Connect server to another, e.g. going from Dev to Prod.

- `tag_page` Creates an html landing page for content given a `tag` based on a simple template. This html page can be deployed to Connect or hosted on a different CMS portal.

- `audit_*` Functions for auditing different aspects of the
Connect server including: content that has open permissions,
what R versions are in use, what vanity urls are available, 
and non-default RunAs settings.

*More utilities will be added.*

