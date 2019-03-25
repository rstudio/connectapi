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
library(connectapi)
client <- connect()

# deploying content
bundle <- bundle_dir("./path/to/directory")
content <- client %>% deploy(bundle, name = "my-app-name") %>% poll_task()

# set an image for content
content %>% set_image_path("./my/local/image.png")
content %>% set_image_url("http://url.example.com/image.png")

# set image and a vanity URL
content %>%
  set_image_path("./my/local/image.png") %>%
  set_vanity_url("/my-awesome-app")
  
# edit another piece of content
client %>%
  content_item("the-content-guid") %>%
  set_vanity_url("/another-awesome-app")
  
# migrate content to another server
client_prod <- connect(
  host = "prod.example.com",
  api_key = "my-secret-key"
)

prod_bnd <- client %>%
  content_item("the-guid-to-promote") %>%
  download_bundle()

client_prod %>%
  deploy(prod_bnd, title = "Now in Production") %>%
  set_vanity_url("/my-app")
```
