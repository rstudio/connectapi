# Create a connection to Posit Connect

Creates a connection to Posit Connect using the server URL and an API
key. Validates the connection and checks that the version of the server
is compatible with the current version of the package.

## Usage

``` r
connect(
  server = Sys.getenv("CONNECT_SERVER"),
  api_key = Sys.getenv("CONNECT_API_KEY"),
  token,
  token_local_testing_key = api_key,
  audience = NULL,
  ...,
  .check_is_fatal = TRUE
)
```

## Arguments

- server:

  The URL for accessing Posit Connect. Defaults to environment variable
  CONNECT_SERVER

- api_key:

  The API Key to authenticate to Posit Connect with. Defaults to
  environment variable CONNECT_API_KEY

- token:

  Optional. A user session token. When running on a Connect server,
  creates a client using the content visitor's account. Running locally,
  the created client uses the provided API key.

- token_local_testing_key:

  Optional. Only used when not running on Connect and a `token` is
  provided. By default, the function returns a `Connect` object using
  the `api_key`. By providing a different key here you can test a
  visitor client with differently-scoped permissions.

- audience:

  Optional. The GUID of a Connect API integration associated with this
  piece of content.

- ...:

  Additional arguments. Not used at present

- .check_is_fatal:

  Whether to fail if "check" requests fail. Useful in rare cases where
  more http request customization is needed for requests to succeed.

## Value

A Posit Connect R6 object that can be passed along to methods

## Details

When running on Connect, the client's environment will contain default
`CONNECT_SERVER` and `CONNECT_API_KEY` variables. The API key's
permissions are scoped to the publishing user's account.

To create a client with permissions scoped to the content visitor's
account, call `connect()` passing a user session token from content
session headers to the `token` argument. To do this, you must first add
a Connect API integration in your published content's Access sidebar.

## Examples

``` r
if (FALSE) { # \dontrun{
client <- connect()

# Running in Connect, create a client using the content visitor's account.
# This example assumes code is being executed in a Shiny app's `server`
# function with a `session` object available.
token <- session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
client <- connect(token = token)

# Test locally with an API key using a different role.
fallback_key <- Sys.getenv("VIEWER_ROLE_API_KEY")
client <- connect(token = token, token_local_testing_key = fallback_key)
} # }


# default is to read CONNECT_SERVER and CONNECT_API_KEY environment variables
connect()
#> Defining Connect with server: http://localhost:3939
#> Posit Connect API Client: 
#>   Posit Connect Server: http://localhost:3939
#>   Posit Connect API Key: ***********CEBY
#> 
```
