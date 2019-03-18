hacky_first_admin <- function(host, user, password, email = user) {
  # need to create the user first (password auth, etc.)
  res <- httr::POST(
    glue::glue("{host}/__api__/v1/users"), 
    body = list(email = email, username = user, password = password), 
    encode = "json"
  )
  if (res$status_code != 200) {
    stop("Creating the first admin user failed")
  }
  
  # login with the password
  res_login <- httr::POST(
    glue::glue("{host}/__login__"), 
    body = list(username = user, password = password), 
    encode = "json"
  )
  
  cookie_jar <- httr::cookies(res_login)
  
  xsrf_info <- cookie_jar[cookie_jar$name == "RSC-XSRF",]
  
  res_api_key <- httr::POST(
    glue::glue("{host}/__api__/keys"),
    httr::add_headers("X-RSC-XSRF" = xsrf_info$value[[1]]),
    body = list(name = "programmatic-setup-key"),
    encode = "json"
  )
  
  connect(host = host, api_key = httr::content(res_api_key)$key)
}
