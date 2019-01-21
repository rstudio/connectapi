yaml_content <- function(filename = ".connect.yml") {
  cfg <- config::get(value = "content", file = filename)
}

yaml_content_deploy <- function(
  name,
  path = "./",
  description = NULL,
  tag = NULL,
  url = NULL,
  image = NULL
) {
  bundle_path <- dir_bundle(path = path)
  
  # create_and_upload
  
  # should have a guid
  
  # tag helper
  
  # set vanity URL
  
  # set image path
  
}
