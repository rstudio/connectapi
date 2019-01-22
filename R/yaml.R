yaml_content <- function(filename = ".connect.yml") {
  cfg <- config::get(value = "content", file = filename)
  return(cfg)
}

yaml_content_deploy <- function(
  connect,
  name,
  path = "./",
  description = NULL,
  tag = NULL,
  url = NULL,
  image = NULL,
  ...
) {
  bundle_path <- dir_bundle(path = path)
  
  c_obj <- rlang::exec(
    content_ensure,
    connect = connect,
    name = name,
    description = description
    ...
  )
  
  c_guid <- c_obj[["guid"]]
  
  c_upload <- connect$content_upload(
    bundle_path = bundle_path,
    guid = c_guid
    )
  
  # deploy
  
  
  # tag helper
  if (!is.null(tag)) {
    
  }
  
  # set vanity URL
  if (!is.null(url)) {
    
  }
  
  # set image path
  if (!is.null(image)) {
    
  }
}
