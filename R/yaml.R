yaml_template <- function(file = NULL){
  obj <- list(
    "default" = list(
      "content" = list(
        list(
          "title" = "Title of the Content",
          "path" = "./",
          "description" = "Content description"
        )
      )
    )
  )
  
  if (!is.null(file)) {
    yaml::write_yaml(x = obj, file = file)
  } else {
    return(cat(yaml::as.yaml(obj)))
  }
}

#' @export
yaml_content <- function(connect, filename = ".connect.yml") {
  cfg <- config::get(value = "content", file = filename)
  
  res <- lapply(
    cfg,
    function(x, connect) {
      rlang::exec(yaml_content_deploy, connect = connect, !!!x)
    },
    connect = connect
  )
  
  return(res)
}

#' @export
yaml_content_deploy <- function(
  connect,
  name = random_name(),
  path = "./",
  description = NULL,
  tag = NULL,
  url = NULL,
  image = NULL,
  ...
) {
  #orig_connect <- connect
  #connect <- connect_input(connect)
  bundle_path <- dir_bundle(path = path)
  
  c_obj <- rlang::exec(
    content_ensure,
    connect = connect,
    name = name,
    description = description,
    ...
  )
  
  c_guid <- c_obj[["guid"]]
  
  c_upload <- connect$content_upload(
    bundle_path = bundle_path,
    guid = c_guid
    )
  
  c_task <- connect$content_deploy(
    guid = c_guid,
    bundle_id = c_upload
  )
  
  # wait for task to complete
  poll_task(
    connect,
    c_task
  )
  
  # tag helper
  if (!is.null(tag)) {
    # need public APIs
  }
  
  # set vanity URL
  if (!is.null(url)) {
    # need public APIs
  }
  
  # set image path
  if (!is.null(image)) {
    # need public APIs
  }
}
