# deploy the current_bundle
deploy_current <- function(content) {
  res <- content$get_connect()$POST(glue::glue("v1/experimental/content/{content$get_content()$guid}/deploy"), body = list(bundle_id = content$get_content()$bundle_id))
  return(ContentTask$new(connect = content$get_connect(), content = content$get_content(), task = res$task_id))
}
