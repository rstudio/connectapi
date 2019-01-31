#' Download a GitHub repository
#' 
#' Downloads a GitHub repository into a temporary directory
#' by downloading and extracting a tarball using a path like
#' https://api.github.com/repos/{repo}/tarball/{ref}
#' 
#' @param repo The GitHub repository to download
#' @param ref optional. The reference to download. Defaults to "master"
#' 
#' @return The path to the GitHub directory
#' 
#' @export
download_github <- function(repo, ref = "master") {
  current_wd <- getwd()
  on.exit(setwd(current_wd), add = TRUE)
  
  temp_dir <- fs::dir_create(fs::path(fs::path_temp(), fs::file_temp("dir_")), recursive = TRUE)
  tar_file <- fs::path(temp_dir, "repo.tar.gz")
  setwd(temp_dir)
  
  req <- httr::GET(
    glue::glue(
      "https://api.github.com/repos/{repo}/tarball/{ref}"
    ), 
    httr::write_disk(tar_file)
  )
  
  # un-tar and enter the repo
  untar("repo.tar.gz")
  setwd(fs::dir_ls(type = "directory"))
  
  final_loc <- getwd()
  
  return(final_loc)
}

#' Deploy a GitHub repository
#' 
#' Downloads and deploys a GitHub repository, using a 
#' standard YAML file for configuration.
#' 
#' @param connect A Connect client
#' @param repo The GitHub repository to download
#' @param ref optional. The reference to download. Defaults to "master"
#' @param filename optional. The name of the YAML file to use for configuration 
#' (inside the GitHub repository). Defaults to ".connect.yml"
#' 
#' @return A list. The result of the deployment process
#' 
#' @export
deploy_github <- function(connect, repo, ref = "master", filename = ".connect.yml") {
  download_dir <- download_github(repo = repo, ref = ref)
  current_wd <- getwd()
  on.exit(setwd(current_wd), add = TRUE)
  
  setwd(download_dir)
  yaml_content(connect = connect, filename = filename)
}
