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

deploy_github <- function(connect, repo, ref = "master", filename = ".connect.yml") {
  download_dir <- download_github(repo = repo, ref = ref)
  current_wd <- getwd()
  on.exit(setwd(current_wd), add = TRUE)
  
  setwd(download_dir)
  yaml_content(connect = connect, filename = filename)
}
