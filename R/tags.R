
#' Get a simple data.frame listing all Tags on server
#' 
#' @param src The source object
#' @param use_cache use the tag list previously queried 
#' from the connect server. src$tags object is null then
#' this parameter will be ignored
#' 
#' @export
get_tags <- function(src, use_cache = FALSE){
  validate_R6_class("Connect", src)
  
  res <- src$get_tags(use_cache = use_cache)
  
  return(res)
}

#' Get a tibble with more information for each tag
#' 
#' @param src The source object
#' 
#' @export
get_tag_info <- function(src){
  validate_R6_class("Connect", src)
  
  res <- src$get_tag_tree()
  
  tag_tbl <- parse_tags_tbl(res)
  
  return(tag_tbl)
}

#' Print a visualization of the tag tree
#' 
#' @param src The source object
#' @param top_tag The value to be printed at the top of the 
#' tag tree. This value can not have any "/" characters in it. 
#' 
#' @export
get_tag_tree <- function(src, top_tag = "RStudio Connect Tags"){
  validate_R6_class("Connect", src)
  
  res <- src$get_tag_tree()
  
  tag_tree(res, top_tag = top_tag)
}

tag_tree <- function(tags, top_tag = "tags"){
  parsed_tags <- parse_tags(tags, top_tag = top_tag)
  
  tag_split <- split(parsed_tags, dirname(parsed_tags))
  
  ch <- box_chars()
  
  print_leaf <- function(x, indent) {
    leafs <- tag_split[[x]]
    for (i in seq_along(leafs)) {
      if (i == length(leafs)) {
        cat(indent, pc(ch$l, ch$h, ch$h, " "), basename(leafs[[i]]), "\n", sep = "")
        print_leaf(leafs[[i]], paste0(indent, "    "))
      } else {
        cat(indent, pc(ch$j, ch$h, ch$h, " "), basename(leafs[[i]]), "\n", sep = "")
        print_leaf(leafs[[i]], paste0(indent, pc(ch$v, "   ")))
      }
    }
  }
  
  cat(top_tag(parsed_tags), "\n", sep = "")
  print_leaf(top_tag(parsed_tags), indent = "")
}


parseTags <- function(x){
  parsed_tags <- purrr::map(x, ~{
    out <- .x$name
    # out <- paste(top_tag, out, sep = "/")
    if (length(.x$children) > 0){
      child_names <- parseTags(.x$children)
      child_names <- paste(out, child_names, sep = "//")
      out <- c(out, child_names)
    } 
    
    return(out)
  })
  
  parsed_tags <- purrr::flatten_chr(parsed_tags)
  out <- gsub("/$", "", parsed_tags)
  
  return(out)
}

parse_tags <- function(x, top_tag = "tags"){
  out <- parseTags(x)
  
  out <- paste(top_tag, out, sep = "//")
  return(out)
}

top_tag <- function(x){
  unique(gsub("\\/.*$", "", x))
}


parse_tags_tbl <- function(x){
  parsed_tags <- purrr::map_dfr(x, ~{
    out <- dplyr::tibble(
      id = .x$id,
      name = .x$name,
      created_time = .x$created_time,
      updated_time = .x$updated_time,
      version = .x$version,
      parent_id = ifelse(is.null(.x$parent_id), NA_integer_, .x$parent_id)
    )
    
    if (length(.x$children) > 0){
      child <- parse_tags_tbl(.x$children)
      out <- dplyr::bind_rows(out, child)
    }
    
    return(out)
  })
  
  return(parsed_tags)
}


# HELPER FUNCTIONS FOR tag_tree FROM fs
pc <- function(...) {
  paste0(..., collapse = "")
}

# These are derived from https://github.com/r-lib/cli/blob/e9acc82b0d20fa5c64dd529400b622c0338374ed/R/tree.R#L111
box_chars <- function() {
  if (is_utf8_output()) {
    list(
      "h" = "\u2500",                   # horizontal
      "v" = "\u2502",                   # vertical
      "l" = "\u2514",
      "j" = "\u251C"
    )
  } else {
    list(
      "h" = "-",                        # horizontal
      "v" = "|",                        # vertical
      "l" = "\\",
      "j" = "+"
    )
  }
}

is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) return(FALSE)
  get("is_latex_output", asNamespace("knitr"))()
}

is_utf8_output <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (! is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is_latex_output()
  }
}
