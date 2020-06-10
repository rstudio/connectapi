
#' Get all Tags on the server
#' 
#' \lifecycle{experimental} Get a tibble of all tags
#' 
#' @param src The source object
#' @param use_cache use the tag list previously queried 
#' from the connect server. If the `src$tags` object is null then
#' this parameter will be ignored
#' 
#' @return A tibble of id / name pairs
#' 
#' @export
get_tags <- function(src, use_cache = FALSE){
  validate_R6_class(src, "Connect")
  
  res <- src$get_tags(use_cache = use_cache)
  
  return(res)
}

#' Get a tibble with more information for each tag
#' 
#' @param src The source object
#' 
#' @export
get_tag_info <- function(src){
  validate_R6_class(src, "Connect")
  
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
  validate_R6_class(src, "Connect")
  
  res <- src$get_tag_tree()
  
  tag_tree(res, top_tag = top_tag)
}

tag_tree <- function(tags, top_tag = "tags"){
  parsed_tags <- parse_tags(tags, top_tag = top_tag)
  
  tag_split <- split(parsed_tags, dirname(parsed_tags))
  
  cat(top_tag(parsed_tags), "\n", sep = "")
  print_leaf(top_tag(parsed_tags), indent = "", tag_split = tag_split)
}

Tag <- R6::R6Class(
  "Tag",
  public = list(
    initialize = function(id, name, ...) {
      # could recurse in an R6 object... might lose autocomplete...?
    }
  )
)

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

ch <- box_chars()

print_leaf <- function(x, indent, tag_split) {
  leafs <- tag_split[[x]]
  for (i in seq_along(leafs)) {
    if (i == length(leafs)) {
      cat(indent, pc(ch$l, ch$h, ch$h, " "), basename(leafs[[i]]), "\n", sep = "")
      print_leaf(leafs[[i]], paste0(indent, "    "), tag_split = tag_split)
    } else {
      cat(indent, pc(ch$j, ch$h, ch$h, " "), basename(leafs[[i]]), "\n", sep = "")
      print_leaf(leafs[[i]], paste0(indent, pc(ch$v, "   ")), tag_split = tag_split)
    }
  }
}

print_leaf_new <- function(x, indent) {
  x_noname <- x
  x_noname$name <- NULL
  x_noname$id <- NULL
  purrr::map2(
    x_noname,
    seq_along(x_noname),
    function(.y, .i, list_length) {
      if (.i == list_length) {
        cat(indent, pc(ch$l, ch$h, ch$h, " "), .y$name, "\n", sep = "")
        print_leaf_new(.y, paste0(indent, "   "))
      } else {
        cat(indent, pc(ch$j, ch$h, ch$h, " "), .y$name, "\n", sep = "")
        print_leaf_new(.y, paste0(indent, pc(ch$v, "   ")))
      }
    },
    list_length = length(x_noname)
  )
  invisible(x)
}

recurse_print <- function(.x, ch, indent = " ") {
  cat(indent, pc(ch$l, ch$h, ch$h, " "), .x$name, "\n", sep = "")
  .x$name <- NULL
  .x$id <- NULL
  purrr::map2(
    .x,
    seq_along(.x),
    function(.y, .i, indent, llength) {
      trim_item <- .y
      trim_item$id <- NULL
      trim_item$name <- NULL
      if (.i == llength) {
        cat(indent, pc(ch$l, ch$h, ch$h, " "), .y$name, "\n", sep = "")
      } else {
        cat(indent, pc(ch$j, ch$h, ch$h, " "), .y$name, "\n", sep = "")
      }
      purrr::map(
        trim_item,
        function(.z) recurse_print(.z, ch = ch, indent = paste0(indent, "   "))
      )
    },
    indent = paste0(indent, "   "),
    llength = length(.x)
  )
  invisible(.x)
}

recurse_children <- function(.x) {
  if (length(.x$children) > 0) {
    rlang::set_names(list(c(purrr::flatten(purrr::map(.x$children, recurse_children)), id = .x$id, name = .x$name)), .x$name)
  } else {
    rlang::set_names(list(list(id = .x$id, name = .x$name)), .x$name)
  }
}

tag_tree_new <- function(.x) {
  purrr::flatten(purrr::map(.x, recurse_children))
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

parse_tags_new <- function(x, top_tag = "tags") {
  out <- parseTagsNew(x)
  
  out <- paste(top_tag, out, sep = "//")
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

