
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
#' @rdname tags
get_tags <- function(src, use_cache = FALSE){
  validate_R6_class(src, "Connect")
  
  connect_tag_tree(tag_tree(src$get_tag_tree()))
}

#' @export
#' @rdname tags
get_tag_data <- function(src){
  validate_R6_class(src, "Connect")
  
  res <- src$get_tag_tree()
  
  tag_tbl <- parse_tags_tbl(res)
  
  return(tag_tbl)
}

connect_tag_tree <- function(tag_data) {
  structure(tag_data, class = c("connect_tag_tree", "list"))
}

print.connect_tag_tree <- function(x, ...) {
  cat("RStudio Connect Tag Tree\n")
  if (length(x) > 0) {
    recursive_tag_print(x, "")
  } else {
    cat("  < No tags defined >")
  }
}

`$.connect_tag_tree` <- function(x,y){
  out <- NextMethod("$")
  if (is.list(out) && length(out) > 2) {
    connect_tag_tree(out)
  } else {
    out
  }
}

create_tag <- function(client, name, parent = NULL) {
  warn_experimental("create_tag")
  scoped_experimental_silence()
  if (is.numeric(parent)) {
    parent_id <- parent
  } else if (inherits(parent, "connect_tag_tree")) {
    parent_id <- parent[["id"]]
  } else {
    stop("`parent` must be an ID or a connect_tag_tree object")
  }
  res <- client$tag_create(name = name, parent_id = parent_id)
  return(client)
}

create_tag_tree <- function(client, ...) {
  # TODO: a way to create a tag tree or many tags at once
}

set_content_tag_tree <- function(content, ...) {
  # TODO: a way to set the tag for a content item
}

set_content_tags <- function(content, ...) {
  # TODO: set tags for a content item
}

get_content_tags <- function(content) {
  # TODO: get tags for a content item
}

delete_tag <- function(client, tag) {
  if (is.numeric(tag)) {
    tag_id <- tag
  } else if (inherits(tag, "connect_tag_tree")) {
    tag_id <- tag[["id"]]
  } else {
    stop("`tag` must be an ID or a connect_tag_tree object")
  }
  res <- client$tag_delete(id = tag_id)
  return(client)
}

recursive_tag_print <- function(x, indent) {
  x_noname <- x
  x_noname$name <- NULL
  x_noname$id <- NULL
  ch <- box_chars()
  purrr::map2(
    x_noname,
    seq_along(x_noname),
    function(.y, .i, list_length) {
      if (.i == list_length) {
        cat(indent, pc(ch$l, ch$h, ch$h, " "), .y$name, "\n", sep = "")
        recursive_tag_print(.y, paste0(indent, "   "))
      } else {
        cat(indent, pc(ch$j, ch$h, ch$h, " "), .y$name, "\n", sep = "")
        recursive_tag_print(.y, paste0(indent, pc(ch$v, "   ")))
      }
    },
    list_length = length(x_noname)
  )
  invisible(x)
}

recursive_tag_restructure <- function(.x) {
  if (length(.x$children) > 0) {
    rlang::set_names(list(c(purrr::flatten(purrr::map(.x$children, recursive_tag_restructure)), id = .x$id, name = .x$name)), .x$name)
  } else {
    rlang::set_names(list(list(id = .x$id, name = .x$name)), .x$name)
  }
}

tag_tree <- function(.x) {
  purrr::flatten(purrr::map(.x, recursive_tag_restructure))
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
