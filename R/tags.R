
#' Get all Tags on the server
#' 
#' \lifecycle{experimental} Get a tibble of all tags
#' 
#' @param src The source object
#' @param .use_cache use the tag list previously queried 
#' from the connect server. If the `src$tags` object is null then
#' this parameter will be ignored
#' @param name The name of the tag to create
#' @param parent optional. A `connect_tag_tree` object (as returned by `get_tags()`) pointed at the parent tag
#' @param content An R6 Content object, as returned by `content_item()`
#' @param tag A `connect_tag_tree` object (as returned by `get_tags()`)
#' @param tags A `connect_tag_tree` object (as returned by `get_tags()`)
#' @param ids A list of `id`s to filter the tag tree by
#' @param ... Additional arguments
#' 
#' @export
#' @rdname tags
get_tags <- function(src){
  warn_experimental("get_tags")
  scoped_experimental_silence()
  validate_R6_class(src, "Connect")
  
  connect_tag_tree(tag_tree(src$get_tag_tree()), NULL)
}

#' @export
#' @rdname tags
get_tag_data <- function(src){
  warn_experimental("get_tag_data")
  scoped_experimental_silence()
  validate_R6_class(src, "Connect")
  
  res <- src$get_tag_tree()
  
  tag_tbl <- parse_tags_tbl(res)
  
  return(tag_tbl)
}

# TODO: Need to find a way to denote categories...?
# error  : chr "Cannot assign a category to an app"
# TODO: Need to protect against a bad data structure...
# TODO: possible that you could decouple this from a connect server and get strange results
#       (i.e. build a tag tree from server A, use it to "set_content_tags" for server B - ids would not match)
connect_tag_tree <- function(tag_data, filter = "filtered") {
  structure(tag_data, class = c("connect_tag_tree", "list"), filter = filter)
}

print.connect_tag_tree <- function(x, ...) {
  if (!is.null(attr(x, "filter"))) {
    cat(glue::glue("RStudio Connect Tag Tree ({attr(x, 'filter')})"))
    cat("\n")
  } else {
    cat("RStudio Connect Tag Tree\n")
  }
  if (length(x) > 0) {
    recursive_tag_print(x, "")
  } else {
    cat("  < No tags defined >")
  }
}

`$.connect_tag_tree` <- function(x,y){
  res <- NextMethod("$")
  if (is.list(res) && length(res) >= 2) {
    connect_tag_tree(res)
  } else {
   res 
  }
}

`[[.connect_tag_tree` <- function(x, ...) {
  res <- NextMethod("[[")
  if (is.list(res) && length(res) >= 2) {
    connect_tag_tree(res)
  } else {
   res 
  }
}

`[.connect_tag_tree` <- function(x, i, j) {
  res <- NextMethod("[")
  if (is.list(res) && length(res) >= 2) {
    connect_tag_tree(res)
  } else {
   res 
  }
}

# TODO: this is hard to "use" directly because what it returns is not a tag... maybe create a Tag R6 class?
#' @export
#' @rdname tags
create_tag <- function(src, name, parent = NULL) {
  warn_experimental("create_tag")
  validate_R6_class(src, "Connect")
  scoped_experimental_silence()
  if (is.null(parent) || is.numeric(parent)) {
    parent_id <- parent
  } else if (inherits(parent, "connect_tag_tree")) {
    parent_id <- parent[["id"]]
  } else {
    stop("`parent` must be an ID or a connect_tag_tree object")
  }
  res <- src$tag_create(name = name, parent_id = parent_id)
  return(src)
}

# TODO: try without quotes...
# TODO: do not fail if the key already exists...
#' @export
#' @rdname tags
create_tag_tree <- function(src, ..., .use_cache = TRUE) {
  warn_experimental("create_tag_tree")
  validate_R6_class(src, "Connect")
  scoped_experimental_silence()
  
  params <- rlang::list2(...)
  
  results <- purrr::reduce(
    params,
    function(.parent, .x, con) {
      res <- con$tag_create_safe(.x, .parent, use_cache = .use_cache)
      return(res[["id"]])
    },
    con = src,
    .init = NULL
  )
  filter_tag_tree(get_tags(src), results)
}

set_content_tag_tree <- function(content, ...) {
  # TODO: a way to set the tag for a content item
}

#' @export
#' @rdname tags
set_content_tags <- function(content, ...) {
  warn_experimental("set_content_tags")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  new_tags <- rlang::list2(...)
  tmp <- purrr::map(
    new_tags,
    function(.x) {
      ifelse(
        inherits(.x, "connect_tag_tree"), 
        content$tag_set(.x$id),
        content$tag_set(.x)
      )
    }
  )
  content
}

#' @export
#' @rdname tags
filter_tag_tree <- function(tags, ids) {
  warn_experimental("filter_tag_tree")
  scoped_experimental_silence()
  stopifnot(inherits(tags, "connect_tag_tree"))
  recursive_filter(tags = tags, ids = ids)
}

recursive_filter <- function(tags, ids) {
  tags_noname <- tags
  tags_noname$name <- NULL
  tags_noname$id <- NULL
  recurse_res <- purrr::map(tags_noname, ~ recursive_filter(.x, ids))
  rr_nonull <- purrr::keep(recurse_res, ~ !is.null(.x))
  if (tags$id %in% ids || length(rr_nonull) > 0) {
    connect_tag_tree(c(list(name = tags$name, id = tags$id), rr_nonull))
  } else {
    NULL
  }
}

recursive_find_tag <- function(tags, tag, parent_id = NULL) {
  tags_noname <- tags
  tags_noname$name <- NULL
  tags_noname$id <- NULL
  recurse_res <- purrr::map_dbl(tags_noname, ~ recursive_find_tag(.x, tag, parent_id))
  recurse_res_any <- recurse_res[!is.na(recurse_res)]
  if (length(recurse_res_any) == 0) {
    recurse_res_any <- NA_real_
  }
  names(recurse_res_any) <- NULL
  
  if (!is.na(recurse_res_any)) {
    recurse_res_any
  } else if (is.null(parent_id) && tags$name == tag) {
    res <- tags$id
    names(res) <- NULL
    res
  } else if (!is.null(parent_id) && tags$id == parent_id && tag %in% names(tags_noname)) {
    res <- tags[[tag]]$id
    names(res) <- NULL
    res
  } else {
    NA_real_
  }
}

#' @export
#' @rdname tags
get_content_tags <- function(content) {
  warn_experimental("get_content_tags")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  ctags <- content$tags()
  # TODO: find a way to build a tag tree from a list of tags
  
  tagtree <- get_tags(content$get_connect(), FALSE)
  res <- filter_tag_tree(tagtree, purrr::map_int(ctags, ~ .x$id))
  attr(res, "filter") <- "content"
  res
}

#' @export
#' @rdname tags
delete_tag <- function(src, tag) {
  warn_experimental("delete_tag")
  scoped_experimental_silence()
  if (is.numeric(tag)) {
    tag_id <- tag
  } else if (inherits(tag, "connect_tag_tree")) {
    tag_id <- tag[["id"]]
  } else {
    stop("`tag` must be an ID or a connect_tag_tree object")
  }
  res <- src$tag_delete(id = tag_id)
  return(src)
}

recursive_tag_print <- function(x, indent) {
  x_noname <- x
  x_noname$name <- NULL
  x_noname$id <- NULL
  ch <- box_chars()
  # print a "single level tag"
  if (length(x_noname) == 0 && nchar(indent) == 0) {
    if (!is.null(x$name)) {
      cat(indent, pc(ch$l, ch$h, ch$h, " "), x$name, "\n", sep = "")
    }
  }
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
