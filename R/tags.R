get_tags <- function(src, use_cache = FALSE){
  validate_R6_class("Connect", src)
  
  res <- src$get_tags(use_cache = use_cache)
  
  return(res)
}







get_tag_tree(src){
  validate_R6_class("Connect", src)
}



tag_tree <- function(tags, top_tag = "tags"){
  parsed_tags <- parse_tags(tags, top_tag = top_tag)
  
  tag_split <- split(parsed_tags, tag_ancestor(parsed_tags))
  names(tag_split) <- gsub("/$", "", names(tag_split))
  
  ch <- fs:::box_chars()
  
  print_leaf <- function(x, indent) {
    # browser()
    leafs <- tag_split[[x]]
    for (i in seq_along(leafs)) {
      if (i == length(leafs)) {
        cat(indent, fs:::pc(ch$l, ch$h, ch$h, " "), basename(leafs[[i]]), "\n", sep = "")
        print_leaf(leafs[[i]], paste0(indent, "    "))
      } else {
        cat(indent, fs:::pc(ch$j, ch$h, ch$h, " "), basename(leafs[[i]]), "\n", sep = "")
        print_leaf(leafs[[i]], paste0(indent, fs:::pc(ch$v, "   ")))
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
    if (!is.null(.x$children)){
      child_names <- parseTags(.x$children)
      child_names <- paste(out, child_names, sep = "/")
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
  
  out <- paste(top_tag, out, sep = "/")
  return(out)
}

tag_ancestor <- function(x){
  # stringr::str_extract(x, "^[^/]+")
  out <- stringr::str_extract(x, "^(.*[\\/])")
  out <- ifelse(is.na(out), paste0(x, "/"), out)
  # if (is.na(out)) out <- x
  return(out)
}

top_tag <- function(x){
  unique(stringr::str_extract(x, "^[^/]+"))
}
