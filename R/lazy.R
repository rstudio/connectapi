# TODO
# - make operation dynamic (i.e. not just users)
#     - next stop: vanity URLs?
# - make the op execution more "legit"
# - figure out filtering... and such...? 
# - draw diagram for understanding dbplyr execution
# - how does the op-list work... can you make "collect" happen before filter, mutate, and such?
# - some type of quoting here...?
# - figure out why attaching dbplyr makes trunc_mat work...? what S3 class am I missing?
#' @export
tbl_connect <- function(src, from, ...) {
  vars <- "test"
  
  ops <- op_base_connect(from, vars)
  
  dplyr::make_tbl(c("connect", "lazy"), src = src, ops = ops)
}

#' @importFrom dplyr collect
#' @export
collect.tbl_connect <- function(x, ..., n = Inf) {
  if (x$ops$x == "users") {
    res <- x$src$get_users(page_size = n)
  } else if (x$ops$x == "data") {
    warning("not yet implemented")
  } else if (x$ops$x == "content") {
    warning("not yet implemented")
  } else if (x$ops$x == "inst_shiny") {
    res <- x$src$inst_shiny_usage(limit = n)
    # protect against NULL... ugly
    res$results <- purrr::map(
      res$results, 
      function(x){
        if (is.null(x$user_guid)){
          x$user_guid <- NA
        }
        return(x)
        })
  }
  purrr::map_df(res$results, tibble::as_tibble)
}

cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

#' @importFrom utils head
#' @export
head.tbl_connect <- function(x, n = 6L, ...) {
  if (inherits(x$ops, "op_head")) {
    x$ops$args$n <- min(x$ops$args$n, n)
  } else {
    x$ops <- op_single("head", x = x$ops, args = list(n = n))
  }
  x
}

#' @export
print.tbl_connect <- function(x, ..., n = NULL) {
  cat_line(format(x, ..., n = n))
  invisible(x)
}

#' @export
as.data.frame.tbl_connect <- function(x, row.names = NULL, optional = NULL, ..., n = Inf) {
  # handle "head" manually for now
  if (x$ops$name == "head") {
    n = x$ops$args$n
  }
  as.data.frame(collect(x, n = n))
}

op_base_connect <- function(x, vars) {
  op_base(x, vars, class = "connect")
}

op_base <- function(x, vars, class = character()) {
  stopifnot(is.character(vars))
  
  structure(
    list(
      x = x,
      vars = vars
    ),
    class = c(paste0("op_base_", class), "op_base", "op")
  )
}

op_single <- function(name, x, dots = list(), args = list()) {
  structure(
    list(
      name = name,
      x = x,
      dots = dots,
      args = args
    ),
    class = c(paste0("op_", name), "op_single", "op")
  )
}

