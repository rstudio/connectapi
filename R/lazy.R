# TODO
# - trace through "n" to default to 10 or so (add a "head" op)
# - implement "actual" collect / handle n, and reduce # of sample rows
# - make operation dynamic (i.e. not just users)
# - figure out filtering... and such...? 
# - draw diagram for understanding dbplyr execution
# - how does the op-list work... can you make "collect" happen before filter, mutate, and such?
# - some type of quoting here...?
#' @export
tbl_connect <- function(src, from, ...) {
  vars <- "test"
  
  ops <- op_base_connect(from, vars)
  
  dplyr::make_tbl(c("connect", "lazy"), src = src, ops = ops)
}

#' @importFrom dplyr collect
#' @export
collect.tbl_connect <- function(x, ..., n = Inf) {
  res <- x$src$get_users(page_size = n)
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
  print(capture.output(str(x$ops)))
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

