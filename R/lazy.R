# TODO
# - trace through "n" to default to 10 or so (add a "head" op)
# - implement "actual" collect / handle n, and reduce # of sample rows
# - make operation dynamic (i.e. not just users)
# - figure out filtering... and such...?
# - how does the op-list work... can you make "collect" happen before filter, mutate, and such?
tbl_connect <- function(src, from, ...) {
  vars <- "test"
  
  ops <- op_base_connect(from, vars)
  
  dplyr::make_tbl(c("connect", "lazy"), src = src, ops = ops)
}

collect.tbl_connect <- function(x, ...) {
  res <- x$src$get_users()
  purrr::map_df(res$results, tibble::as_tibble)
}

cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

print.tbl_connect <- function(x, ..., n = NULL) {
  cat_line(format(x, ..., n = n))
  invisible(x)
}

as.data.frame.tbl_connect <- function(x, row.names = NULL, optional = NULL, ..., n = Inf) {
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
