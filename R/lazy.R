# TODO
# - make operation dynamic (i.e. not just users)
#     - next stop: vanity URLs?
# - make the op execution more "legit"
# - figure out filtering... and such...? 
# - draw diagram for understanding dbplyr execution
# - how does the op-list work... can you make "collect" happen before filter, mutate, and such?
# - some type of quoting here...?
# - need to make pagination actually work...
# - need to add a helper to filter to a number of pages
# - need to add a progress bar...
# - filters based on content_guid, started, ended, etc.
# - nrow should be super fast if we know how many total records there are...
#' @export
tbl_connect <- function(src, from = c("users", "shiny_usage", "content_visits"), ...) {
  validate_R6_class("Connect", src)
  
  # TODO: go get the vars we should expect...
  vars <- "test"
  
  # TODO: figure out number of rows...
  ops <- op_base_connect(from, vars)
  
  dplyr::make_tbl(c("connect", "lazy"), src = src, ops = ops)
}

vars_lookup <- list(
  users = c()
)

#' @importFrom dplyr collect
#' @export
collect.tbl_connect <- function(x, ..., n = Inf) {
  api_build(op = x$ops, con = x$src, n = n)
}

api_build <- function(op, con = NULL, ..., n = NULL) {
  UseMethod("api_build")
}

#' @export
api_build.op_head <- function(op, con, ..., n) {
  n <- op$args$n
  api_build(op$x, con, ..., n = n)
}

#' @export
api_build.op_base_connect <- function(op, con, ..., n) {
  if (op$x == "users") {
    res <- con$users(page_size = n)
  } else if (op$x == "data") {
    stop(glue::glue("'{op$x} is not yet implemented"))
  } else if (op$x == "content") {
    stop(glue::glue("'{op$x}' is not yet implemented"))
  } else if (op$x == "shiny_usage") {
    res <- con$inst_shiny_usage(limit = n)
  } else if (op$x == "content_visits") {
    res <- con$inst_content_visits(limit = n)
  } else {
    stop(glue::glue("'{op$x}' is not recognized"))
  }
  purrr::map_df(
    res$results, 
    function(x) {
      purrr::map(
        .x = x,
        .f = function(y) {
          purrr::pluck(y, .default = NA)
        }
      )
    }
  )
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

#' @export
op_vars <- function(op) UseMethod("op_vars")
#' @export
op_vars.op_base <- function(op) op$vars
#' @export
op_vars.op_single <- function(op) op_vars(op$x)
#' @export
op_vars.tbl_lazy <- function(op) op_vars(op$ops)

#' @export
dim.tbl_lazy <- function(x) {
  c(NA, length(op_vars(x$ops)))
}
