# TODO
# - next stop: vanity URLs?
# - figure out filtering... and such...?
# - draw diagram for understanding dbplyr execution
# - how does the op-list work... can you make "collect" happen before filter, mutate, and such?
# - need to make pagination actually work...
# - filters based on content_guid, started, ended, etc.
# - nrow should be super fast if we know how many total records there are...
# - show example usage...
#' Connect Tibble
#'
#' \lifecycle{experimental}
#' A lazy tibble that automatically pages through API requests when `collect`ed.
#'
#' @param src The source object
#' @param from The type of tibble
#' @param ... Additional arguments that are not yet implemented
#'
#' @return A `tbl_connect` object
#'
#' @export
tbl_connect <- function(src, from = c("users", "groups", "content", "usage_shiny", "usage_static", "audit_logs"), ...) {
  rlang::check_installed("dbplyr")

  validate_R6_class(src, "Connect")

  from <- match.arg(from)

  # TODO: go get the vars we should expect...
  vars <- connectapi_ptypes[[from]]
  if (is.null(vars)) vars <- character()

  # TODO: figure out number of rows...
  ops <- op_base_connect(from, vars)

  dplyr::make_tbl(c("connect", "lazy"), src = src, ops = ops)
}

# This will be registered in .onLoad is dplyr is available
collect.tbl_connect <- function(x, ..., n = Inf) {
  api_build(op = x[["ops"]], con = x[["src"]], n = n)
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
    res <- page_offset(con, con$users(), limit = n)
  } else if (op$x == "groups") {
    res <- page_offset(con, con$groups(), limit = n)
  } else if (op$x == "content") {
    # TODO: no limit notion here... we just pull all of them...
    res <- con$content()
  } else if (op$x == "usage_shiny") {
    res <- con$inst_shiny_usage(limit = n) %>% page_cursor(con, ., limit = n)
  } else if (op$x == "usage_static") {
    res <- con$inst_content_visits(limit = n) %>% page_cursor(con, ., limit = n)
  } else if (op$x == "audit_logs") {
    res <- con$audit_logs(limit = n) %>% page_cursor(con, ., limit = n)
  } else {
    stop(glue::glue("'{op$x}' is not recognized"))
  }
  parse_connectapi_typed(res, op$ptype)
}

cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

#' @importFrom utils head
#' @export
head.tbl_connect <- function(x, n = 6L, ...) {
  if (inherits(x[["ops"]], "op_head")) {
    x$ops$args$n <- min(x$ops$args$n, n)
  } else {
    x$ops <- op_single("head", x = x[["ops"]], args = list(n = n))
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
  as.data.frame(dplyr::collect(x, n = n))
}

op_base_connect <- function(x, vars) {
  op_base(x, vars, class = "connect")
}

op_base <- function(x, vars, class = character()) {
  stopifnot(is.character(vars) || is.character(names(vars)))

  structure(
    list(
      x = x,
      vars = names(vars),
      ptype = vars
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

connect_vars <- function(op) UseMethod("connect_vars")
#' @export
connect_vars.op_base <- function(op) op$vars
#' @export
connect_vars.op_single <- function(op) connect_vars(op$x)
#' @export
connect_vars.tbl_connect <- function(op) connect_vars(op[["ops"]])

# important for `nrow`/`ncol` to work
#' @export
dim.tbl_connect <- function(x) {
  c(NA, length(connect_vars(x[["ops"]])))
}

# important for `colnames` to work
#' @export
dimnames.tbl_connect <- function(x) {
  list(NULL, connect_vars(x[["ops"]]))
}
