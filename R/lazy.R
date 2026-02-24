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
#' `r lifecycle::badge('experimental')`
#' A lazy tibble that automatically pages through API requests when `collect`ed.
#'
#' @param src The source object
#' @param from The type of tibble
#' @param ... Additional arguments that are not yet implemented
#'
#' @return A `tbl_connect` object
#'
#' @export
tbl_connect <- function(
  src,
  from = c(
    "users",
    "groups",
    "content",
    "usage_shiny",
    "usage_static",
    "audit_logs"
  ),
  ...
) {
  rlang::check_installed("dbplyr")

  validate_R6_class(src, "Connect")

  from <- arg_match(from)

  # Discover column names from a small API request rather than maintaining a
  # hardcoded dictionary that must be updated every time the server changes.
  vars <- tryCatch(
    names(tbl_lazy_fetch(src, from, limit = 1)),
    error = function(e) character()
  )

  # TODO: figure out number of rows...
  ops <- op_base_connect(from, vars)

  dplyr::make_tbl(c("connect", "lazy"), src = src, ops = ops)
}

# Datetime columns for each lazy table type. These must match the
# datetime_cols passed in the corresponding getter functions:
# get_users(), get_groups(), get_content(), get_usage_shiny(),
# get_usage_static(), get_audit_logs().
lazy_datetime_cols <- list(
  users = c("created_time", "updated_time", "active_time"),
  groups = character(),
  content = c("created_time", "last_deployed_time"),
  usage_shiny = c("started", "ended"),
  usage_static = "time",
  audit_logs = "time"
)

# Fetch data for a lazy table endpoint. Shared by tbl_connect (for column
# discovery) and api_build.op_base_connect (for full collection).
tbl_lazy_fetch <- function(src, from, limit = Inf) {
  if (from == "users") {
    res <- page_offset(src, src$users(), limit = limit)
  } else if (from == "groups") {
    res <- page_offset(src, src$groups(), limit = limit)
  } else if (from == "content") {
    # TODO: no limit notion here... we just pull all of them...
    res <- src$content()
  } else if (from == "usage_shiny") {
    res <- src$inst_shiny_usage(limit = limit) %>%
      page_cursor(src, ., limit = limit)
  } else if (from == "usage_static") {
    res <- src$inst_content_visits(limit = limit) %>%
      page_cursor(src, ., limit = limit)
  } else if (from == "audit_logs") {
    res <- src$audit_logs(limit = limit) %>%
      page_cursor(src, ., limit = limit)
  } else {
    stop(glue::glue("'{from}' is not recognized"))
  }
  parse_connectapi_typed(res, datetime_cols = lazy_datetime_cols[[from]])
}

# This will be registered in .onLoad if dplyr is available
collect.tbl_connect <- # nolint
  function(x, ..., n = Inf) {
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
  tbl_lazy_fetch(con, op$x, limit = n)
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
as.data.frame.tbl_connect <- function(
  x,
  row.names = NULL, # nolint
  optional = NULL,
  ...,
  n = Inf
) {
  # We don't need to check if dplyr is available here
  # because you won't have a tbl_connect without first
  # checking for dplyr.
  as.data.frame(dplyr::collect(x, n = n))
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
