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
  validate_R6_class(src, "Connect")
  
  stopifnot(length(from) == 1)
  if (!from %in% c("users", "groups", "content", "usage_shiny", "usage_static", "audit_logs", deprecated_names))
    stop(glue::glue("ERROR: invalid table name: {from}"))
  
  from <- check_deprecated_names(from)
  
  # TODO: go get the vars we should expect...
  vars <- connectapi_ptypes[[from]]
  if (is.null(vars)) vars <- character()
  
  # TODO: figure out number of rows...
  ops <- op_base_connect(from, vars)
  
  dplyr::make_tbl(c("connect", "lazy"), src = src, ops = ops)
}

deprecated_names <- c(
  usage_shiny = "shiny_usage",
  usage_static = "content_visits"
)

check_deprecated_names <- function(.name, deprecated_names) {
  if (.name == "shiny_usage") {
    warning("`shiny_usage` is deprecated. Please use `usage_shiny`")
    .name <- "usage_shiny"
  }
  if (.name == "content_visits") {
    warning("`content_visits` is deprecated. Please use `usage_static`")
    .name <- "usage_static"
  }
  return(.name)
}

NA_datetime_ <- vctrs::new_datetime(NA_real_)

connectapi_ptypes <- list(
  users = tibble::tibble(
    "email" = NA_character_,
    "username" = NA_character_,
    "first_name" = NA_character_,
    "last_name" = NA_character_,
    "user_role" = NA_character_,
    "created_time" = NA_datetime_,
    "updated_time" = NA_datetime_,
    "active_time" = NA_datetime_,
    "confirmed" = NA,
    "locked" = NA,
    "guid" = NA_character_
  ),
  usage_shiny = tibble::tibble(
    "content_guid" = NA_character_,
    "user_guid" = NA_character_,
    "started" = NA_datetime_,
    "ended" = NA_datetime_,
    "data_version" = NA_integer_
  ),
  usage_static = tibble::tibble(
    "content_guid" = NA_character_,
    "user_guid" = NA_character_,
    "variant_key" = NA_character_,
    "time" = NA_datetime_,
    "rendering_id" = NA_character_,
    "bundle_id" = NA_character_,
    "data_version" = NA_integer_
  )
  
)


vars_lookup <- list(
  users = c(
    "email",
    "username",
    "first_name",
    "last_name",
    "user_role",
    "created_time",
    "updated_time",
    "active_time",
    "confirmed",
    "locked",
    "guid"
  ),
  groups = c(
    "guid",
    "name",
    "owner_guid"
  ),
  shiny_usage = c(
    "content_guid",
    "user_guid",
    "started",
    "ended",
    "data_version"
  ),
  content_visits = c(
    "content_guid",
    "user_guid",
    "variant_key",
    "time",
    "rendering_id",
    "bundle_id",
    "data_version"
  ),
  content = c(
    "id",
    "guid",
    "access_type",
    "connection_timeout",
    "read_timeout",
    "init_timeout",
    "idle_timeout",
    "max_processes",
    "min_processes",
    "max_conns_per_process",
    "load_factor",
    "url",
    "vanity_url",
    "name",
    "title",
    "bundle_id",
    "app_mode",
    "content_category",
    "has_parameters",
    "created_time",
    "last_deployed_time",
    "r_version",
    "py_version",
    "build_status",
    "run_as",
    "run_as_current_user",
    "description",
    "app_role",
    "owner_first_name",
    "owner_last_name",
    "owner_username",
    "owner_guid",
    "owner_email",
    "owner_locked",
    "is_scheduled",
    "git"
  ),
  audit_logs = c(
    "id",
    "time",
    "user_id",
    "user_description",
    "action",
    "event_description"
  )
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
    res <- con$users(page_size = n) %>% .$results
    if (length(res) >= 400) {
      warning("The 'users' tbl_connect does not page and will return max 500 users")
    }
  } else if (op$x == "groups") {
    res <- con$groups(page_size = n) %>% .$results
    if (length(res) >= 400) {
      warning("The 'groups' tbl_connect does not page and will return max 500 users")
    }
  } else if (op$x == "content") {
    warn_experimental("tbl_connect 'content'")
    res <- con$get_apps(.limit = n)
  } else if (op$x == "usage_shiny") {
    res <- con$inst_shiny_usage(limit = n) %>% page_cursor(con, ., limit = n)
  } else if (op$x == "usage_static") {
    res <- con$inst_content_visits(limit = n) %>% page_cursor(con, ., limit = n)
  } else if (op$x == "audit_logs") {
    res <- con$audit_logs(limit = n) %>% page_cursor(con, ., limit = n) 
  } else {
    stop(glue::glue("'{op$x}' is not recognized"))
  }
  purrr::map_df(
    res, 
    function(x) {
      purrr::map(
        .x = x,
        .f = function(y) {
          prep <- purrr::pluck(y, .default = NA)
          # TODO: Should figure out what we want to do about sub-objects...
          # i.e. content: git details... could build a nested list...?
          if (length(prep) > 1)
            prep <- NA
          return(prep)
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

# #' @export
op_vars <- function(op) UseMethod("op_vars")
#' @export
op_vars.op_base <- function(op) op$vars
#' @export
op_vars.op_single <- function(op) op_vars(op$x)
#' @export
op_vars.tbl_lazy <- function(op) op_vars(op$ops)

# important for `nrow`/`ncol` to work
#' @export
dim.tbl_lazy <- function(x) {
  c(NA, length(op_vars(x$ops)))
}

# important for `colnames` to work
#' @export
dimnames.tbl_lazy <- function (x) 
{
  list(NULL, op_vars(x$ops))
}
