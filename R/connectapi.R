#' @importFrom utils compareVersion untar
#' @importFrom lifecycle deprecate_warn
#' @importFrom rlang arg_match
"_PACKAGE"

# Even if we don't currently have any references to functions in lifecycle,
# we have to include it in Imports because we use it in RdMacros

utils::globalVariables(
  c(
    ".",
    "access_type",
    "connectapi_ptypes",
    "guid",
    "last_deployed_time",
    "owner_guid",
    "parse_connectapi_typed",
    "permission",
    "r_version",
    "scoped_experimental_silence",
    "validate_R6_class",
    "warn_experimental"
  )
)

current_connect_version <- "2024.03.0"

.onLoad <- function(...) {
  vctrs::s3_register("dplyr::collect", "tbl_connect")
  vctrs::s3_register("vctrs::vec_cast", "character.integer")
  invisible()
}
