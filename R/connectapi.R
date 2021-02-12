#' @importFrom utils compareVersion
#' @importFrom utils untar
"_PACKAGE"

utils::globalVariables(
  c(
    "r_version",
    ".",
    "parse_connectapi_typed",
    "connectapi_ptypes",
    "validate_R6_class",
    "warn_experimental"
  )
)

current_connect_version <- "1.8.6"
