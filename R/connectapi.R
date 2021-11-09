#' @importFrom utils compareVersion
#' @importFrom utils untar
"_PACKAGE"

utils::globalVariables(
  c(
    ".",
    "access_type",
    "connectapi_ptypes",
    "guid",
    "owner_guid",
    "parse_connectapi_typed",
    "permission",
    "r_version",
    "scoped_experimental_silence",
    "validate_R6_class",
    "warn_experimental",
  )
)

current_connect_version <- '2021.10.0'
