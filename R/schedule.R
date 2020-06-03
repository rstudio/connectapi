#' VariantSchedule
#' 
#' An R6 class that represents a Schedule
VariantSchedule <- R6::R6Class(
  "VariantSchedule",
  # TODO: would be cool to have multiple inheritance...
  inherit = Variant,
  public = list(
    schedule_data = NULL,
    initialize = function(connect, content, key, schedule) {
      super$initialize(connect = connect, content = content, key = key)
      # TODO: need to validate schedule (needs an ID)
      self$schedule_data <- schedule
    },
    GET = function(path) {
      self$get_connect()$GET(path)
    },
    POST = function(path, body) {
      self$get_connect()$POST(path = path, body = body)
    },
    DELETE = function(path) {
      self$get_connect()$DELETE(path = path)
    },
    is_empty = function() {
      if (length(self$schedule_data) == 0) {
        TRUE
      } else {
        FALSE
      }
    },
    print = function(...) {
      super$print(...)
      cat("Schedule:\n")
      cat("  get_variant_schedule(variant)\n\n")
      if (self$is_empty()) {
        cat("  WARNING: No schedule defined\n")
      } else {
        cat("  TODO: describe schedule\n")
      }
    },
    get_schedule = function() {
      return(self$schedule_data)
    }
  )
)

get_variant_schedule <- function(variant) {
  warn_experimental("get_schedule")
  scoped_experimental_silence()
  validate_R6_class(variant, "Variant")
  content_details <- variant$get_content()
  connect_client <- variant$get_connect()
  variant_key <- variant$key
  variant_schedule <- variant$get_schedule_remote()
  VariantSchedule$new(connect = connect_client, content = content_details, key = variant_key, schedule = variant_schedule)
}

#' @param .schedule A schedule object. As returned by `get_variant_schedule()`
#' @param type The type of schedule. One of "hour", "minute", "day", "weekday",
#'   "week", "semimonth", "dayofmonth", "dayweekofmonth", "year"
#' @param email Whether to send emails on this schedule
#' @param activate Whether to publish the output of this schedule
#' @param schedule A JSON blob (as a string) describing the schedule. See "More Details"
#' @param start_time The start time of the schedule
#' @param next_run The next run of the schedule (set to `start_time` in most cases)
set_schedule <- function(
  .schedule, 
  ...
  ) {
  validate_R6_class(.schedule, "VariantSchedule")
  cli <- .schedule$get_connect()
  path <- glue::glue("schedules/{.schedule$get_schedule()$id}")
  params <- rlang::list2(...)
  if ("schedule" %in% names(params)) {
    orig_schedule <- params$schedule
    if (is.list(params$schedule)) {
      params$schedule <- jsonlite::toJSON(params$schedule, auto_unbox = TRUE)
    }
    if (!(is.character(params$schedule) && length(params$schedule) == 1 && jsonlite::validate(params$schedule))) {
      stop(glue::glue("The schedule you provided is invalid: {capture.output(str(orig_schedule))}"))
    }
  }
  final_params <- purrr::list_modify(.schedule$get_schedule_remote(), !!!params)
  res <- cli$POST(path = path, body = final_params)
  
  .schedule$get_schedule_remote()
  .schedule
}

set_schedule_remove <- function(.schedule) {
  validate_R6_class(.schedule, "VariantSchedule")
  cli <- .schedule$get_connect()
  path <- glue::glue("schedules/{.schedule$get_schedule()$id}")
  cli$DELETE(path = path)
  get_variant(.schedule, .schedule$key)
}
