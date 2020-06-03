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

set_schedule <- function(
  schedule, 
  type = c(),
  iter = c(),
  start_time = Sys.time(),
  publish = TRUE,
  email = FALSE
  ) {
  # TODO: set a schedule
}

set_schedule_remove <- function(schedule) {
  # TODO: delete a schedule
}
