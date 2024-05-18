#' VariantSchedule
#'
#' An R6 class that represents a Schedule
#' @family R6 classes
#' @export
VariantSchedule <- R6::R6Class(
  "VariantSchedule",
  # TODO: would be cool to have multiple inheritance...
  inherit = Variant,
  public = list(
    #' @field schedule_data The schedule data.
    schedule_data = NULL,
    #' @description Initialize this schedule.
    #' @param connect The `Connect` instance.
    #' @param content The `Content` instance.
    #' @param key The variant key.
    #' @param schedule The schedule data.
    initialize = function(connect, content, key, schedule) {
      super$initialize(connect = connect, content = content, key = key)
      # TODO: need to validate schedule (needs an ID)
      self$schedule_data <- schedule
    },
    #' @description Perform an HTTP GET request of the named API path. Returns an object parsed from the HTTP response.
    #' @param path API path.
    GET = function(path) {
      self$get_connect()$GET(path)
    },
    #' @description Perform an HTTP POST request of the named API path. Returns an object parsed from the HTTP response.
    #' @param path API path.
    #' @param body The HTTP payload.
    POST = function(path, body) {
      self$get_connect()$POST(path = path, body = body)
    },
    #' @description Perform an HTTP DELETE request of the named API path. Returns the HTTP response object.
    #' @param path API path.
    DELETE = function(path) {
      self$get_connect()$DELETE(path = path)
    },
    #' @description Set the schedule for this variant
    #' @param ... Schedule fields.
    set_schedule = function(...) {
      warn_experimental("set_schedule")
      params <- rlang::list2(...)
      if ("start_time" %in% names(params)) {
        params$start_time <- make_timestamp(params$start_time)
      }
      if ("next_run" %in% names(params)) {
        params$next_run <- make_timestamp(params$next_run)
      }
      if (self$is_empty()) {
        params <- purrr::list_modify(
          params,
          app_id = self$get_variant()$app_id,
          variant_id = self$get_variant()$id
        )
        path <- "schedules"
      } else {
        path <- glue::glue("schedules/{self$get_schedule()$id}")
      }
      cli <- self$get_connect()
      res <- cli$POST(path = path, body = params)

      self$schedule_data <- res
      return(self)
    },
    #' @description Return if this variant has a schedule.
    is_empty = function() {
      if (length(self$schedule_data) == 0) {
        TRUE
      } else {
        FALSE
      }
    },
    #' @description Print this object.
    #' @param ... Unused.
    print = function(...) {
      super$print(...)
      cat("Schedule:\n")
      cat("  get_variant_schedule(variant)\n\n")
      if (self$is_empty()) {
        cat("  WARNING: No schedule defined\n")
      } else {
        cat(c("", paste0(" ", self$describe_schedule(), "\n")))
      }
    },
    #' @description Get the schedule data.
    get_schedule = function() {
      return(self$schedule_data)
    },
    #' @description Get and store the (remote) schedule data.
    get_schedule_remote = function() {
      sch <- super$get_schedule_remote()
      self$schedule_data <- sch
      return(self$schedule_data)
    },
    #' @description Description of the associated schedule.
    describe_schedule = function() {
      # TODO: create a human readable description of schedule
      if (!self$is_empty()) {
        rawdata <- self$get_schedule()
        schdata <- jsonlite::fromJSON(rawdata$schedule)
        # TODO: translate dayofweek "Days" to something more usable
        plural <- ifelse(ifelse(is.null(schdata$N), FALSE, schdata$N > 1), "s", "")
        desc <- switch(rawdata$type,
          "minute" = glue::glue("Every {schdata$N} minute{plural}"),
          "hour" = glue::glue("Every {schdata$N} hour{plural}"),
          "day" = glue::glue("Every {schdata$N} day{plural}"),
          "weekday" = glue::glue("Every weekday"),
          "week" = glue::glue("Every {schdata$N} week{plural}"),
          "dayofweek" = glue::glue("On week days {glue::glue_collapse(schdata$Days, ', ')}"),
          "semimonth" = ifelse(schdata$First == "TRUE", "On the 1st and 15th of each month", "On the 14th and Last day of each month"),
          "dayofmonth" = glue::glue("Every {schdata$N} month{plural} on day {schdata$Day}"),
          "dayweekofmonth" = glue::glue("Every {schdata$N} month{plural} on week {schdata$Week}, day {schdata$Day}"),
          "year" = glue::glue("Every {schdata$N} year{plural}"),
          "Unknown schedule"
        )
        # TODO: is fetching data during a PRINT a bit overkill?
        tz_offset <- .get_offset(self$get_connect(), rawdata$timezone)
        c(
          desc,
          # TODO: a nice way to print out relative times...?
          glue::glue("Starting {swap_timestamp_format(rawdata$start_time)} ({tz_offset})"),
          glue::glue("Next Run {swap_timestamp_format(rawdata$next_run)} ({tz_offset})")
        )
      }
    }
  )
)

.get_offset <- function(connect, timezone) {
  # TODO: some type of cache to reduce churn here?
  tz <- connect$GET(unversioned_url("timezones"))
  res <- purrr::keep(tz, ~ .x[["timezone"]] == timezone)
  if (length(res) != 1) {
    stop(glue::glue("ERROR: timezone '{timezone}' not found"))
  }
  return(res[[1]][["offset"]])
}

#' Get a Variant Schedule
#'
#' \lifecycle{experimental} Gets the schedule associated with a Variant.
#'
#' @param variant A Variant object, as returned by `get_variant()` or `get_variant_default()`
#'
#' @return A VariantSchedule object
#'
#' @rdname get_variant_schedule
#' @family schedule functions
#' @export
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

#' Set a Schedule
#'
#' \lifecycle{experimental} Sets the schedule for a given Variant. Requires a
#' `Schedule` object (as returned by `get_variant_schedule()`)
#'
#' - `set_schedule()` is a raw interface to Posit Connect's `schedule` API
#' - `set_schedule_*()` functions provide handy wrappers around `set_schedule()`
#' - `set_schedule_remove()` removes a schedule / un-schedules a variant
#'
#' Beware, using `set_schedule()` currently uses the Posit Connect `schedule` API
#' directly, and so can be a little clunky. Using the `set_schedule_*()` is generally
#' recommended.
#'
#' @param .schedule A schedule object. As returned by `get_variant_schedule()`
#' @param email Whether to send emails on this schedule
#' @param activate Whether to publish the output of this schedule
#' @param schedule A JSON blob (as a string) describing the schedule. See "More Details"
#' @param start_time The start time of the schedule
#' @param n The "number of" iterations
#' @param day The day of the week (0-6) or day of the month (0-31)
#' @param days The days of the week (0-6)
#' @param week The week of the month (0-5)
#' @param first [logical] Whether to execute on the 1st and 15th (TRUE) or 14th and last (FALSE)
#' @param timezone The timezone to use for setting the schedule. Defaults to `Sys.timezone()`
#' @param ... Scheduling parameters
#'
#' @return An updated Schedule object
#'
#' @rdname set_schedule
#' @family schedule functions
#' @export
set_schedule <- function(
    .schedule,
    ...) {
  warn_experimental("set_schedule")
  scoped_experimental_silence()
  validate_R6_class(.schedule, "VariantSchedule")
  params <- rlang::list2(...)

  # TODO: check whether this schedule actually exists...
  # TODO: fix capitalization if "day" or "days" or "first" or "n" is provided
  # TODO: if type = "weekday", make sure "schedule" is turned into a {} JSON blob properly

  # because "schedule" has to be a JSON blob, which is confusing
  if ("schedule" %in% names(params)) {
    orig_schedule <- params$schedule
    if (is.list(params$schedule)) {
      params$schedule <- jsonlite::toJSON(params$schedule, auto_unbox = TRUE)
    }
    if (!(is.character(params$schedule) && length(params$schedule) == 1 && jsonlite::validate(params$schedule))) {
      stop(glue::glue("The schedule you provided is invalid: {capture.output(str(orig_schedule))}"))
    }
  }

  if ("type" %in% names(params) && !"schedule" %in% names(params)) {
    warning("Specifying 'type' without 'schedule' can cause unexpected results. Different schedule 'type's have different 'schedule' requirements")
  }

  if ("type" %in% names(params) && !params$type %in% schedule_types) {
    stop(glue::glue("Invalid `type` provided. Should be one of `schedule_types`: {params$type}"))
  }

  # update the existing schedule rather than (likely) erroring
  # this could create some weird edge cases... (see warnings / errors above)
  final_params <- purrr::list_modify(.schedule$get_schedule_remote(), !!!params)

  .schedule$set_schedule(!!!final_params)
}

schedule_types <- c("minute", "hour", "day", "weekday", "week", "dayofweek", "semimonth", "dayofmonth", "dayweekofmonth", "year")

#' @rdname set_schedule
#' @export
set_schedule_minute <- function(.schedule, n = 30, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "minute", schedule = list(N = n), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_hour <- function(.schedule, n = 1, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "hour", schedule = list(N = n), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_day <- function(.schedule, n = 1, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "day", schedule = list(N = n), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_weekday <- function(.schedule, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "weekday", schedule = "{}", start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_week <- function(.schedule, n = 1, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "week", schedule = list(N = n), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_dayofweek <- function(.schedule, days, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "dayofweek", schedule = list(Days = days), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_semimonth <- function(.schedule, first = TRUE, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "semimonth", schedule = list(First = first), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_dayofmonth <- function(.schedule, n = 1, day = 1, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "dayofmonth", schedule = list(N = n, Day = day), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_dayweekofmonth <- function(.schedule, n = 1, day = 1, week = 1, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "dayweekofmonth", schedule = list(N = n, Day = day, Week = week), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

#' @rdname set_schedule
#' @export
set_schedule_year <- function(.schedule, n = 1, start_time = Sys.time(), activate = TRUE, email = FALSE, timezone = Sys.timezone()) {
  set_schedule(.schedule, type = "year", schedule = list(N = n), start_time = start_time, activate = activate, email = email, timezone = timezone)
}

example_schedules <- list(
  list(type = "minute", schedule = list(N = 15)),
  list(type = "hour", schedule = list(N = 2)),
  list(type = "day", schedule = list(N = 3)),
  list(type = "weekday", schedule = "{}"),
  list(type = "week", schedule = list(N = 2)),
  list(type = "dayofweek", schedule = list(Days = list(1))),
  list(type = "dayofweek", schedule = list(Days = list(0, 1, 2, 3, 4, 5, 6))),
  list(type = "semimonth", schedule = list(First = TRUE)),
  list(type = "semimonth", schedule = list(First = FALSE)),
  list(type = "dayofmonth", schedule = list(N = 3, Day = 4)),
  list(type = "dayweekofmonth", schedule = list(N = 3, Day = 1, Week = 4)),
  list(type = "year", schedule = list(N = 2))
)

#' @rdname set_schedule
#' @export
set_schedule_remove <- function(.schedule) {
  validate_R6_class(.schedule, "VariantSchedule")
  cli <- .schedule$get_connect()
  path <- unversioned_url("schedules", .schedule$get_schedule()$id)
  cli$DELETE(path = path)
  get_variant(.schedule, .schedule$key)
}

#' @rdname set_schedule
#' @export
schedule_describe <- function(.schedule) {
  cat(.schedule$describe_schedule(), sep = "\n")
  invisible(.schedule)
}


#' Get TimeZones
#'
#' Get the available timezones from the server.
#'
#' @param connect An R6 Connect object
#'
#' @return A TimeZone vector to be used for setting time zones
#'
#' @family schedule functions
#' @export
get_timezones <- function(connect) {
  raw_tz <- connect$GET(unversioned_url("timezones"))
  tz_values <- purrr::map_chr(raw_tz, ~ .x[["timezone"]])
  tz_display <- purrr::map_chr(raw_tz, ~ glue::glue("{.x[['timezone']]} ({.x[['offset']]})"))

  return(as.list(rlang::set_names(tz_values, tz_display)))
}
