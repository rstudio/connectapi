#' Variant
#'
#' An R6 class that represents a Variant
#'
#' @rdname VariantR6
#' @family R6 classes
#' @export
Variant <- R6::R6Class(
  "Variant",
  inherit = Content,
  public = list(
    #' @field key The variant key.
    key = NULL,
    #' @field variant The variant.
    variant = NULL,
    #' @description Get the underlying variant data.
    get_variant = function() {
      self$variant
    },
    #' @description Get and store the (remote) variant data.
    get_variant_remote = function() {
      path <- unversioned_url("variants", self$get_variant()$id)
      self$variant <- self$get_connect()$GET(path)
      self$variant
    },
    #' @description Initialize this variant.
    #' @param connect The `Connect` instance.
    #' @param content The `Content` instance.
    #' @param key The variant key.
    initialize = function(connect, content, key) {
      super$initialize(connect = connect, content = content)
      # TODO: a better way to GET self
      all_variants <- self$variants()
      if (identical(key, "default")) {
        self$variant <- purrr::keep(all_variants, ~ .x[["is_default"]])[[1]]
      } else {
        self$variant <- purrr::keep(all_variants, ~ .x$key == key)[[1]]
      }
      self$key <- self$variant$key
    },
    #' @description Mail previously rendered content.
    #' @param to Targeting.
    send_mail = function(to = c("me", "collaborators", "collaborators_viewers")) {
      warn_experimental("send_mail")
      url <- unversioned_url("variants", self$get_variant()$id, "sender")
      self$get_connect()$POST(
        path = url,
        query = list(
          email = arg_match(to),
          rendering_id = self$get_variant()$rendering_id
        )
      )
    },
    #' @description Get the (remote) schedule data.
    get_schedule = function() {
      self$get_schedule_remote()
    },
    #' @description Get the (remote) schedule data.
    get_schedule_remote = function() {
      warn_experimental("get_schedule_remote")
      url <- unversioned_url("variants", self$get_variant()$id, "schedules")
      res <- self$get_connect()$GET(url)

      if (length(res) == 1) {
        res <- res[[1]]
      }

      if (length(res) > 0) {
        # add the content guid and variant key
        content_guid <- self$get_content()$guid
        variant_key <- self$key
        res <- purrr::list_modify(res, app_guid = content_guid, variant_key = variant_key)
      }

      res
    },
    #' @description Get the subscribers.
    get_subscribers = function() {
      warn_experimental("subscribers")
      path <- unversioned_url("variants", self$get_variant()$id, "subscribers")
      self$get_connect()$GET(path)
    },
    #' @description Remove a named subscriber.
    #' @param guid User GUID.
    remove_subscriber = function(guid) {
      warn_experimental("subscribers")
      path <- unversioned_url("variants", self$get_variant()$id, "subscribers", guid)
      self$get_connect()$DELETE(path)
    },
    #' @description Add named subscribers.
    #' @param guids User GUIDs.
    add_subscribers = function(guids) {
      warn_experimental("subscribers")
      path <- unversioned_url("variants", self$get_variant()$id, "subscribers")
      self$get_connect()$POST(path = path, body = guids)
    },
    #' @description Render this variant.
    render = function() {
      warn_experimental("render")
      path <- unversioned_url("variants", self$get_variant()$id, "render")
      res <- self$get_connect()$POST(path)

      # add the content guid and variant key
      content_guid <- self$get_content()$guid
      variant_key <- self$key

      purrr::list_modify(res, app_guid = content_guid, variant_key = variant_key)
    },
    #' @description List the renderings of this variant.
    renderings = function() {
      warn_experimental("renderings")
      url <- unversioned_url("variants", self$get_variant()$id, "renderings")
      res <- self$get_connect()$GET(path = url)
      # add the content guid and variant key
      content_guid <- self$get_content()$guid
      variant_key <- self$key

      purrr::map(
        res,
        ~ purrr::list_modify(.x, app_guid = content_guid, variant_key = variant_key)
      )
    },
    #' @description Update this variant.
    #' @param ... Target fields and values.
    update_variant = function(...) {
      params <- rlang::list2(...)
      # TODO: allow updating a variant
      url <- unversioned_url("variants", self$get_variant()$id)
      res <- self$get_connect()$POST(url, body = params)
      return(self)
    },
    #' @description Jobs for this variant.
    jobs = function() {
      pre_jobs <- super$jobs()
      purrr::map(
        pre_jobs,
        ~ purrr::list_modify(.x, variant_key = self$key)
      )
    },
    #' @description Return single job for this variant.
    #' @param key The job key.
    job = function(key) {
      pre_job <- super$job(key = key)
      purrr::map(
        list(pre_job),
        ~ purrr::list_modify(.x, variant_key = self$key)
      )[[1]]
    },
    #' @description Return the URL for this variant.
    get_url = function() {
      base_content <- super$get_url()
      glue::glue("{base_content}v{self$key}/")
    },
    #' @description Return the URL associated with one rendering for this variant.
    #' @param rev Rendering identifier.
    get_url_rev = function(rev) {
      base_url <- self$get_url()
      glue::glue("{base_url}_rev{rev}")
    },
    #' @description Return the URL for this variant in the Posit Connect dashboard.
    #' @param pane The pane in the dashboard to link to.
    get_dashboard_url = function(pane = "access") {
      base_content <- super$get_dashboard_url("")
      glue::glue("{base_content}{pane}/{self$get_variant()$id}")
    },
    # nolint start: commented_code_linter
    # TODO: dashboard cannot navigate directly to renderings today
    # get_dashboard_url_rev = function(rev, pane = "") {
    #  base_content <- self$get_dashboard_url("")
    #  glue::glue("{base_content}_rev{rev}")
    # },
    # nolint end
    #' @description Print this object.
    #' @param ... Unused.
    print = function(...) {
      super$print(...)
      cat("Variant:\n")
      cat(glue::glue("  get_variant(content, key = '{self$key}' )"), "\n")
      cat("\n")
    }
  )
)

#' VariantTask
#'
#' An R6 class that represents a Variant Task
#'
#' @family R6 classes
VariantTask <- R6::R6Class(
  "VariantTask",
  inherit = Variant,
  public = list(
    #' @field task The task.
    task = NULL,
    #' @field data The variant data.
    data = NULL,
    #' @description Initialize this variant task.
    #' @param connect The `Connect` instance.
    #' @param content The `Content` instance.
    #' @param key The variant key.
    #' @param task The task data.
    initialize = function(connect, content, key, task) {
      super$initialize(connect = connect, content = content, key = key)
      # TODO: need to validate task (needs task_id)
      self$task <- task
    },
    #' @description Return the underlying task.
    get_task = function() {
      self$task
    },
    #' @description Set the data.
    #' @param data The data.
    add_data = function(data) {
      self$data <- data
      invisible(self)
    },
    #' @description Get the data.
    get_data = function() {
      self$data
    },
    #' @description Print this object.
    #' @param ... Unused.
    print = function(...) {
      super$print(...)
      cat("Task: \n")
      cat("  Task ID: ", self$get_task()$task_id, "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

#' Get Variant
#'
#' `r lifecycle::badge('experimental')` Work with variants
#'
#' - `get_variants()` returns a `tibble` with variant data for a `content_item`
#' - `get_variant_default()` returns the default variant for a `content_item`
#' - `get_variant()` returns a specific variant for a `content_item` (specified by `key`)
#'
#' @param content An R6 Content object. Returned from `content_item()`
#' @param key The Variant key for a specific variant
#'
#' @rdname variant
#'
#' @family variant functions
#' @export
get_variants <- function(content) {
  warn_experimental("get_variants")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")

  variants <- content$variants()

  parse_connectapi_typed(variants, connectapi_ptypes$variant)
}

#' @rdname variant
#' @family variant functions
#' @export
get_variant <- function(content, key) {
  warn_experimental("get_variant")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  Variant$new(connect = content$get_connect(), content = content$get_content(), key = key)
}

#' @rdname variant
#' @family variant functions
#' @export
get_variant_default <- function(content) {
  get_variant(content, "default")
}

#' Render a Variant
#'
#' `r lifecycle::badge('experimental')` Get details about renderings (i.e. render history)
#' or execute a variant on demand
#'
#' - `get_variant_renderings()` returns all renderings / content for a particular variant. Returns a `tibble`
#' - `variant_render()` executes a variant on demand. Returns a `VariantTask` object
#'
#' @param variant An R6 Variant object. As returned by `get_variant()` or `get_variant_default()`
#'
#' @rdname variant_render
#' @family variant functions
#' @export
get_variant_renderings <- function(variant) {
  warn_experimental("get_variant_renderings")
  scoped_experimental_silence()
  validate_R6_class(variant, "Variant")

  renders <- variant$renderings()
  parse_connectapi_typed(renders, connectapi_ptypes$rendering)
}

#' @rdname variant_render
#' @export
variant_render <- function(variant) {
  warn_experimental("variant_render")
  scoped_experimental_silence()
  validate_R6_class(variant, "Variant")

  rendered <- variant$render()
  rendered$task_id <- rendered$id

  VariantTask$new(connect = variant$get_connect(), content = variant$get_content(), key = variant$key, task = rendered)
}

# TODO
# set_variant_email_viewers <- function() {
#
# }
#
# set_variant_email_collaborators <- function() {
#
# }
#
# set_variant_email_subscribe <- function() {
#
# }
#
# set_variant_email_unsubscribe <- function() {
#
# }
