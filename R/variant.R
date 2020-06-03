#' Variant
#' 
#' An R6 class that represents a Variant
#' 
Variant <- R6::R6Class(
  "Variant",
  inherit = Content,
  public = list(
    key = NULL,
    variant = NULL,
    get_variant = function() {self$variant},
    get_variant_remote = function() {
      variant <- self$get_connect()$GET("variants/{self$get_variant()$id}")
      self$variant
    },
    initialize = function(connect, content, key) {
      super$initialize(connect = connect, content = content)
      self$key <- key
      # TODO: a better way to GET self
      all_variants <- self$variants()
      this_variant <- purrr::keep(all_variants, ~ .x$key == key)[[1]]
      self$variant <- this_variant
    },
    send_mail = function(to = c("me", "collaborators", "collaborators_viewers")) {
      warn_experimental("send_mail")
      if (length(to) > 1) to <- "me"
      url <- glue::glue("variants/{self$get_variant()$id}/sender")
      self$get_connect()$POST(
        path = url, 
        body = list(
          email = to
        ))
    },
    get_schedule = function() {
      self$get_schedule_remote()
    },
    get_schedule_remote = function() {
      warn_experimental("get_schedule_remote")
      url <- glue::glue("variants/{self$get_variant()$id}/schedules")
      res <- self$get_connect()$GET(
        path = url
      )
      
      # add the content guid and variant key
      content_guid <- self$get_content()$guid
      variant_key <- self$key
      
      purrr::list_modify(res, app_guid = content_guid, variant_key = variant_key)
      return(res)
    },
    get_subscribers = function() {
      self$get_connect()$GET("variants/{self$get_variant()$id}/subscribers")
    },
    remove_subscriber = function(guid) {
      self$get_connect()$DELETE("variants/{self$get_variant()$id}/subscribers/{guid}")
    },
    add_subscribers = function(guids) {
      url <- glue::glue("variants/{self$get_variant()$id}/subscribers")
      self$get_connect()$POST(
        path = url,
        body = guids
      )
    },
    render = function() {
      warn_experimental("render")
      # TODO: why both in query AND in body?
      url <- glue::glue("variants/{self$get_variant()$id}/render?email=none&activate=true")
      res <- self$get_connect()$POST(
        path = url,
        body = list(
          email = "none",
          activate = TRUE
        )
      )
      
      # add the content guid and variant key
      content_guid <- self$get_content()$guid
      variant_key <- self$key
      
      purrr::list_modify(res, app_guid = content_guid, variant_key = variant_key)
    },
    renderings = function() {
      warn_experimental("renderings")
      url <- glue::glue("variants/{self$get_variant()$id}/renderings")
      res <- self$get_connect()$GET(
        path = url
      )
      # add the content guid and variant key
      content_guid <- self$get_content()$guid
      variant_key <- self$key
      
      purrr::map(
        res,
        ~ purrr::list_modify(.x, app_guid = content_guid, variant_key = variant_key)
      )
    },
    update_variant = function(...) {
      params <- rlang::list2(...)
      # TODO: allow updating a variant
      url <- glue::glue("variants/{self$get_variant()$id}")
      res <- self$get_connect()$POST(
        url,
        params
      )
      return(self)
    },
    jobs = function() {
      pre_jobs <- super$jobs()
      purrr::map(
        pre_jobs,
        ~ purrr::list_modify(.x, variant_key = self$key)
      )
    },
    job = function(key) {
      pre_job <- super$job(key = key)
      purrr::map(
        list(pre_job),
        ~ purrr::list_modify(.x, variant_key = self$key)
      )[[1]]
    },
    get_url = function() {
      base_content <- super$get_url()
      glue::glue("{base_content}v{self$key}/")
    },
    get_url_rev = function(rev) {
      base_url <- self$get_url()
      glue::glue("{base_url}_rev{rev}")
    },
    get_dashboard_url = function(pane = "access") {
      base_content <- super$get_dashboard_url("")
      glue::glue("{base_content}{pane}/{self$get_variant()$id}")
    },
    # TODO: dashboard cannot navigate directly to renderings today
    #get_dashboard_url_rev = function(rev, pane = "") {
    #  base_content <- self$get_dashboard_url("")
    #  glue::glue("{base_content}_rev{rev}")
    #},
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
    task = NULL,
    initialize = function(connect, content, key, task) {
      super$initialize(connect = connect, content = content, key = key)
      # TODO: need to validate task (needs task_id)
      self$task <- task
    },
    get_task = function() {
      self$task
    },
    
    print = function(...) {
      super$print(...)
      cat("Task: \n")
      cat("  Task ID: ", self$get_task()$task_id, "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

get_variants <- function(content) {
  warn_experimental("get_variants")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  
  variants <- content$variants()
  
  parse_connectapi_typed(variants, !!!connectapi_ptypes$variant)
}

get_variant_default <- function(content) {
  warn_experimental("get_variant_default")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  all_variants <- content$variants()
  the_default <- purrr::keep(all_variants, ~ .x[["is_default"]])[[1]]
  variant <- Variant$new(connect =  content$get_connect(), content = content$get_content(), key = the_default$key)
  return(variant)
}

get_variant <- function(content, key) {
  warn_experimental("get_variant")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  variant <- Variant$new(connect = content$get_connect(), content = content$get_content(), key = key)
  return(variant)
}

get_variant_renderings <- function(variant) {
  warn_experimental("get_variant_renderings")
  scoped_experimental_silence()
  validate_R6_class(variant, "Variant")
  
  renders <- variant$renderings()
  parse_connectapi_typed(renders, !!!connectapi_ptypes$rendering)
}

get_jobs <- function(content) {
  warn_experimental("get_jobs")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  
  jobs <- content$jobs()
  parse_connectapi_typed(jobs, !!!connectapi_ptypes$jobs)
}

# TODO: Need to test `logged_error` on a real error
get_job <- function(content, key) {
  warn_experimental("get_job")
  scoped_experimental_silence()
  validate_R6_class(content, "Content")
  
  job <- content$job(key = key)
  # protect against becoming a list...
  job$stdout <- strsplit(job$stdout, "\n")[[1]]
  job$stderr <- strsplit(job$stderr, "\n")[[1]]
  # a bit of an abuse
  # since stdout / stderr / logged_error are here now...
  parse_connectapi_typed(list(job), !!!connectapi_ptypes$job)
}

variant_render <- function(variant) {
  warn_experimental("variant_render")
  scoped_experimental_silence()
  validate_R6_class(variant, "Variant")
  
  rendered <- variant$render()
  rendered$task_id <- rendered$id
  
  VariantTask$new(connect = variant$get_connect(), content = variant$get_content(), key = variant$key, task = rendered)
}
