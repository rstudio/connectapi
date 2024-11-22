NA_datetime_ <- # nolint: object_name_linter
  vctrs::new_datetime(NA_real_, tzone = "UTC")
NA_list_ <- # nolint: object_name_linter
  list(list())

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
  groups = tibble::tibble(
    "guid" = NA_character_,
    "name" = NA_character_,
    "owner_guid" = NA_character_
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
  ),
  content = tibble::tibble(
    "guid" = NA_character_,
    "name" = NA_character_,
    "title" = NA_character_,
    "description" = NA_character_,
    "access_type" = NA_character_,
    "connection_timeout" = NA_integer_,
    "read_timeout" = NA_integer_,
    "init_timeout" = NA_integer_,
    "idle_timeout" = NA_integer_,
    "max_processes" = NA_integer_,
    "min_processes" = NA_integer_,
    "max_conns_per_process" = NA_integer_,
    "load_factor" = NA_real_,
    "created_time" = NA_datetime_,
    "last_deployed_time" = NA_datetime_,
    "bundle_id" = NA_character_,
    "app_mode" = NA_character_,
    "content_category" = NA_character_,
    "parameterized" = FALSE,
    "cluster_name" = NA_character_,
    "image_name" = NA_character_,
    "r_version" = NA_character_,
    "py_version" = NA_character_,
    "quarto_version" = NA_character_,
    "run_as" = NA_character_,
    "run_as_current_user" = FALSE,
    "owner_guid" = NA_character_,
    "content_url" = NA_character_,
    "dashboard_url" = NA_character_,
    "app_role" = NA_character_,
    "id" = NA_character_,
    "owner" = NA_list_,
  ),
  content_old = tibble::tibble(
    "id" = NA_integer_,
    "guid" = NA_character_,
    "access_type" = NA_character_,
    "connection_timeout" = NA_real_,
    "read_timeout" = NA_real_,
    "init_timeout" = NA_real_,
    "idle_timeout" = NA_real_,
    "max_processes" = NA_integer_,
    "min_processes" = NA_integer_,
    "max_conns_per_process" = NA_integer_,
    "load_factor" = NA_real_,
    "url" = NA_character_,
    "vanity_url" = NA,
    "name" = NA_character_,
    "title" = NA_character_,
    "bundle_id" = NA_integer_,
    # (1=shiny, 2=shiny Rmd, 3=source Rmd, 4=static, 5=api, 6=tensorflow, 7=python, 8=flask, 9=dash, 10=streamlit)
    "app_mode" = NA_integer_,
    "content_category" = NA_character_,
    "has_parameters" = NA,
    "created_time" = NA_datetime_,
    "last_deployed_time" = NA_datetime_,
    "r_version" = NA_character_,
    "py_version" = NA_character_,
    "build_status" = NA_integer_,
    "run_as" = NA_character_,
    "run_as_current_user" = NA,
    "description" = NA_character_,
    "app_role" = NA_character_,
    "owner_first_name" = NA_character_,
    "owner_last_name" = NA_character_,
    "owner_username" = NA_character_,
    "owner_guid" = NA_character_,
    "owner_email" = NA_character_,
    "owner_locked" = NA,
    "is_scheduled" = NA,
    "git" = NA_list_
  ),
  audit_logs = tibble::tibble(
    "id" = NA_character_,
    "time" = NA_datetime_,
    "user_id" = NA_character_,
    "user_guid" = NA_character_,
    "user_description" = NA_character_,
    "action" = NA_character_,
    "event_description" = NA_character_
  ),
  procs = tibble::tibble(
    pid = NA_character_,
    appId = NA_integer_,
    appGuid = NA_character_,
    appName = NA_character_,
    appUrl = NA_character_,
    appRunAs = NA_character_,
    type = NA_character_,
    cpuCurrent = NA_real_,
    cpuTotal = NA_integer_,
    ram = fs::as_fs_bytes(NA_integer_)
  ),
  variant = tibble::tibble(
    id = NA_integer_,
    app_id = NA_integer_,
    key = NA_character_,
    bundle_id = NA_integer_,
    is_default = NA,
    name = NA_character_,
    email_collaborators = NA,
    email_viewers = NA,
    created_time = NA_datetime_,
    rendering_id = NA_integer_,
    render_time = NA_datetime_,
    render_duration = bit64::NA_integer64_,
    visibility = NA_character_,
    owner_id = NA_integer_
  ),
  rendering = tibble::tibble(
    id = NA_integer_,
    app_id = NA_integer_,
    variant_id = NA_integer_,
    bundle_id = NA_integer_,
    job_key = NA_character_,
    render_time = NA_datetime_,
    render_duration = bit64::as.integer64(NA_integer_),
    active = NA,
    app_guid = NA_character_,
    variant_key = NA_character_,
  ),
  jobs = tibble::tibble(
    id = NA_integer_,
    pid = NA_integer_,
    key = NA_character_,
    app_id = NA_integer_,
    app_guid = NA_character_,
    variant_id = NA_integer_,
    bundle_id = NA_integer_,
    start_time = NA_datetime_,
    end_time = NA_datetime_,
    tag = NA_character_,
    exit_code = NA_integer_,
    finalized = NA,
    hostname = NA_character_,
    variant_key = NA_character_
  ),
  job = tibble::tibble(
    pid = NA_integer_,
    key = NA_character_,
    app_id = NA_integer_,
    variant_id = NA_integer_,
    bundle_id = NA_integer_,
    tag = NA_character_,
    finalized = NA,
    hostname = NA_character_,
    origin = NA_character_,
    stdout = NA_list_,
    stderr = NA_list_,
    logged_error = NA_list_,
    start_time = NA_datetime_,
    end_time = NA_datetime_,
    exit_code = NA_integer_,
    app_guid = NA_character_,
    variant_key = NA_character_
  ),
  bundles = tibble::tibble(
    id = NA_character_,
    content_guid = NA_character_,
    created_time = NA_datetime_,
    r_version = NA_character_,
    py_version = NA_character_,
    active = NA,
    size = fs::as_fs_bytes(NA_integer_),
    metadata = NA_list_,
  ),
  permissions = tibble::tibble(
    id = NA_character_,
    content_guid = NA_character_,
    principal_guid = NA_character_,
    principal_type = NA_character_,
    role = NA_character_
  ),
  group_content = tibble::tibble(
    content_guid = NA_character_,
    content_name = NA_character_,
    content_title = NA_character_,
    access_type = NA_character_,
    permissions = NA_list_
  )
)

# Validates an input data frame against a required schema ptype.
# 1. is a data frame or similar object;
# 2. contains all the names from the required;
# 3. that all matching names have the correct ptype.
validate_df_ptype <- function(input, required) {
  if (!inherits(input, "data.frame")) {
    stop("Input must be a data frame.")
  }
  if (!all(names(input) %in% required)) {
    missing <- setdiff(names(required), names(input))
    if (length(missing) > 0) {
      stop(glue::glue("Missing required columns: {paste0(missing, collapse = ', ')}"))
    }
  }

  for (col in names(required)) {
    tryCatch(
      vctrs::vec_ptype_common(input[[col]], required[[col]]),
      error = function(e) {
        stop(glue::glue(
          "Column `{col}` has type `{vctrs::vec_ptype_abbr(input[[col]])}`; ",
          "needs `{vctrs::vec_ptype_abbr(required[[col]])}:`\n",
          conditionMessage(e)
        ))
      }
    )
  }
}
