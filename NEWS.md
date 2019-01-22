# connectApiUtils 0.1.0.9000

BREAKING: 
* Rename a handful of functions:
  - `connect$activate_bundle` to `connect$content_deploy`
  - `connect$create_app` to `connect$content_create`
  - `connect$upload_bundle` to `connect$content_upload`
* Switch endpoints from using `app_id` to `guid`
* `get_task$start` renamed to `get_task$first`
  
OTHER: 
* Add some endpoints:
  - `get_content`
  - `get_audit_logs`
  - `get_server_settings`
  - `get_server_settings_r`
  - `inst_shiny_usage`
  - `inst_content_visits`
* Update `connect` R6 object to be compatible with Connect 1.7.0 APIs
* Added a `NEWS.md` file to track changes to the package.
