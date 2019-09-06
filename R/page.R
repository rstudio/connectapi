#' @export
page_cursor <- function(client, req, limit = Inf) {
  qreq <- rlang::enquo(req)
  
  prg <- progress::progress_bar$new(
    format = "downloading page :current (:tick_rate/sec) :elapsedfull",
    total = NA,
    clear = FALSE
  )
  
  prg$tick()
  response <- rlang::eval_tidy(qreq)
  
  res <- response$results
  while(!is.null(response$paging$`next`) && length(res) < limit) {
    prg$tick()
    response <- client$GET_URL(response$paging$`next`)
    res <- c(res, response$results)
  }
  return(res)
}
