#' Paging
#'
#' Helper functions that make paging easier in
#' the Posit Connect Server API.
#'
#' @rdname paging
#'
#' @param client A Connect client object
#' @param req The request that needs to be paged
#' @param limit A row limit
#'
#' @return The aggregated results from all requests
#'
#' @export
page_cursor <- function(client, req, limit = Inf) {
  qreq <- rlang::enquo(req)

  prg <- optional_progress_bar(
    format = "downloading page :current (:tick_rate/sec) :elapsedfull",
    total = NA,
    clear = FALSE
  )

  prg$tick()
  response <- rlang::eval_tidy(qreq)

  res <- response$results
  while (!is.null(response$paging$`next`) && length(res) < limit) {
    prg$tick()

    next_url <- response$paging$`next`
    response <- client$GET(url = next_url)

    res <- c(res, response$results)
  }
  res <- head(res, n = limit)
  return(res)
}
# TODO: Decide if this `limit = Inf` is helpful or a hack...
#       it is essentially a "row limit" on paging


#' Paging
#'
#' Helper functions that make paging easier in
#' the Posit Connect Server API.
#'
#' @rdname paging
#'
#' @param client A Connect client object
#' @param req The request that needs to be paged
#' @param limit A row limit
#'
#' @return The aggregated results from all requests
#'
#' @export
page_offset <- function(client, req, limit = Inf) {
  qreq <- rlang::enquo(req)
  qexpr <- rlang::quo_get_expr(qreq)

  prg <- optional_progress_bar(
    format = "downloading page :current (:tick_rate/sec) :elapsedfull",
    total = NA,
    clear = FALSE
  )

  # handle paging
  prg$tick()

  req_response <- rlang::eval_tidy(qreq)
  res <- req_response$results

  agg_response <- res
  agg_length <- length(agg_response)
  while (length(res) > 0 && agg_length < limit) {
    prg$tick()

    # bump the page number
    current_page <- req_response$current_page
    new_expr <- qexpr
    new_expr$page_number <- current_page + 1
    new_req <- rlang::quo_set_expr(qreq, new_expr)

    # next request
    req_response <- rlang::eval_tidy(new_req)
    res <- req_response$results

    agg_response <- c(agg_response, res)
    agg_length <- length(agg_response)

    # clear out variables
    current_page <- NULL
    new_expr <- NULL
    new_req <- NULL
  }
  return(agg_response)
}

optional_progress_bar <- function(...) {
  if (requireNamespace("progress", quietly = TRUE)) {
    progress::progress_bar$new(...)
  } else {
    # Return a mock object that behaves enough like a progress bar object
    list(
      tick = function() {}
    )
  }
}
