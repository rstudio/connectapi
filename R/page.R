#' Paging
#'
#' Helper functions that make paging easier in
#' the Posit Connect Server API.
#'
#' @rdname paging
#'
#' @param client A Connect client object
#' @param req For page_cursor, the output from an initial response to an API
#'   endpoint that uses cursor-based pagination. For page_offset, a request that
#'   needs to be paged.
#' @param limit A row limit
#'
#' @return The aggregated results from all requests
#'
#' @export
page_cursor <- function(client, req, limit = Inf) {
  prg <- optional_progress_bar(
    format = "downloading page :current (:tick_rate/sec) :elapsedfull",
    total = NA,
    clear = FALSE
  )

  prg$tick()
  response <- req

  # Convert the first page (list-of-lists from simplify=FALSE) to a data frame.
  # Subsequent pages use simplify=TRUE so jsonlite builds data frames in C,
  # which is significantly faster for high-volume endpoints.
  first_results <- response$results
  if (length(first_results) > 0 && !is.data.frame(first_results)) {
    first_results <- parse_connectapi(first_results)
  }

  pages <- list(first_results)
  n_items <- if (is.data.frame(first_results)) nrow(first_results) else length(first_results)

  while (!is.null(response$paging$`next`) && n_items < limit) {
    prg$tick()

    next_url <- response$paging$`next`
    response <- client$GET(url = next_url, simplify = TRUE)

    pages[[length(pages) + 1L]] <- response$results
    n_items <- n_items + nrow(response$results)
  }

  if (length(pages) == 1L && !is.data.frame(pages[[1L]])) {
    # Single empty page — return as-is for downstream handling
    return(pages[[1L]])
  }

  out <- vctrs::vec_rbind(!!!pages)
  head(out, n = limit)
}
# TODO: Decide if this `limit = Inf` is helpful or a hack...
#       it is essentially a "row limit" on paging

#' @rdname paging
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
  total_items <- req_response$total

  agg_response <- res
  agg_length <- length(agg_response)
  while (length(res) > 0 && agg_length < limit && agg_length < total_items) {
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
  # Make sure we never return more than `limit` records
  head(agg_response, limit)
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
