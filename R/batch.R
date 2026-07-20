#' Send one or more Sheets API requests in a single `batchUpdate` call
#'
#' Internal engine used by every `prettysheets` function. Built entirely on
#' `googlesheets4`'s exported low-level API
#' ([googlesheets4::request_generate()] and [googlesheets4::request_make()]),
#' the same functions the package itself documents as intended "for internal
#' use and for programming around the Sheets API" — no reliance on
#' unexported internals for this layer.
#'
#' @param ss A `sheets_id`.
#' @param requests A single request object (a named list with one element,
#'   e.g. `list(repeatCell = ...)`), or a list of such request objects, to be
#'   sent together as one `batchUpdate` call.
#' @return `ss`, invisibly, so calls chain with `|>`.
#' @keywords internal
#' @noRd
apply_requests <- function(ss, requests) {
  ss <- googlesheets4::as_sheets_id(ss)

  is_single_request <- !is.null(names(requests)) && length(names(requests)) == length(requests)
  requests <- if (is_single_request) list(requests) else requests

  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = ss,
      requests = requests
    )
  )
  resp <- googlesheets4::request_make(req)
  # request_make() returns the raw httr response as-is -- it does NOT check
  # the status code or raise on a 4xx/5xx. Without this, a rejected request
  # (e.g. a malformed body) fails completely silently: no R error, and
  # whatever this call was supposed to do just doesn't happen. Piping it
  # through response_process() makes a bad request surface as a real error
  # instead of vanishing.
  gargle::response_process(resp)

  invisible(ss)
}

#' Batch multiple `prettysheets` formatting calls into a single API request
#'
#' Every `prettysheets` function normally sends its own `batchUpdate` call as
#' soon as it runs, which is simple but costs one network round trip per
#' call. Wrap a block of `prettysheets` calls in `prettysheets_batch()` to
#' collect their requests and send them all together in one call instead —
#' useful when you're applying several formatting operations to the same
#' spreadsheet in a row and want to reduce API traffic (the Sheets API's
#' documented per-user rate limit is 100 requests per 100 seconds).
#'
#' @param code Code containing one or more `prettysheets` formatting calls.
#' @return The `sheets_id` of the spreadsheet that was updated, invisibly.
#' @export
#' @examples
#' \dontrun{
#' prettysheets_batch({
#'   range_format(ss, range = "A1:D1", bold = TRUE)
#'   range_format(ss, range = "A1:D1", background_color = "#F4CCCC")
#' })
#' }
prettysheets_batch <- function(code) {
  .prettysheets_env$collecting <- TRUE
  .prettysheets_env$queue <- list()
  .prettysheets_env$ss <- NULL
  on.exit({
    .prettysheets_env$collecting <- FALSE
    .prettysheets_env$queue <- list()
  })

  force(code)

  if (length(.prettysheets_env$queue) == 0 || is.null(.prettysheets_env$ss)) {
    cli::cli_abort("No {.pkg prettysheets} requests were queued inside {.fn prettysheets_batch}.")
  }

  apply_requests(.prettysheets_env$ss, .prettysheets_env$queue)
}

#' @keywords internal
#' @noRd
.prettysheets_env <- new.env(parent = emptyenv())
.prettysheets_env$collecting <- FALSE
.prettysheets_env$queue <- list()
.prettysheets_env$ss <- NULL

#' Queue or immediately send a request, depending on whether we're inside
#' [prettysheets_batch()]
#' @keywords internal
#' @noRd
send_or_queue <- function(ss, request) {
  if (isTRUE(.prettysheets_env$collecting)) {
    ss <- googlesheets4::as_sheets_id(ss)
    .prettysheets_env$ss <- ss
    .prettysheets_env$queue[[length(.prettysheets_env$queue) + 1]] <- request
    return(invisible(ss))
  }
  apply_requests(ss, request)
}
