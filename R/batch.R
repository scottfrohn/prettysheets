#' Send one or more Sheets API requests in a single `batchUpdate` call
#'
#' Internal engine used by every `gs4mattr` function. Built entirely on
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
  googlesheets4::request_make(req)

  invisible(ss)
}

#' Batch multiple `gs4mattr` formatting calls into a single API request
#'
#' Every `gs4mattr` function normally sends its own `batchUpdate` call as
#' soon as it runs, which is simple but costs one network round trip per
#' call. Wrap a block of `gs4mattr` calls in `gs4mattr_batch()` to collect
#' their requests and send them all together in one call instead — useful
#' when you're applying several formatting operations to the same
#' spreadsheet in a row and want to reduce API traffic (the Sheets API's
#' documented per-user rate limit is 100 requests per 100 seconds).
#'
#' @param code Code containing one or more `gs4mattr` formatting calls.
#' @return The `sheets_id` of the spreadsheet that was updated, invisibly.
#' @export
#' @examples
#' \dontrun{
#' gs4mattr_batch({
#'   range_format(ss, range = "A1:D1", bold = TRUE)
#'   range_format(ss, range = "A1:D1", background_color = "#F4CCCC")
#' })
#' }
gs4mattr_batch <- function(code) {
  .gs4mattr_env$collecting <- TRUE
  .gs4mattr_env$queue <- list()
  .gs4mattr_env$ss <- NULL
  on.exit({
    .gs4mattr_env$collecting <- FALSE
    .gs4mattr_env$queue <- list()
  })

  force(code)

  if (length(.gs4mattr_env$queue) == 0 || is.null(.gs4mattr_env$ss)) {
    cli::cli_abort("No {.pkg gs4mattr} requests were queued inside {.fn gs4mattr_batch}.")
  }

  apply_requests(.gs4mattr_env$ss, .gs4mattr_env$queue)
}

#' @keywords internal
#' @noRd
.gs4mattr_env <- new.env(parent = emptyenv())
.gs4mattr_env$collecting <- FALSE
.gs4mattr_env$queue <- list()
.gs4mattr_env$ss <- NULL

#' Queue or immediately send a request, depending on whether we're inside
#' [gs4mattr_batch()]
#' @keywords internal
#' @noRd
send_or_queue <- function(ss, request) {
  if (isTRUE(.gs4mattr_env$collecting)) {
    ss <- googlesheets4::as_sheets_id(ss)
    .gs4mattr_env$ss <- ss
    .gs4mattr_env$queue[[length(.gs4mattr_env$queue) + 1]] <- request
    return(invisible(ss))
  }
  apply_requests(ss, request)
}
