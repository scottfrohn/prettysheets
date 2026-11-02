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
  # request_make() returns the raw httr response as-is; it doesn't check the
  # status code. Piping it through response_process() raises a real error on
  # a rejected request instead of failing silently.
  resp <- googlesheets4::request_make(req)
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
#' Inside a batch, the spreadsheet's sheet list (needed to turn a sheet name
#' into the numeric id the API wants) is also fetched only once and reused,
#' instead of once per call. A few functions still need to read the sheet
#' live -- [sheet_clear_format()] (and so the `gs_theme_*()` functions)
#' looks up existing banding and conditional formats, and [gs4_cols()]
#' without `data` reads the header row -- so a batch isn't always exactly
#' one API call, but all the *writes* go out together.
#'
#' The `gs_theme_*()` functions and [write_pretty_sheet()] already batch
#' themselves. Batches can be nested: an inner `prettysheets_batch()` just
#' adds its requests to the outer one, which sends everything at the end.
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
  run_batch(code, allow_empty = FALSE)
}

#' Engine behind [prettysheets_batch()], also used internally by functions
#' that make several requests (e.g. the `gs_theme_*()` functions) so they
#' batch themselves
#'
#' If a batch is already being collected, `code` just adds to it and the
#' outermost batch sends everything.
#'
#' @param code Code containing `prettysheets` formatting calls.
#' @param allow_empty Logical; if `FALSE`, error when `code` queued nothing.
#' @keywords internal
#' @noRd
run_batch <- function(code, allow_empty = TRUE) {
  if (isTRUE(.prettysheets_env$collecting)) {
    force(code)
    return(invisible(.prettysheets_env$ss))
  }

  .prettysheets_env$collecting <- TRUE
  .prettysheets_env$queue <- list()
  .prettysheets_env$ss <- NULL
  .prettysheets_env$sheet_props <- list()
  on.exit({
    .prettysheets_env$collecting <- FALSE
    .prettysheets_env$queue <- list()
    .prettysheets_env$sheet_props <- list()
  })

  force(code)

  if (length(.prettysheets_env$queue) == 0 || is.null(.prettysheets_env$ss)) {
    if (allow_empty) {
      return(invisible(.prettysheets_env$ss))
    }
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
.prettysheets_env$sheet_props <- list()

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

#' [googlesheets4::sheet_properties()], fetched once per spreadsheet per
#' batch
#'
#' Outside a batch this is a plain `sheet_properties()` call. Inside one,
#' the result is cached until the batch ends, so a block of calls against
#' the same spreadsheet doesn't re-read its sheet list every time. Call
#' `forget_sheet_properties()` after anything that adds, removes, or renames
#' a sheet.
#'
#' @param ss A `sheets_id`.
#' @keywords internal
#' @noRd
cached_sheet_properties <- function(ss) {
  if (!isTRUE(.prettysheets_env$collecting)) {
    return(googlesheets4::sheet_properties(ss))
  }
  key <- as.character(ss)
  if (is.null(.prettysheets_env$sheet_props[[key]])) {
    .prettysheets_env$sheet_props[[key]] <- googlesheets4::sheet_properties(ss)
  }
  .prettysheets_env$sheet_props[[key]]
}

#' @rdname cached_sheet_properties
#' @keywords internal
#' @noRd
forget_sheet_properties <- function(ss) {
  .prettysheets_env$sheet_props[[as.character(ss)]] <- NULL
  invisible(ss)
}
