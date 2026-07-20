#' Reference a range by column name instead of column letter
#'
#' Pass the result as `range` to [range_format()], [range_format_border()],
#' or any other `prettysheets` function that takes a `range`, to target one or
#' more columns by their header name (e.g. `"mpg"`) instead of a column
#' letter or A1 string. Only the data rows below the header are selected —
#' the header row itself is left untouched, so styling a column by name
#' never re-styles its header.
#'
#' If the named columns aren't contiguous in the sheet (e.g. `"mpg"` and
#' `"wt"` with another column between them), this expands into one
#' `GridRange` per contiguous run, and the calling function sends one
#' request per run — so formatting still reaches every named column, just
#' as more than one API request under the hood.
#'
#' @param ... One or more column names (character strings).
#' @param data Optional; a data frame (its `names()` are used) or a
#'   character vector of column names, matched against `...` with no API
#'   call — handy right after [range_write_format()] with the same `data`.
#'   If omitted, column names are instead read live from the sheet's header
#'   row when the range is resolved.
#' @param header_row Row number of the header. Determines which row is
#'   read (when `data` is not supplied) and which row is excluded from the
#'   selected range. Default `1`.
#' @return A `prettysheets_cols` object.
#' @export
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("prettysheets-demo", sheets = mtcars)
#' range_format(ss, range = gs4_cols("mpg", "hp"), bold = TRUE)
#' range_format(ss, range = gs4_cols("mpg", data = mtcars), number_format = "0.0")
#' }
gs4_cols <- function(..., data = NULL, header_row = 1) {
  names <- c(...)
  if (length(names) == 0 || !is.character(names)) {
    cli::cli_abort("{.fn gs4_cols} needs at least one column name (a string).")
  }
  structure(
    list(names = names, data = data, header_row = as.integer(header_row)),
    class = "prettysheets_cols"
  )
}

#' Get the header names a `prettysheets_cols` object should be matched against
#'
#' Uses `cols$data` if supplied (no API call); otherwise reads the sheet's
#' header row live.
#'
#' @param cols A `prettysheets_cols` object (see [gs4_cols()]).
#' @keywords internal
#' @noRd
resolve_cols_header <- function(ss, sheet, cols) {
  if (!is.null(cols$data)) {
    return(if (is.data.frame(cols$data)) names(cols$data) else as.character(cols$data))
  }
  # col_names = FALSE means googlesheets4 has no real header to assign, so it
  # falls back to placeholder names ("...1", "...2", ...) via tibble's name
  # repair -- which by default *announces* the repair with a "New names:"
  # message. We immediately discard those placeholder names below (we only
  # want the row's actual values), so the message is just noise here.
  header_row_data <- suppressMessages(googlesheets4::range_read(
    ss,
    sheet = sheet,
    range = cellranger::cell_rows(cols$header_row),
    col_names = FALSE,
    col_types = "c"
  ))
  as.character(unlist(header_row_data[1, ], use.names = FALSE))
}

#' Match a `prettysheets_cols` object's names against a header, and group the
#' resulting column positions into contiguous runs
#'
#' Pure logic, kept separate from `resolve_cols_header()` so it's testable
#' without a live sheet: given the header, this never talks to the API.
#'
#' @param cols A `prettysheets_cols` object (see [gs4_cols()]).
#' @param header Character vector of column names, e.g. from
#'   `resolve_cols_header()`.
#' @param include_header Logical; include `cols$header_row` itself in the
#'   returned range, instead of starting one row below it. Default `FALSE`.
#' @return A list of `cellranger::cell_limits()` objects, one per
#'   contiguous run of matched columns, covering the data rows below
#'   `cols$header_row` in each run (or, with `include_header = TRUE`,
#'   `cols$header_row` and everything below it).
#' @keywords internal
#' @noRd
cols_to_cell_limits <- function(cols, header, include_header = FALSE) {
  positions <- match(cols$names, header)
  missing <- cols$names[is.na(positions)]
  if (length(missing) > 0) {
    cli::cli_abort("Column name{?s} not found in the header row: {.val {missing}}.")
  }

  positions <- sort(unique(positions))
  run_id <- cumsum(c(1, diff(positions) != 1))
  # unname() -- split() names each group by its run_id ("1", "2", ...), and
  # that name would otherwise survive all the way out of
  # resolve_grid_ranges(). Harmless for callers (like range_format()) that
  # use each element on its own, but callers that bundle every element into
  # one field (like range_add_conditional_format()'s `ranges = grid_ranges`)
  # need a plain unnamed list -- a *named* list serializes to a JSON object
  # instead of a JSON array, which silently breaks the request.
  runs <- unname(split(positions, run_id))

  start_row <- if (isTRUE(include_header)) cols$header_row else cols$header_row + 1L

  lapply(runs, function(run) {
    cellranger::cell_limits(
      ul = c(start_row, min(run)),
      lr = c(NA, max(run))
    )
  })
}

#' Resolve a (sheet, range) pair into one or more Sheets API `GridRange`s
#'
#' Like `resolve_grid_range()`, but also understands [gs4_cols()] objects,
#' which can expand into more than one `GridRange` (one per contiguous run
#' of named columns). Every `prettysheets` function that formats a range should
#' call this instead of `resolve_grid_range()` directly, then send one
#' request per range in the returned list.
#'
#' @inheritParams resolve_grid_range
#' @param include_header Logical; whether a whole-column selection (a
#'   [gs4_cols()] object, or a plain whole-column `range` like `"B:B"`)
#'   includes its header row. If not supplied at all, this defaults to
#'   `FALSE` (exclude) for a `gs4_cols()` `range` -- that's `gs4_cols()`'s
#'   own contract, regardless of which function resolves it -- and to
#'   `TRUE` (include, i.e. no change) for every other kind of `range`, so
#'   callers that don't expose this argument (like `range_format_border()`)
#'   keep their original behavior. Callers that *do* expose the argument
#'   (like [range_format()]) should always pass it explicitly, which
#'   overrides both of those defaults with one consistent value.
#' @return A list of `GridRange` lists (always at least one).
#' @keywords internal
#' @noRd
resolve_grid_ranges <- function(ss, sheet = NULL, range = NULL, include_header) {
  if (missing(include_header)) {
    include_header <- !inherits(range, "prettysheets_cols")
  }

  if (!inherits(range, "prettysheets_cols")) {
    return(list(resolve_grid_range(ss, sheet, range, include_header = include_header)))
  }

  header <- resolve_cols_header(ss, sheet, range)
  limits_list <- cols_to_cell_limits(range, header, include_header = include_header)
  lapply(limits_list, function(limits) resolve_grid_range(ss, sheet, limits))
}
