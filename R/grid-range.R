#' Resolve a (sheet, range) pair into a Sheets API `GridRange`
#'
#' Internal helper used by every `prettysheets` function that needs to target
#' cells. Looks up the numeric `sheetId` (the Sheets API always wants an
#' integer id, never a sheet name) via [googlesheets4::sheet_properties()],
#' and converts the range into zero-based, half-open row/column bounds.
#'
#' @param ss A `sheets_id`, already run through [googlesheets4::as_sheets_id()]
#'   by the caller.
#' @param sheet Sheet name (string) or position (number). `NULL` means "first
#'   visible sheet", unless `range` is a `cell_limits` object that already
#'   specifies a sheet.
#' @param range `NULL` (whole sheet), an A1-style string (e.g. `"B2:D10"`,
#'   `"C:C"`, `"2:5"`), or a `cellranger` helper like
#'   [cellranger::cell_rows()], [cellranger::cell_cols()], or
#'   [cellranger::cell_limits()].
#' @param include_header Logical; only relevant when `range` selects one or
#'   more WHOLE columns with no row bounds at all (e.g. `"B:B"`, `"B:D"`, or
#'   a bare [cellranger::cell_cols()]) -- there's no way to know where a
#'   header row would be for any other kind of `range`. `TRUE` (the
#'   default) keeps the literal A1 meaning of a whole-column reference
#'   (row 1 included). `FALSE` assumes row 1 is a header and starts at row
#'   2 instead.
#' @return A list with `sheetId` and any of `startRowIndex`/`endRowIndex`/
#'   `startColumnIndex`/`endColumnIndex` that are actually bounded (unbounded
#'   dimensions are simply omitted, which is how the Sheets API represents
#'   "to the end of the sheet").
#' @keywords internal
#' @noRd
resolve_grid_range <- function(ss, sheet = NULL, range = NULL, include_header = TRUE) {
  sheet_props <- googlesheets4::sheet_properties(ss)

  limits <- NULL
  if (!is.null(range)) {
    limits <- parse_range_shorthand(range)
    if (is.null(sheet) && !is.null(limits$sheet) && !is.na(limits$sheet)) {
      sheet <- limits$sheet
    }
    if (!isTRUE(include_header) && is.na(limits$ul[1]) && is.na(limits$lr[1])) {
      # A fully open-ended column selection (e.g. "B:B", "B:D", or a bare
      # cellranger::cell_cols()) has no row bounds at all -- assume row 1
      # is a header and start at row 2 instead.
      limits$ul[1] <- 2
    }
  }

  if (is.null(sheet)) {
    visible <- sheet_props[sheet_props$visible, , drop = FALSE]
    if (nrow(visible) == 0) {
      cli::cli_abort("This spreadsheet has no visible sheets to target.")
    }
    sheet_row <- visible[1, ]
  } else if (is.numeric(sheet)) {
    sheet_row <- sheet_props[sheet_props$index == (sheet - 1), , drop = FALSE]
    if (nrow(sheet_row) == 0) {
      cli::cli_abort("No sheet at position {sheet}.")
    }
  } else {
    sheet_row <- sheet_props[sheet_props$name == sheet, , drop = FALSE]
    if (nrow(sheet_row) == 0) {
      cli::cli_abort("No sheet named {.val {sheet}} in this spreadsheet.")
    }
  }
  sheet_id <- sheet_row$id[[1]]

  if (is.null(limits)) {
    return(list(sheetId = sheet_id))
  }

  drop_nulls(list(
    sheetId = sheet_id,
    startRowIndex = if (!is.na(limits$ul[1])) as.integer(limits$ul[1] - 1) else NULL,
    endRowIndex = if (!is.na(limits$lr[1])) as.integer(limits$lr[1]) else NULL,
    startColumnIndex = if (!is.na(limits$ul[2])) as.integer(limits$ul[2] - 1) else NULL,
    endColumnIndex = if (!is.na(limits$lr[2])) as.integer(limits$lr[2]) else NULL
  ))
}

#' Parse a `range` argument, including Google Sheets' whole-column /
#' whole-row A1 shorthand
#'
#' `cellranger::as.cell_limits()` correctly parses single-cell and
#' cell-to-cell A1 references (`"A1"`, `"A1:D10"`, `"Sheet1!A1:B2"`), but
#' errors on Google Sheets/Excel's own whole-column (`"B:B"`, `"B:D"`) and
#' whole-row (`"2:5"`) shorthand -- e.g. `as.cell_limits("B:B")` fails with
#' "Can't guess format of this cell reference", because that shorthand
#' isn't standard cell-to-cell A1 notation. This recognizes those two forms
#' first and routes them through [cellranger::cell_cols()]/
#' [cellranger::cell_rows()] instead (which do understand them), falling
#' back to `as.cell_limits()` for everything else. Sheet-qualified shorthand
#' (`"Sheet1!B:B"`) isn't supported -- use plain `"B:B"` plus the `sheet`
#' argument instead.
#'
#' @param range A `cell_limits` object (returned as-is), or anything else
#'   accepted by `cellranger::as.cell_limits()`, plus the two shorthand
#'   forms described above.
#' @keywords internal
#' @noRd
parse_range_shorthand <- function(range) {
  if (inherits(range, "cell_limits")) {
    return(range)
  }
  if (is.character(range) && length(range) == 1) {
    cols <- regmatches(range, regexec("^([A-Za-z]+):([A-Za-z]+)$", range))[[1]]
    if (length(cols) == 3) {
      return(cellranger::cell_cols(cols[2:3]))
    }
    rows <- regmatches(range, regexec("^([0-9]+):([0-9]+)$", range))[[1]]
    if (length(rows) == 3) {
      return(cellranger::cell_rows(as.integer(rows[2:3])))
    }
  }
  cellranger::as.cell_limits(range)
}
