#' Resolve a (sheet, range) pair into a Sheets API `GridRange`
#'
#' Internal helper used by every `gs4mattr` function that needs to target
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
#' @return A list with `sheetId` and any of `startRowIndex`/`endRowIndex`/
#'   `startColumnIndex`/`endColumnIndex` that are actually bounded (unbounded
#'   dimensions are simply omitted, which is how the Sheets API represents
#'   "to the end of the sheet").
#' @keywords internal
#' @noRd
resolve_grid_range <- function(ss, sheet = NULL, range = NULL) {
  sheet_props <- googlesheets4::sheet_properties(ss)

  limits <- NULL
  if (!is.null(range)) {
    limits <- if (inherits(range, "cell_limits")) {
      range
    } else {
      cellranger::as.cell_limits(range)
    }
    if (is.null(sheet) && !is.null(limits$sheet) && !is.na(limits$sheet)) {
      sheet <- limits$sheet
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

  compact(list(
    sheetId = sheet_id,
    startRowIndex = if (!is.na(limits$ul[1])) as.integer(limits$ul[1] - 1) else NULL,
    endRowIndex = if (!is.na(limits$lr[1])) as.integer(limits$lr[1]) else NULL,
    startColumnIndex = if (!is.na(limits$ul[2])) as.integer(limits$ul[2] - 1) else NULL,
    endColumnIndex = if (!is.na(limits$lr[2])) as.integer(limits$lr[2]) else NULL
  ))
}
