#' Freeze rows and/or columns of a (work)sheet
#'
#' Follows the exact argument style of [googlesheets4::sheet_resize()],
#' since both are implemented as an `UpdateSheetPropertiesRequest` under the
#' hood — here targeting `gridProperties.frozenRowCount`/`frozenColumnCount`.
#'
#' @inheritParams range_format
#' @param n_rows,n_cols Number of rows/columns to freeze from the top-left.
#'   `NULL` (the default) leaves that dimension unchanged.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes an `UpdateSheetPropertiesRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#updatesheetpropertiesrequest>
#' @export
sheet_freeze <- function(ss, sheet = NULL, n_rows = NULL, n_cols = NULL) {
  ss <- googlesheets4::as_sheets_id(ss)
  grid_range <- resolve_grid_range(ss, sheet, NULL)

  grid_properties <- compact(list(
    frozenRowCount = if (!is.null(n_rows)) as.integer(n_rows) else NULL,
    frozenColumnCount = if (!is.null(n_cols)) as.integer(n_cols) else NULL
  ))
  if (length(grid_properties) == 0) {
    cli::cli_abort("{.fn sheet_freeze} needs at least one of {.arg n_rows} or {.arg n_cols}.")
  }

  fields <- paste0(
    "gridProperties(", paste(names(grid_properties), collapse = ","), ")"
  )

  request <- list(
    updateSheetProperties = list(
      properties = list(
        sheetId = grid_range$sheetId,
        gridProperties = grid_properties
      ),
      fields = fields
    )
  )
  send_or_queue(ss, request)
}

#' Style the header row of a range you wrote with `range_write()`
#'
#' `googlesheets4::sheet_write()` already applies header styling and freezes
#' row 1 automatically. Use this when you wrote data with
#' [googlesheets4::range_write()] instead (which does *not* auto-style), and
#' want the same "table" look applied manually.
#'
#' @inheritParams range_format
#' @param row Row to style as a header. Defaults to `1`.
#' @param bold,background_color,font_color Passed straight through to
#'   [range_format()]; defaults give a bold header with a light grey
#'   background, matching the look `sheet_write()` applies automatically.
#' @param freeze Logical; also freeze this row (and everything above it) via
#'   [sheet_freeze()]. Default `TRUE`.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @export
sheet_format_header <- function(ss,
                                 sheet = NULL,
                                 row = 1,
                                 bold = TRUE,
                                 background_color = "#F3F3F3",
                                 font_color = NULL,
                                 freeze = TRUE) {
  ss <- googlesheets4::as_sheets_id(ss)
  range_format(
    ss, sheet = sheet, range = cellranger::cell_rows(row),
    bold = bold, background_color = background_color, font_color = font_color
  )
  if (freeze) {
    sheet_freeze(ss, sheet = sheet, n_rows = row)
  }
  invisible(ss)
}

#' Set a (work)sheet's tab color
#'
#' @inheritParams range_format
#' @param color Hex string or named R color.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes an `UpdateSheetPropertiesRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#updatesheetpropertiesrequest>
#' @export
sheet_format_tabcolor <- function(ss, sheet = NULL, color) {
  ss <- googlesheets4::as_sheets_id(ss)
  grid_range <- resolve_grid_range(ss, sheet, NULL)

  request <- list(
    updateSheetProperties = list(
      properties = list(
        sheetId = grid_range$sheetId,
        tabColor = gs4_color(color)
      ),
      fields = "tabColor"
    )
  )
  send_or_queue(ss, request)
}

#' Merge or unmerge a range of cells
#'
#' @inheritParams range_format
#' @param type How to merge: `"all"` (one big merged cell), `"columns"`
#'   (merge each column of the range separately), or `"rows"` (merge each
#'   row separately). Only used by `range_merge()`.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes a `MergeCellsRequest`/`UnmergeCellsRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#mergecellsrequest>
#' @export
range_merge <- function(ss, sheet = NULL, range, type = c("all", "columns", "rows")) {
  type <- match.arg(type)
  ss <- googlesheets4::as_sheets_id(ss)
  grid_range <- resolve_grid_range(ss, sheet, range)

  merge_type <- switch(type,
    all = "MERGE_ALL",
    columns = "MERGE_COLUMNS",
    rows = "MERGE_ROWS"
  )
  request <- list(mergeCells = list(range = grid_range, mergeType = merge_type))
  send_or_queue(ss, request)
}

#' @rdname range_merge
#' @export
range_unmerge <- function(ss, sheet = NULL, range) {
  ss <- googlesheets4::as_sheets_id(ss)
  grid_range <- resolve_grid_range(ss, sheet, range)

  request <- list(unmergeCells = list(range = grid_range))
  send_or_queue(ss, request)
}
