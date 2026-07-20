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

  grid_properties <- drop_nulls(list(
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

#' Whether two `GridRange`s (assumed to be on the same sheet) overlap
#'
#' `GridRange` bounds are optional -- a missing `start*Index` means "from
#' the very first row/column" and a missing `end*Index` means "to the very
#' last" -- so this treats a missing bound as unbounded before comparing,
#' rather than assuming it means zero-width.
#'
#' @param a,b `GridRange` lists.
#' @keywords internal
#' @noRd
grid_ranges_overlap <- function(a, b) {
  bounded <- function(x, default) if (is.null(x)) default else x
  a_row <- c(bounded(a$startRowIndex, -Inf), bounded(a$endRowIndex, Inf))
  b_row <- c(bounded(b$startRowIndex, -Inf), bounded(b$endRowIndex, Inf))
  a_col <- c(bounded(a$startColumnIndex, -Inf), bounded(a$endColumnIndex, Inf))
  b_col <- c(bounded(b$startColumnIndex, -Inf), bounded(b$endColumnIndex, Inf))
  (a_row[1] < b_row[2]) && (b_row[1] < a_row[2]) &&
    (a_col[1] < b_col[2]) && (b_col[1] < a_col[2])
}

#' Find the id(s) of every existing banded range (alternating colors --
#' from the Sheets UI's Format > Alternating colors, or an
#' `AddBandingRequest` like the one `gs_theme_*()` sends) that overlaps a
#' `GridRange`
#'
#' Banded ranges are a sheet-level object, stored separately from
#' `userEnteredFormat` -- clearing cell formatting never touches them. To
#' remove one, `DeleteBandingRequest` needs its `bandedRangeId`, and
#' there's no way to know that id without asking the API for it, since it
#' isn't derived from anything the caller already has on hand (unlike a
#' `GridRange`, which is fully computable from a sheet + A1 range).
#'
#' @param ss A `sheets_id`.
#' @param grid_range The `GridRange` to check for overlap against.
#' @return Integer vector of `bandedRangeId`s (possibly empty).
#' @keywords internal
#' @noRd
find_banded_range_ids <- function(ss, grid_range) {
  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.get",
    params = list(
      spreadsheetId = ss,
      fields = "sheets(properties.sheetId,bandedRanges(bandedRangeId,range))"
    )
  )
  resp <- googlesheets4::request_make(req)
  body <- gargle::response_process(resp)

  sheets <- body$sheets
  if (is.null(sheets) || length(sheets) == 0) {
    return(integer(0))
  }
  sheet_ids <- vapply(sheets, function(s) s$properties$sheetId %||% NA_real_, numeric(1))
  idx <- match(grid_range$sheetId, sheet_ids)
  if (is.na(idx)) {
    return(integer(0))
  }
  banded <- sheets[[idx]]$bandedRanges
  if (is.null(banded) || length(banded) == 0) {
    return(integer(0))
  }
  overlapping <- Filter(function(b) grid_ranges_overlap(b$range, grid_range), banded)
  vapply(overlapping, function(b) b$bandedRangeId, numeric(1))
}

#' Find the index/indices of every existing conditional-format rule
#' (`range_add_conditional_format()`/`range_add_gradient_format()`, or one
#' added by hand in the Sheets UI) that overlaps a `GridRange`
#'
#' Conditional-format rules are a sheet-level object, like banded ranges --
#' stored separately from `userEnteredFormat`, so clearing cell formatting
#' never touches them either. `DeleteConditionalFormatRuleRequest` needs
#' the rule's `index` (its position within that sheet's own
#' `conditionalFormats` list), not an id, and that position isn't derived
#' from anything the caller already has on hand, so this asks the API for
#' it the same way `find_banded_range_ids()` does.
#'
#' A rule can have more than one `GridRange` in its own `ranges` list (e.g.
#' a single rule applied to two non-adjacent column selections); this
#' counts the rule as overlapping if *any* of its ranges do.
#'
#' @param ss A `sheets_id`.
#' @param grid_range The `GridRange` to check for overlap against.
#' @return Integer vector of 0-based rule indices, in *descending* order --
#'   deleting by index shifts every later rule's index down by one, so the
#'   caller needs to delete highest-index-first for the remaining indices
#'   in this vector to stay valid. Possibly empty.
#' @keywords internal
#' @noRd
find_conditional_format_indices <- function(ss, grid_range) {
  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.get",
    params = list(
      spreadsheetId = ss,
      fields = "sheets(properties.sheetId,conditionalFormats(ranges))"
    )
  )
  resp <- googlesheets4::request_make(req)
  body <- gargle::response_process(resp)

  sheets <- body$sheets
  if (is.null(sheets) || length(sheets) == 0) {
    return(integer(0))
  }
  sheet_ids <- vapply(sheets, function(s) s$properties$sheetId %||% NA_real_, numeric(1))
  idx <- match(grid_range$sheetId, sheet_ids)
  if (is.na(idx)) {
    return(integer(0))
  }
  rules <- sheets[[idx]]$conditionalFormats
  if (is.null(rules) || length(rules) == 0) {
    return(integer(0))
  }
  overlaps <- vapply(rules, function(rule) {
    any(vapply(rule$ranges %||% list(), grid_ranges_overlap, logical(1), b = grid_range))
  }, logical(1))
  sort(which(overlaps) - 1L, decreasing = TRUE)
}

#' Clear all cell formatting for a sheet, or a range within it
#'
#' Resets fonts, colors, number formats, alignment, and text wrap back to
#' the spreadsheet default. This is the same reset [range_format()]'s
#' `clear_first = TRUE` performs before applying new formatting, exposed on
#' its own for when you just want a clean slate without immediately
#' formatting anything. This also removes any existing banded range
#' (alternating colors -- from the Sheets UI or a `gs_theme_*()` call) and
#' any conditional-format rule (from [range_add_conditional_format()],
#' [range_add_gradient_format()], or the Sheets UI) that overlaps `range`,
#' since both are separate sheet-level objects `userEnteredFormat` doesn't
#' cover and clearing it wouldn't otherwise touch. Note this still doesn't
#' affect borders — a separate property again; see [range_format_border()]
#' (`style = "NONE"`) to clear those.
#'
#' @inheritParams range_format
#' @param range `NULL` (the default) clears the whole sheet. Pass an A1
#'   string or `cellranger` helper to clear only that range instead.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes a `RepeatCellRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#repeatcellrequest>
#' @export
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("prettysheets-demo", sheets = mtcars)
#' sheet_clear_format(ss)
#' sheet_clear_format(ss, range = "A1:K1")
#' }
sheet_clear_format <- function(ss, sheet = NULL, range = NULL) {
  ss <- googlesheets4::as_sheets_id(ss)
  grid_range <- resolve_grid_range(ss, sheet, range)
  send_or_queue(ss, clear_format_request(grid_range))

  banded_ids <- find_banded_range_ids(ss, grid_range)
  for (id in banded_ids) {
    send_or_queue(ss, list(deleteBanding = list(bandedRangeId = id)))
  }

  # Descending order (see find_conditional_format_indices()): deleting the
  # highest index first keeps every remaining index in this vector valid.
  cf_indices <- find_conditional_format_indices(ss, grid_range)
  for (i in cf_indices) {
    send_or_queue(ss, list(
      deleteConditionalFormatRule = list(sheetId = grid_range$sheetId, index = as.integer(i))
    ))
  }

  invisible(ss)
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

#' Set (or clear) a (work)sheet's tab color
#'
#' @inheritParams range_format
#' @param color Hex string, named R color, or [gs4_palette] name. `NULL`
#'   clears the tab back to its default (no color) -- the `fields` mask
#'   still names `tabColor`, but with no value supplied for it in
#'   `properties`, which is the Sheets API's own convention for resetting
#'   a masked field to its default rather than leaving it untouched.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes an `UpdateSheetPropertiesRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#updatesheetpropertiesrequest>
#' @export
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("prettysheets-demo", sheets = mtcars)
#' sheet_format_tabcolor(ss, color = "forestgreen")
#' sheet_format_tabcolor(ss, color = NULL) # clears it back to no color
#' }
sheet_format_tabcolor <- function(ss, sheet = NULL, color = NULL) {
  ss <- googlesheets4::as_sheets_id(ss)
  grid_range <- resolve_grid_range(ss, sheet, NULL)

  request <- list(
    updateSheetProperties = list(
      properties = drop_nulls(list(
        sheetId = grid_range$sheetId,
        tabColor = gs4_color(color)
      )),
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
