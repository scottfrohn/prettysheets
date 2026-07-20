#' Convert a Sheets API `Color` object back to a hex string
#'
#' The reverse of [gs4_color()]: `gs4_color()` turns a hex/named/palette
#' color into `{red, green, blue}` floats for the API; this turns the
#' `{red, green, blue}` floats the API sends *back* (e.g. in a cell's
#' `userEnteredFormat.backgroundColor`, or a banded range's colors) into a
#' hex string, for building a [gs_theme()] from what's actually on a
#' sheet. A missing component is treated as `0`, matching the API's own
#' convention that an absent `Color` field means zero intensity.
#'
#' @param color A list with `red`/`green`/`blue` (each `0`-`1`), or `NULL`.
#' @return A hex string (e.g. `"#F4CCCC"`), or `NULL` if `color` is `NULL`.
#' @keywords internal
#' @noRd
color_to_hex <- function(color) {
  if (is.null(color)) {
    return(NULL)
  }
  channel <- function(x) round((x %||% 0) * 255)
  grDevices::rgb(channel(color$red), channel(color$green), channel(color$blue), maxColorValue = 255)
}

#' Pull the `range_format()`-style arguments out of a raw `CellFormat`
#'
#' Only includes a field if the raw format actually has it set -- absent
#' fields mean "not explicitly set" (the Sheets default), so they're left
#' out rather than guessed at, which is what keeps a captured column
#' override from being polluted with values that were never really a
#' deliberate style choice.
#'
#' @param fmt A `CellFormat` list (a cell's `userEnteredFormat`, as
#'   returned by [googlesheets4::range_read_cells()] with `cell_data =
#'   "full"`), or `NULL`.
#' @return A named list of [range_format()]-style arguments (possibly
#'   empty).
#' @keywords internal
#' @noRd
extract_cell_format_args <- function(fmt) {
  if (is.null(fmt)) {
    return(list())
  }
  tf <- fmt$textFormat
  nf <- fmt$numberFormat
  out <- drop_nulls(list(
    bold = tf$bold,
    italic = tf$italic,
    underline = tf$underline,
    strikethrough = tf$strikethrough,
    font_size = tf$fontSize,
    font_family = tf$fontFamily,
    font_color = color_to_hex(tf$foregroundColor),
    background_color = color_to_hex(fmt$backgroundColor),
    number_format = nf$pattern,
    number_format_type = if (!is.null(nf$pattern)) nf$type else NULL,
    horizontal_alignment = fmt$horizontalAlignment,
    vertical_alignment = fmt$verticalAlignment,
    wrap_strategy = fmt$wrapStrategy
  ))
  # drop_nulls() on an all-NULL input leaves a length-0 list that still
  # carries a (zero-length) `names` attribute -- not identical() /
  # expect_equal()-equal to a bare list(), even though both are "empty" in
  # every way that matters to this function's callers. Normalize so an
  # empty-format cell always comes back as the same plain list() the
  # `is.null(fmt)` branch above already returns.
  if (length(out) == 0) list() else out
}

#' Fetch the structural (sheet-level) properties `read_pretty_sheet()`
#' needs: frozen row count, banded ranges, and conditional-format rules
#'
#' None of this is exposed by any `googlesheets4` function, so it goes
#' straight to the `spreadsheets.get` endpoint with a narrow `fields` mask
#' -- the same pattern `find_banded_range_ids()` (sheet-structure.R) uses.
#'
#' @param ss A `sheets_id`.
#' @param sheet_id Numeric sheet id (from `resolve_grid_range()`).
#' @return A list with `properties`, `bandedRanges`, `conditionalFormats`
#'   -- whatever the API returned for that one sheet, or empty defaults if
#'   the sheet somehow isn't found in the response.
#' @keywords internal
#' @noRd
fetch_sheet_meta <- function(ss, sheet_id) {
  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.get",
    params = list(
      spreadsheetId = ss,
      fields = "sheets(properties(sheetId,gridProperties),bandedRanges,conditionalFormats)"
    )
  )
  resp <- googlesheets4::request_make(req)
  body <- gargle::response_process(resp)

  sheets <- body$sheets %||% list()
  ids <- vapply(sheets, function(s) as.numeric(s$properties$sheetId %||% NA_real_), numeric(1))
  idx <- match(as.numeric(sheet_id), ids)
  if (is.na(idx)) {
    return(list(properties = list(), bandedRanges = list(), conditionalFormats = list()))
  }
  sheets[[idx]]
}

#' Which header names a `GridRange`'s column bounds span
#'
#' @param range_piece A `GridRange` list (0-based, half-open columns).
#' @param headers Character vector of column names, in sheet order.
#' @return A character vector (possibly empty, if the range's columns
#'   don't overlap `headers` at all).
#' @keywords internal
#' @noRd
matched_column_names <- function(range_piece, headers) {
  n_col <- length(headers)
  start_col <- max((range_piece$startColumnIndex %||% 0) + 1L, 1L)
  end_col <- min(range_piece$endColumnIndex %||% n_col, n_col)
  if (start_col > end_col) {
    return(character(0))
  }
  headers[start_col:end_col]
}

#' Translate a sheet's raw `bandedRanges` into a `gs_theme()` `banding`
#' list, if one overlaps the table
#'
#' @param banded_ranges `body$bandedRanges` from `fetch_sheet_meta()`.
#' @param table_grid_range The table's `GridRange`.
#' @return A list with `band1`/`band2` (and the raw `headerColor`, for the
#'   header-background fallback), or `NULL` if nothing overlaps.
#' @keywords internal
#' @noRd
translate_banding <- function(banded_ranges, table_grid_range) {
  overlapping <- Filter(function(b) grid_ranges_overlap(b$range, table_grid_range), banded_ranges %||% list())
  if (length(overlapping) == 0) {
    return(NULL)
  }
  row_props <- overlapping[[1]]$rowProperties
  list(
    band1 = color_to_hex(row_props$firstBandColor),
    band2 = color_to_hex(row_props$secondBandColor),
    header_color = color_to_hex(row_props$headerColor)
  )
}

#' Translate a sheet's raw `conditionalFormats` into `gs_theme()`
#' `conditional_formats` entries, for rules that overlap the table
#'
#' A `cf_cell_value()`/`cf_format()`/`cf_gradient()` call already builds
#' its `condition`/`format`/`gradient` argument in exactly the shape the
#' Sheets API sends back (see conditional-format.R), so no translation is
#' needed there -- only the `range` needs work, since a rule's raw
#' `GridRange` is tied to this sheet's absolute row/column numbers. It's
#' converted to a [gs4_cols()] selection (matched by header name) instead,
#' so the rule keeps applying to the right columns regardless of how many
#' rows of data a later `write_pretty_sheet()` call writes.
#'
#' @param conditional_formats `body$conditionalFormats` from
#'   `fetch_sheet_meta()`.
#' @param table_grid_range The table's `GridRange`.
#' @param headers Character vector of column names, in sheet order.
#' @return A list of `list(range = gs4_cols(...), rule = , format = )` or
#'   `list(range = gs4_cols(...), gradient = )` entries (possibly empty).
#' @keywords internal
#' @noRd
translate_conditional_formats <- function(conditional_formats, table_grid_range, headers) {
  specs <- list()
  for (rule in (conditional_formats %||% list())) {
    for (range_piece in (rule$ranges %||% list())) {
      if (!grid_ranges_overlap(range_piece, table_grid_range)) {
        next
      }
      names <- matched_column_names(range_piece, headers)
      if (length(names) == 0) {
        next
      }
      spec <- if (!is.null(rule$booleanRule)) {
        list(range = gs4_cols(names), rule = rule$booleanRule$condition, format = rule$booleanRule$format)
      } else if (!is.null(rule$gradientRule)) {
        list(range = gs4_cols(names), gradient = rule$gradientRule)
      } else {
        next
      }
      specs[[length(specs) + 1]] <- spec
    }
  }
  specs
}

#' Reconstruct a `gs_theme()` from what's actually applied to a sheet
#'
#' Samples the header row and first data row for per-column formatting
#' (see `read_pretty_sheet()`'s docs for why only two rows), and fetches
#' banding/conditional-format/freeze state structurally in one
#' `spreadsheets.get` call.
#'
#' @param ss A `sheets_id`.
#' @param sheet Sheet name or position (as passed to `read_pretty_sheet()`).
#' @param data The data already read from `sheet` (used for its column
#'   names and row count; no extra API call).
#' @return A `prettysheets_theme` object.
#' @keywords internal
#' @noRd
build_theme_from_sheet <- function(ss, sheet, data) {
  headers <- names(data)
  n_col <- length(headers)
  n_row <- nrow(data)

  table_grid_range <- resolve_grid_range(
    ss, sheet, cellranger::cell_limits(ul = c(1, 1), lr = c(n_row + 1L, n_col))
  )

  sample_range <- cellranger::cell_limits(ul = c(1, 1), lr = c(2, n_col))
  cells <- googlesheets4::range_read_cells(
    ss, sheet = sheet, range = sample_range, cell_data = "full", discard_empty = FALSE
  )

  cell_at <- function(row, col) {
    match <- cells[cells$row == row & cells$col == col, ]
    if (nrow(match) == 0) NULL else match$cell[[1]]$userEnteredFormat
  }

  header_args <- extract_cell_format_args(cell_at(1, 1))
  header <- header_args[intersect(names(header_args), c("bold", "background_color", "font_color"))]

  columns <- lapply(seq_len(n_col), function(i) extract_cell_format_args(cell_at(2, i)))
  names(columns) <- headers
  columns <- columns[vapply(columns, length, integer(1)) > 0]

  wrap_long_text <- any(vapply(columns, function(x) identical(x$wrap_strategy, "WRAP"), logical(1)))

  meta <- fetch_sheet_meta(ss, table_grid_range$sheetId)

  banding_raw <- translate_banding(meta$bandedRanges, table_grid_range)
  if (!is.null(banding_raw)) {
    if (is.null(header$background_color)) {
      header$background_color <- banding_raw$header_color
    }
    banding <- list(band1 = banding_raw$band1, band2 = banding_raw$band2)
  } else {
    banding <- NULL
  }

  conditional_formats <- translate_conditional_formats(meta$conditionalFormats, table_grid_range, headers)

  frozen_rows <- meta$properties$gridProperties$frozenRowCount %||% 0
  freeze_header <- frozen_rows >= 1

  gs_theme(
    header = header,
    banding = banding,
    columns = columns,
    wrap_long_text = wrap_long_text,
    freeze_header = freeze_header,
    conditional_formats = conditional_formats
  )
}

#' Read a sheet's data along with the formatting theme applied to it
#'
#' The theme-aware counterpart to `googlesheets4::range_read()`
#' (`read_sheet()`): reads the sheet's data exactly like `range_read()`
#' does, and additionally reconstructs a [gs_theme()] describing how the
#' table is formatted -- header style, banding (zebra-striping) colors,
#' per-column formatting, and any conditional-format rules that cover the
#' table. The theme is attached to the returned tibble as `attr(x,
#' "prettysheets_theme")`, so `write_pretty_sheet(ss2, data =
#' read_pretty_sheet(ss1))` reuses the original sheet's look on a
#' different sheet (or spreadsheet) with no extra arguments.
#'
#' Only row 1 (the header) and row 2 (the first data row) are inspected
#' for formatting -- not every cell -- on the assumption that a themed
#' table's body formatting is uniform down each column, which is true of
#' every theme this package produces and of anything hand-styled the same
#' way. This keeps `read_pretty_sheet()` cheap regardless of how many rows
#' the sheet has. The one thing this assumption *can* miss is genuinely
#' one-off manual formatting applied below row 2 (e.g. hand-highlighting a
#' single cell); conditional formats aren't affected by this limitation at
#' all, since they're read from the sheet's actual `conditionalFormats`
#' rules, not sampled from cells.
#'
#' Borders and merged cells aren't reconstructed (the API doesn't expose
#' "does this table have an outside border" as a queryable property --
#' only the per-edge-cell formatting that *produces* one -- and merges
#' don't affect a per-column theme). Add them back with [modify_theme()]
#' if you need them: `modify_theme(theme, border = list(color = "gray40"))`.
#'
#' @inheritParams googlesheets4::range_read
#' @param ... Passed on to [googlesheets4::range_read()] (e.g. `col_types`,
#'   `na`, `skip`, `n_max`).
#' @return A tibble, like [googlesheets4::range_read()], with a
#'   `prettysheets_theme` attribute (see [gs_theme()]).
#' @export
#' @examples
#' \dontrun{
#' ss1 <- googlesheets4::gs4_create("styled-source", sheets = mtcars)
#' gs_theme_professional(ss1)
#'
#' out <- read_pretty_sheet(ss1)
#' attr(out, "prettysheets_theme")
#'
#' ss2 <- googlesheets4::gs4_create("styled-copy")
#' write_pretty_sheet(ss2, data = out, sheet = "Sheet1")
#' }
read_pretty_sheet <- function(ss, sheet = NULL, range = NULL, col_names = TRUE, ...) {
  ss <- googlesheets4::as_sheets_id(ss)

  data <- googlesheets4::range_read(ss, sheet = sheet, range = range, col_names = col_names, ...)

  theme <- tryCatch(
    build_theme_from_sheet(ss, sheet, data),
    error = function(e) {
      cli::cli_warn(c(
        "Couldn't reconstruct formatting from this sheet; returning data with a plain default theme.",
        "i" = conditionMessage(e)
      ))
      gs_theme()
    }
  )

  attr(data, "prettysheets_theme") <- theme
  data
}
