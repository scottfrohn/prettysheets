#' Build a `CellFormat` object plus its dynamic `fields` update-mask
#'
#' The Sheets API's `RepeatCellRequest` (and similar) require an explicit
#' `fields` mask naming exactly which sub-fields of `CellFormat` are being
#' set — any field *not* named in the mask is left untouched, but if the
#' mask is missing or too broad, unrelated formatting on the same cells gets
#' silently reset. This builder only adds a field (and its mask entry) when
#' the caller actually supplied a value, so `range_format(bold = TRUE)`
#' followed later by `range_format(background_color = "red")` never clobbers
#' the earlier call.
#'
#' Note: per the Sheets API docs, only bold, italic, strikethrough,
#' foreground color, and background color are legal inside a *conditional*
#' format rule (`BooleanRule$format`). Number formats, alignment, wrap, and
#' font family/size are legal for plain cell formatting (`range_format()`)
#' but are silently unsupported by Google inside conditional rules, so
#' `cf_format()` (see conditional-format.R) only exposes the five properties
#' that are actually honored there.
#'
#' `link` sets `userEnteredFormat.textFormat.link.uri`, making the whole
#' cell a clickable hyperlink to that URI as a pure formatting property (no
#' `=HYPERLINK()` formula, no change to the cell's underlying value). Since
#' a hyperlink's target is necessarily specific to one cell, callers that
#' want a different link per cell in a range (see `apply_url_hyperlinks()`
#' in theme.R) issue one single-cell request per cell rather than one
#' request across a whole range.
#'
#' @keywords internal
#' @noRd
build_cell_format <- function(bold = NULL,
                               italic = NULL,
                               underline = NULL,
                               strikethrough = NULL,
                               font_size = NULL,
                               font_family = NULL,
                               font_color = NULL,
                               background_color = NULL,
                               number_format = NULL,
                               number_format_type = "NUMBER",
                               horizontal_alignment = NULL,
                               vertical_alignment = NULL,
                               wrap_strategy = NULL,
                               link = NULL) {
  fields <- character(0)
  fmt <- list()
  text_format <- list()

  add <- function(name, value, field_path) {
    if (!is.null(value)) {
      fields <<- c(fields, field_path)
    }
    value
  }

  text_format$bold <- add("bold", bold, "userEnteredFormat.textFormat.bold")
  text_format$italic <- add("italic", italic, "userEnteredFormat.textFormat.italic")
  text_format$underline <- add("underline", underline, "userEnteredFormat.textFormat.underline")
  text_format$strikethrough <- add("strikethrough", strikethrough, "userEnteredFormat.textFormat.strikethrough")
  text_format$fontSize <- add("fontSize", font_size, "userEnteredFormat.textFormat.fontSize")
  text_format$fontFamily <- add("fontFamily", font_family, "userEnteredFormat.textFormat.fontFamily")
  if (!is.null(font_color)) {
    text_format$foregroundColor <- gs4_color(font_color)
    fields <- c(fields, "userEnteredFormat.textFormat.foregroundColor")
  }
  if (!is.null(link)) {
    text_format$link <- list(uri = link)
    fields <- c(fields, "userEnteredFormat.textFormat.link")
  }
  text_format <- drop_nulls(text_format)
  if (length(text_format)) fmt$textFormat <- text_format

  if (!is.null(background_color)) {
    fmt$backgroundColor <- gs4_color(background_color)
    fields <- c(fields, "userEnteredFormat.backgroundColor")
  }
  if (!is.null(number_format)) {
    fmt$numberFormat <- list(type = number_format_type, pattern = number_format)
    fields <- c(fields, "userEnteredFormat.numberFormat")
  }
  if (!is.null(horizontal_alignment)) {
    fmt$horizontalAlignment <- horizontal_alignment
    fields <- c(fields, "userEnteredFormat.horizontalAlignment")
  }
  if (!is.null(vertical_alignment)) {
    fmt$verticalAlignment <- vertical_alignment
    fields <- c(fields, "userEnteredFormat.verticalAlignment")
  }
  if (!is.null(wrap_strategy)) {
    fmt$wrapStrategy <- wrap_strategy
    fields <- c(fields, "userEnteredFormat.wrapStrategy")
  }

  list(cell_format = fmt, fields = fields)
}

#' Build a `RepeatCellRequest` that clears all cell formatting
#'
#' Sets `fields = "userEnteredFormat"` with `cell = {userEnteredFormat: {}}`
#' -- the shape Google's own docs use for this
#' (<https://developers.google.com/sheets/api/samples/formatting>) -- which
#' per the Sheets API's update-mask semantics *replaces* `userEnteredFormat`
#' with an empty object rather than leaving it untouched, i.e. resets
#' fonts, colors, number formats, alignment, and wrap back to the
#' spreadsheet default. Borders are a separate `Border` property untouched
#' by this request; see [range_format_border()] (`style = "NONE"`) to clear
#' those.
#'
#' @param grid_range A `GridRange` list, as returned by
#'   `resolve_grid_range()`.
#' @keywords internal
#' @noRd
clear_format_request <- function(grid_range) {
  list(
    repeatCell = list(
      range = grid_range,
      cell = list(userEnteredFormat = empty_json_object()),
      fields = "userEnteredFormat"
    )
  )
}

#' An R value that `jsonlite`/`httr2` serialize as `{}`, not `[]`
#'
#' A bare `list()` has no `names` attribute at all, and `jsonlite::toJSON()`
#' treats that as an *unnamed* list -- i.e. a JSON array (`[]`) -- even
#' though it's also, trivially, a valid empty named list. Giving it a
#' zero-length (but non-`NULL`) `names` attribute is what tips `jsonlite`
#' into treating it as a named list instead, so it serializes as an empty
#' JSON object (`{}`). Needed anywhere the Sheets API expects an explicitly
#' empty *object* value (e.g. clearing `userEnteredFormat`) rather than the
#' field being left out of the request entirely.
#'
#' @keywords internal
#' @noRd
empty_json_object <- function() structure(list(), names = character(0))

#' Expand `range_format_border()`'s `sides` shorthand into literal
#' `UpdateBordersRequest` field names
#'
#' `"outside"` -> the four outer sides; `"inside"` -> the two inner
#' gridline directions; `"all"` -> both. Anything else must already be one
#' of the six literal side names, or this errors.
#'
#' @param sides Character vector, as passed to `range_format_border()`.
#' @return Character vector of literal side names (`"top"`, `"bottom"`,
#'   `"left"`, `"right"`, `"innerHorizontal"`, `"innerVertical"`), with
#'   duplicates removed.
#' @keywords internal
#' @noRd
resolve_border_sides <- function(sides) {
  side_shorthand <- list(
    outside = c("top", "bottom", "left", "right"),
    inside = c("innerHorizontal", "innerVertical"),
    all = c("top", "bottom", "left", "right", "innerHorizontal", "innerVertical")
  )
  valid_sides <- c(
    "top", "bottom", "left", "right", "innerHorizontal", "innerVertical",
    names(side_shorthand)
  )
  unknown <- setdiff(sides, valid_sides)
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown {.arg sides} value{?s}: {.val {unknown}}.")
  }
  unique(unlist(lapply(sides, function(s) side_shorthand[[s]] %||% s)))
}
