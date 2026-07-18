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
                               wrap_strategy = NULL) {
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
  text_format <- compact(text_format)
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
