#' Write a data frame and format it in one call
#'
#' Combines [googlesheets4::range_write()] with `gs4mattr`'s formatting
#' functions. Internally calls `range_write(..., reformat = FALSE)` *before*
#' applying formatting — `range_write()`'s default `reformat = TRUE` clears
#' existing formatting on write, which would otherwise immediately erase
#' whatever this function had just applied.
#'
#' @inheritParams googlesheets4::range_write
#' @param header_bold,header_background_color,header_font_color Passed to
#'   [range_format()] for the header row, if `col_names = TRUE`. Set
#'   `header_bold = NULL` (and leave the color args `NULL`) to skip header
#'   styling entirely.
#' @param freeze_header Logical; freeze the header row via [sheet_freeze()].
#'   Only applies if `col_names = TRUE`.
#' @param conditional_formats A list of formatting calls to apply after
#'   writing, each itself a `list(range = ..., rule = ..., format = ...)` for
#'   a boolean rule, or `list(range = ..., gradient = ...)` for a gradient
#'   rule. Optional.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("gs4mattr-write-demo")
#' range_write_format(
#'   ss, data = mtcars, sheet = "Sheet1", range = "A1",
#'   header_background_color = "#D9E1F2",
#'   conditional_formats = list(
#'     list(range = "F2:F33", rule = cf_cell_value(">", 3.5),
#'          format = cf_format(background_color = "#F4CCCC"))
#'   )
#' )
#' }
range_write_format <- function(ss,
                                data,
                                sheet = NULL,
                                range = NULL,
                                col_names = TRUE,
                                header_bold = TRUE,
                                header_background_color = "#F3F3F3",
                                header_font_color = NULL,
                                freeze_header = TRUE,
                                conditional_formats = list()) {
  ss <- googlesheets4::as_sheets_id(ss)

  googlesheets4::range_write(
    ss, data = data, sheet = sheet, range = range,
    col_names = col_names, reformat = FALSE
  )

  if (isTRUE(col_names) && !is.null(header_bold)) {
    sheet_format_header(
      ss, sheet = sheet, row = 1,
      bold = header_bold, background_color = header_background_color,
      font_color = header_font_color, freeze = freeze_header
    )
  }

  for (spec in conditional_formats) {
    if (!is.null(spec$gradient)) {
      range_add_gradient_format(ss, sheet = sheet, range = spec$range, gradient = spec$gradient)
    } else {
      range_add_conditional_format(
        ss, sheet = sheet, range = spec$range, rule = spec$rule, format = spec$format
      )
    }
  }

  invisible(ss)
}
