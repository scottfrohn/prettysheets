#' Apply basic cell formatting to a range
#'
#' Formats fonts, colors, number formats, alignment, and text wrap for a
#' range of cells. This is `gs4mattr`'s equivalent of
#' [googlesheets4::range_write()] for styling rather than data: same `ss`,
#' `sheet`, `range` argument order, and it accepts the same range inputs
#' (A1 strings or `cellranger` helpers like [cellranger::cell_rows()]).
#'
#' Only the arguments you supply are touched — see the `fields` update-mask
#' note in `build_cell_format()` — so you can call `range_format()` more
#' than once on the same range to layer on formatting incrementally, without
#' one call undoing another.
#'
#' @param ss Something that identifies a Google Sheet, processed through
#'   [googlesheets4::as_sheets_id()] (a Sheet id, URL, dribble, or
#'   `googlesheets4_spreadsheet`).
#' @param sheet Sheet to format, by name or position. Defaults to the first
#'   visible sheet, or the sheet named in `range` if `range` is a
#'   `cell_limits` object built from `"SheetName!A1:B2"`.
#' @param range `NULL` (whole sheet), an A1-style string (`"A1:D10"`,
#'   `"B:B"`, `"2:5"`), or a `cellranger` helper such as
#'   [cellranger::cell_rows()] or [cellranger::cell_cols()].
#' @param bold,italic,underline,strikethrough Logical.
#' @param font_size Numeric point size.
#' @param font_family Character, e.g. `"Arial"`.
#' @param font_color,background_color A color: hex string (`"#F4CCCC"`) or
#'   named R color (`"steelblue"`), converted via [gs4_color()].
#' @param number_format A number format pattern string, e.g. `"$#,##0.00"`
#'   or `"0.0%"`.
#' @param number_format_type One of `"NUMBER"`, `"PERCENT"`, `"CURRENCY"`,
#'   `"DATE"`, `"TIME"`, `"DATE_TIME"`, `"SCIENTIFIC"`, `"TEXT"`. Only used
#'   if `number_format` is supplied.
#' @param horizontal_alignment One of `"LEFT"`, `"CENTER"`, `"RIGHT"`.
#' @param vertical_alignment One of `"TOP"`, `"MIDDLE"`, `"BOTTOM"`.
#' @param wrap_strategy One of `"OVERFLOW_CELL"`, `"LEGACY_WRAP"`, `"CLIP"`,
#'   `"WRAP"`.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes a `RepeatCellRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#repeatcellrequest>
#' @export
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("gs4mattr-demo", sheets = mtcars)
#' range_format(ss, range = "A1:K1", bold = TRUE, background_color = "#D9E1F2")
#' range_format(ss, range = "B2:B33", number_format = "0.0", horizontal_alignment = "RIGHT")
#' }
range_format <- function(ss,
                          sheet = NULL,
                          range = NULL,
                          bold = NULL,
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
  ss <- googlesheets4::as_sheets_id(ss)
  grid_range <- resolve_grid_range(ss, sheet, range)

  built <- build_cell_format(
    bold = bold, italic = italic, underline = underline, strikethrough = strikethrough,
    font_size = font_size, font_family = font_family, font_color = font_color,
    background_color = background_color, number_format = number_format,
    number_format_type = number_format_type, horizontal_alignment = horizontal_alignment,
    vertical_alignment = vertical_alignment, wrap_strategy = wrap_strategy
  )

  if (length(built$fields) == 0) {
    cli::cli_abort("{.fn range_format} needs at least one formatting argument (e.g. {.arg bold}, {.arg background_color}).")
  }

  request <- list(
    repeatCell = list(
      range = grid_range,
      cell = list(userEnteredFormat = built$cell_format),
      fields = paste(built$fields, collapse = ",")
    )
  )
  send_or_queue(ss, request)
}

#' Apply a border to a range
#'
#' @inheritParams range_format
#' @param sides Which sides to draw: any of `"top"`, `"bottom"`, `"left"`,
#'   `"right"`, `"innerHorizontal"`, `"innerVertical"`, or `"all"` as a
#'   shorthand for all four outer sides.
#' @param style Border line style: one of `"SOLID"`, `"SOLID_MEDIUM"`,
#'   `"SOLID_THICK"`, `"DASHED"`, `"DOTTED"`, `"DOUBLE"`.
#' @param color Border color: hex string or named R color.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes an `UpdateBordersRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#updatebordersrequest>
#' @export
range_format_border <- function(ss,
                                 sheet = NULL,
                                 range = NULL,
                                 sides = "all",
                                 style = "SOLID",
                                 color = "black") {
  ss <- googlesheets4::as_sheets_id(ss)
  grid_range <- resolve_grid_range(ss, sheet, range)

  if ("all" %in% sides) {
    sides <- c("top", "bottom", "left", "right")
  }
  border_spec <- list(style = style, color = gs4_color(color))

  update_borders <- list(range = grid_range)
  for (side in sides) {
    update_borders[[side]] <- border_spec
  }

  request <- list(updateBorders = update_borders)
  send_or_queue(ss, request)
}
