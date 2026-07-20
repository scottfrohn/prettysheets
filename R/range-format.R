#' Apply basic cell formatting to a range
#'
#' Formats fonts, colors, number formats, alignment, and text wrap for a
#' range of cells. This is `prettysheets`'s equivalent of
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
#'   `"B:B"`, `"2:5"`), a `cellranger` helper such as
#'   [cellranger::cell_rows()] or [cellranger::cell_cols()], or [gs4_cols()]
#'   to target one or more columns by header name.
#' @param bold,italic,underline,strikethrough Logical.
#' @param font_size Numeric point size.
#' @param font_family Character, e.g. `"Arial"`.
#' @param font_color,background_color A color: hex string (`"#F4CCCC"`),
#'   named R color (`"steelblue"`), or [gs4_palette] name (`"dark yellow 1"`),
#'   converted via [gs4_color()].
#' @param number_format A number format pattern string, e.g. `"$#,##0.00"`
#'   or `"0.0%"`.
#' @param number_format_type One of `"NUMBER"`, `"PERCENT"`, `"CURRENCY"`,
#'   `"DATE"`, `"TIME"`, `"DATE_TIME"`, `"SCIENTIFIC"`, `"TEXT"`. Only used
#'   if `number_format` is supplied.
#' @param horizontal_alignment One of `"LEFT"`, `"CENTER"`, `"RIGHT"`.
#' @param vertical_alignment One of `"TOP"`, `"MIDDLE"`, `"BOTTOM"`.
#' @param wrap_strategy One of `"OVERFLOW_CELL"`, `"LEGACY_WRAP"`, `"CLIP"`,
#'   `"WRAP"`.
#' @param link A URL string. Makes every cell in `range` a clickable
#'   hyperlink to this same URL, as a formatting property (no
#'   `=HYPERLINK()` formula, no change to the cells' underlying values).
#'   Since this applies one URL to the *whole* range, it's meant for
#'   ranges you want all pointing at one link (e.g. a "source" column
#'   header); to give each cell in a column its own distinct link (e.g. a
#'   column of different URLs), call `range_format()` once per cell instead.
#' @param clear_first Logical; clear all existing formatting on `range`
#'   (see [sheet_clear_format()]) before applying the formatting requested
#'   in this call. Default `FALSE`, so repeated `range_format()` calls layer
#'   on top of each other as described above. Set `TRUE` to start from a
#'   blank slate instead — useful if the range may already carry formatting
#'   you don't want to inherit.
#' @param include_header Logical; whether a *whole-column* `range` includes
#'   its header row. Applies to [gs4_cols()] (which otherwise selects just
#'   the data rows below the header) and to any other fully open-ended
#'   whole-column reference with no row bounds at all -- an A1 shorthand
#'   string like `"B:B"`/`"B:D"`, or a bare [cellranger::cell_cols()].
#'   Default `FALSE`, so a call like `range_format(range = "B:B", ...)`
#'   formats row 2 downward, not row 1 -- pass `TRUE` for the literal A1
#'   meaning of `"B:B"` (the whole column, header included). Has no effect
#'   on ranges that already specify explicit rows (`"B2:B33"`, `"A1:D10"`)
#'   or on whole-row references (`"2:5"`), since there's no header *row* to
#'   exclude from a selection that already names its rows, or from a
#'   selection of specific rows in the first place.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes a `RepeatCellRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#repeatcellrequest>
#' @export
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("prettysheets-demo", sheets = mtcars)
#' range_format(ss, range = "A1:K1", bold = TRUE, background_color = "#D9E1F2")
#' range_format(ss, range = "B2:B33", number_format = "0.0", horizontal_alignment = "RIGHT")
#' range_format(ss, range = "A1:K1", clear_first = TRUE, bold = TRUE)
#' range_format(ss, range = gs4_cols("mpg", "hp"), number_format = "0.0")
#' range_format(ss, range = gs4_cols("mpg"), include_header = TRUE, bold = TRUE)
#' range_format(ss, range = "B:B", number_format = "0.0") # skips row 1
#' range_format(ss, range = "B:B", include_header = TRUE, number_format = "0.0") # row 1 too
#' range_format(ss, range = "A2", link = "https://www.khanacademy.org")
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
                          wrap_strategy = NULL,
                          link = NULL,
                          clear_first = FALSE,
                          include_header = FALSE) {
  ss <- googlesheets4::as_sheets_id(ss)
  grid_ranges <- resolve_grid_ranges(ss, sheet, range, include_header = include_header)

  built <- build_cell_format(
    bold = bold, italic = italic, underline = underline, strikethrough = strikethrough,
    font_size = font_size, font_family = font_family, font_color = font_color,
    background_color = background_color, number_format = number_format,
    number_format_type = number_format_type, horizontal_alignment = horizontal_alignment,
    vertical_alignment = vertical_alignment, wrap_strategy = wrap_strategy, link = link
  )

  if (length(built$fields) == 0 && !isTRUE(clear_first)) {
    cli::cli_abort("{.fn range_format} needs at least one formatting argument (e.g. {.arg bold}, {.arg background_color}), unless {.arg clear_first} is TRUE.")
  }

  for (grid_range in grid_ranges) {
    if (isTRUE(clear_first)) {
      send_or_queue(ss, clear_format_request(grid_range))
    }

    if (length(built$fields) > 0) {
      request <- list(
        repeatCell = list(
          range = grid_range,
          cell = list(userEnteredFormat = built$cell_format),
          fields = paste(built$fields, collapse = ",")
        )
      )
      send_or_queue(ss, request)
    }
  }

  invisible(ss)
}

#' Apply a border to a range
#'
#' @inheritParams range_format
#' @param sides Which sides to draw, any combination of:
#'   \describe{
#'     \item{`"top"`, `"bottom"`, `"left"`, `"right"`}{A single outer side.}
#'     \item{`"outside"`}{Shorthand for all four outer sides, i.e.
#'       `c("top", "bottom", "left", "right")`. This is what `range_format_border()`
#'       drew when `sides` was `"all"` prior to prettysheets 0.0.0.9000's border rework.}
#'     \item{`"inside"`}{Shorthand for the gridlines *between* cells in the
#'       range, i.e. `c("innerHorizontal", "innerVertical")`.}
#'     \item{`"innerHorizontal"`, `"innerVertical"`}{A single inner gridline
#'       direction on its own.}
#'     \item{`"all"`}{Shorthand for `"outside"` and `"inside"` together —
#'       every side and every inner gridline.}
#'   }
#'   Defaults to `"outside"`.
#' @param style Border line style: one of `"SOLID"`, `"SOLID_MEDIUM"`,
#'   `"SOLID_THICK"`, `"DASHED"`, `"DOTTED"`, `"DOUBLE"`, or `"NONE"` to
#'   remove the border(s) named in `sides` — an `UpdateBordersRequest` is
#'   independent of `RepeatCellRequest`, so this clears borders without
#'   touching any other formatting (fill, font, number format, etc.) on the
#'   same range.
#' @param color Border color: hex string, named R color, or [gs4_palette]
#'   name (converted via [gs4_color()]). Ignored when `style = "NONE"`.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes an `UpdateBordersRequest`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#updatebordersrequest>
#' @export
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("prettysheets-demo", sheets = mtcars)
#' range_format_border(ss, range = "A1:K1")
#' range_format_border(ss, range = "A1:K33", sides = "all", color = "gray80")
#' range_format_border(ss, range = "A1:K33", sides = "all", style = "NONE")
#' range_format_border(ss, range = gs4_cols("mpg", "hp"))
#' }
range_format_border <- function(ss,
                                 sheet = NULL,
                                 range = NULL,
                                 sides = "outside",
                                 style = "SOLID",
                                 color = "black") {
  ss <- googlesheets4::as_sheets_id(ss)
  grid_ranges <- resolve_grid_ranges(ss, sheet, range)

  sides <- resolve_border_sides(sides)

  border_spec <- if (identical(style, "NONE")) {
    list(style = style)
  } else {
    list(style = style, color = gs4_color(color))
  }

  for (grid_range in grid_ranges) {
    update_borders <- list(range = grid_range)
    for (side in sides) {
      update_borders[[side]] <- border_spec
    }
    send_or_queue(ss, list(updateBorders = update_borders))
  }

  invisible(ss)
}
