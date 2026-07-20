#' Write a data frame and format it in one call
#'
#' Combines [googlesheets4::range_write()] with `prettysheets`'s formatting
#' functions. Internally calls `range_write(..., reformat = FALSE)` *before*
#' applying formatting — `range_write()`'s default `reformat = TRUE` clears
#' existing formatting on write, which would otherwise immediately erase
#' whatever this function had just applied.
#'
#' Unlike `range_write()` itself, `sheet` doesn't need to already exist --
#' if it's a name not already in the spreadsheet, an empty (work)sheet by
#' that name is added first (`range_write()` alone errors in this case,
#' since -- unlike `googlesheets4::sheet_write()` -- it doesn't create
#' sheets on demand), sized to fit `data` (plus a header row, if
#' `col_names`) rather than left at the API's own default grid size.
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
#' ss <- googlesheets4::gs4_create("prettysheets-write-demo")
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
  ensure_sheet_exists(ss, sheet, data = data, col_names = col_names)

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

#' Write a data frame and apply a `gs_theme()` in one call
#'
#' The `theme`-based counterpart to [range_write_format()]: writes `data`
#' with `googlesheets4::range_write(..., reformat = FALSE)`, then applies
#' `theme` via the same engine behind the `gs_theme_*()` presets -- header
#' style, optional banding, per-column type-inferred formatting (with any
#' `theme$columns` overrides layered on top), column widths, an optional
#' border, `theme`'s conditional formats, and freezing the header row.
#'
#' `theme` can be:
#' \itemize{
#'   \item a [gs_theme()] object, built by hand or via [modify_theme()]
#'   \item the bare name of a built-in preset (`"clean"`, `"professional"`,
#'     `"fun"`, or `"stylish"`) -- shorthand for
#'     `gs_theme_clean()`/etc., but returning control to `write_pretty_sheet()`
#'     instead of applying immediately
#'   \item omitted entirely, in which case `write_pretty_sheet()` looks
#'     for a theme attached to `data` -- see [read_pretty_sheet()], which
#'     attaches the theme it detects on a sheet as `attr(data,
#'     "prettysheets_theme")` -- so
#'     `write_pretty_sheet(ss2, data = read_pretty_sheet(ss1))` reuses the
#'     original sheet's look automatically. Falls back to a plain default
#'     theme (bold header, light gray background, no banding/border) if
#'     neither is available.
#' }
#'
#' Like [range_write_format()], `sheet` doesn't need to already exist -- a
#' missing sheet name is created first (sized to fit `data`) instead of
#' erroring.
#'
#' @inheritParams googlesheets4::range_write
#' @param theme A [gs_theme()] object, a preset name string, or `NULL`
#'   (see above).
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("prettysheets-write-demo")
#' write_pretty_sheet(ss, data = mtcars, sheet = "Sheet1", theme = "professional")
#'
#' my_theme <- gs_theme(
#'   header = list(background_color = "#1F4E79", font_color = "white"),
#'   columns = list(mpg = list(number_format = "0.0"))
#' )
#' write_pretty_sheet(ss, data = mtcars, sheet = "Sheet2", theme = my_theme)
#' }
write_pretty_sheet <- function(ss,
                                data,
                                sheet = NULL,
                                range = NULL,
                                col_names = TRUE,
                                theme = NULL) {
  ss <- googlesheets4::as_sheets_id(ss)

  if (is.character(theme) && length(theme) == 1) {
    theme <- gs_theme_preset(theme)
  }
  theme <- theme %||% attr(data, "prettysheets_theme") %||% gs_theme(header = list(background_color = "#F3F3F3"))
  if (!inherits(theme, "prettysheets_theme")) {
    cli::cli_abort("{.arg theme} must be a {.cls prettysheets_theme} object (see {.fn gs_theme}), a preset name, or NULL.")
  }

  ensure_sheet_exists(ss, sheet, data = data, col_names = col_names)

  googlesheets4::range_write(
    ss, data = data, sheet = sheet, range = range,
    col_names = col_names, reformat = FALSE
  )

  if (isTRUE(col_names)) {
    apply_gs_theme(ss, sheet = sheet, data = data, theme = theme)
  }

  invisible(ss)
}
