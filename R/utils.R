#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create `sheet` first if it doesn't already exist in `ss`, sized to fit
#' `data`
#'
#' Unlike `googlesheets4::sheet_write()`, `googlesheets4::range_write()`
#' requires its target (work)sheet to already exist -- it errors (`"Can't
#' find a sheet with this name"`) rather than creating one. That's
#' surprising for `range_write_format()`/`write_pretty_sheet()`, since both
#' otherwise mirror `range_write()`'s own flexible `ss`/`sheet`/`range`
#' arguments and read as drop-in replacements for it. This adds the
#' missing sheet first (via `googlesheets4::sheet_add()`) so writing to a
#' brand-new sheet name works the same as writing to an existing one.
#'
#' `googlesheets4::sheet_add()` alone leaves a new sheet at whatever
#' default grid size the Sheets API assigns (commonly 1000 rows x 26
#' columns) -- `sheet_write()` avoids that by sizing a sheet it creates to
#' the data being written instead, and this matches that: when `data` is
#' supplied, the new sheet is immediately resized (via
#' `googlesheets4::sheet_resize(..., exact = TRUE)`) to `nrow(data)` (plus
#' one row for the header, if `col_names`) by `ncol(data)`.
#'
#' Only applies when `sheet` is a single name (character string) that
#' doesn't already exist -- a numeric position that doesn't exist is a
#' real error (there's no sensible "create position 5"), so it's left for
#' `range_write()` itself to reject. `sheet = NULL` (the default sheet) is
#' also left alone, since there's always at least one sheet to fall back
#' to. An *existing* sheet is never resized -- only one this function
#' itself just created.
#'
#' @param ss A `sheets_id`.
#' @param sheet Sheet name or position, as passed by the caller.
#' @param data The data about to be written, used to size a newly created
#'   sheet. `NULL` (the default) skips sizing -- the sheet is created at
#'   the API's own default size.
#' @param col_names Logical; whether a header row is also being written,
#'   so the new sheet gets one extra row beyond `nrow(data)`.
#' @return `ss`, invisibly.
#' @keywords internal
#' @noRd
ensure_sheet_exists <- function(ss, sheet, data = NULL, col_names = TRUE) {
  if (is.character(sheet) && length(sheet) == 1) {
    existing <- googlesheets4::sheet_properties(ss)$name
    if (!(sheet %in% existing)) {
      googlesheets4::sheet_add(ss, sheet = sheet)

      if (!is.null(data)) {
        n_row <- nrow(data) + if (isTRUE(col_names)) 1L else 0L
        n_col <- ncol(data)
        if (n_row > 0 && n_col > 0) {
          googlesheets4::sheet_resize(ss, sheet = sheet, nrow = n_row, ncol = n_col, exact = TRUE)
        }
      }
    }
  }
  invisible(ss)
}

#' Drop NULL elements from a list
#'
#' Named `drop_nulls()` (not `compact()`) so it doesn't collide with
#' `purrr::compact()` (and similar) when `devtools::load_all()` -- which
#' attaches internal helpers, unlike `library()` -- is used alongside the
#' tidyverse during development.
#' @keywords internal
#' @noRd
drop_nulls <- function(x) x[!vapply(x, is.null, logical(1))]

#' Convert a color specification to the Sheets API `Color` schema
#'
#' Accepts a hex string (e.g. `"#F4CCCC"`), a named R color (e.g. `"steelblue"`,
#' anything [grDevices::col2rgb()] understands), a [gs4_palette] name (e.g.
#' `"light cornflower blue 1"`, `"dark yellow 1"` -- anything
#' [gs4_palette_color()] understands), or an already-built list with
#' `red`/`green`/`blue` components (passed through unchanged, so you can
#' construct a `Color` object by hand if you need `alpha` control the helper
#' doesn't expose yet).
#'
#' @param x A color, or `NULL`.
#' @param alpha Optional alpha channel, 0-1.
#' @return A list with `red`, `green`, `blue` (and optionally `alpha`), each in
#'   `[0, 1]`, as required by the Sheets API's `Color` object. `NULL` if `x`
#'   is `NULL`.
#' @export
#' @examples
#' gs4_color("#F4CCCC")
#' gs4_color("steelblue")
#' gs4_color("light cornflower blue 1")
gs4_color <- function(x, alpha = NULL) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x) && all(c("red", "green", "blue") %in% names(x))) {
    return(x)
  }
  x <- resolve_color_name(x)
  rgb <- grDevices::col2rgb(x, alpha = FALSE) / 255
  out <- list(
    red = unname(rgb[1, 1]),
    green = unname(rgb[2, 1]),
    blue = unname(rgb[3, 1])
  )
  if (!is.null(alpha)) {
    out$alpha <- alpha
  }
  out
}

#' Resolve a color string to something `grDevices::col2rgb()` understands
#'
#' `grDevices::col2rgb()` only knows hex strings and the ~650 named R colors
#' -- it has no idea what `"light cornflower blue 1"` or `"dark yellow 1"`
#' (Google's own palette names, see [gs4_palette]) mean. This tries
#' `col2rgb()` first (so ordinary hex/R-color input is never slowed down by
#' a palette lookup) and only falls back to [gs4_palette_color()] if that
#' fails, so `gs4_color()` (and everything built on it: `range_format()`,
#' `range_format_border()`, `sheet_format_tabcolor()`, the `gs_theme_*()`
#' functions) accepts palette names anywhere a color is expected.
#'
#' @param x A length-1 character color spec, or anything else (passed
#'   through unchanged -- only single strings can be palette names).
#' @return A string `grDevices::col2rgb()` can parse.
#' @keywords internal
#' @noRd
resolve_color_name <- function(x) {
  if (!is.character(x) || length(x) != 1L) {
    return(x)
  }
  ok <- tryCatch(
    {
      grDevices::col2rgb(x)
      TRUE
    },
    error = function(e) FALSE
  )
  if (ok) {
    return(x)
  }
  palette_hex <- tryCatch(gs4_palette_color(x), error = function(e) NULL)
  if (!is.null(palette_hex)) {
    return(palette_hex)
  }
  cli::cli_abort(c(
    "Unknown color: {.val {x}}.",
    "i" = "Must be a hex string (e.g. {.val #F4CCCC}), a named R color (e.g. {.val steelblue}), or a {.code gs4_palette} name (e.g. {.val light cornflower blue 1}; see {.code gs4_palette$name} for the full list)."
  ))
}
