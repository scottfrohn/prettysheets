#' Classify a column's data for theme purposes
#'
#' Spreadsheets have no native "integer" cell type -- every number, however
#' it got there, is stored as a plain double. So rather than trust an R
#' column's class (which would only ever say "integer" for data that's
#' still in memory as an R `integer` vector, and would say "double" for
#' *everything* read back from a live sheet via [googlesheets4::range_read()],
#' even a column of whole numbers like `mtcars$cyl`), this classifies
#' numeric columns by their *values*: all-whole-number columns are
#' `"integer"`, anything else numeric is `"double"`. This makes theming
#' behave the same whether it's working from data you still have in memory
#' or from data read back live from the sheet.
#'
#' @param x A vector (one column of a data frame).
#' @return One of `"integer"`, `"double"`, `"url"`, `"long_text"`, or
#'   `"character"` (the last is also the fallback for logical, Date, and
#'   other types this doesn't otherwise recognize).
#' @keywords internal
#' @noRd
classify_column <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x)) {
    non_na <- x[!is.na(x)]
    if (length(non_na) > 0 && all(non_na == floor(non_na))) {
      return("integer")
    }
    return("double")
  }

  if (is.character(x)) {
    non_na <- x[!is.na(x) & nzchar(x)]
    if (length(non_na) == 0) {
      return("character")
    }
    if (all(grepl("^(https?://|www\\.)\\S+$", non_na, ignore.case = TRUE))) {
      return("url")
    }
    if (max(nchar(non_na)) > 50) {
      return("long_text")
    }
    return("character")
  }

  "character"
}

#' The `range_format()` arguments a theme applies for one column type
#'
#' @param type One of `classify_column()`'s return values.
#' @param wrap_long_text Logical; whether a `"long_text"` column should
#'   wrap (`TRUE`) or stay clipped to one line like a `"url"` column
#'   (`FALSE`, the default). See a `gs_theme_*()` function.
#' @return A named list of arguments, suitable for
#'   `do.call(range_format, c(list(ss = ..., range = ...), theme_column_format(type)))`.
#' @keywords internal
#' @noRd
theme_column_format <- function(type, wrap_long_text = FALSE) {
  switch(type,
    integer = list(
      number_format = "#,##0", number_format_type = "NUMBER",
      horizontal_alignment = "CENTER"
    ),
    double = list(
      number_format = "#,##0.00", number_format_type = "NUMBER",
      horizontal_alignment = "CENTER"
    ),
    url = list(horizontal_alignment = "LEFT", wrap_strategy = "CLIP"),
    long_text = list(
      horizontal_alignment = "LEFT",
      wrap_strategy = if (isTRUE(wrap_long_text)) "WRAP" else "CLIP"
    ),
    list(horizontal_alignment = "LEFT") # "character", and the fallback for anything else
  )
}

#' A fixed pixel width for columns a theme sizes by hand instead of
#' estimating from content
#'
#' `"url"` and `"long_text"` are wrapped/clipped by design -- sizing them
#' from their raw (unwrapped) content would defeat the point, stretching
#' `"url"` out to full URL length or `"long_text"` out to full-paragraph
#' length. These get a fixed, reasonable width instead. `NULL` for any
#' other type, which `theme_column_width()` sizes from content instead.
#'
#' @param sheet_id Numeric sheet id.
#' @param col_index 1-based column index.
#' @param pixel_size Target width in pixels.
#' @keywords internal
#' @noRd
set_column_width_request <- function(sheet_id, col_index, pixel_size) {
  list(
    updateDimensionProperties = list(
      range = list(
        sheetId = sheet_id,
        dimension = "COLUMNS",
        startIndex = as.integer(col_index - 1L),
        endIndex = as.integer(col_index)
      ),
      properties = list(pixelSize = as.integer(pixel_size)),
      fields = "pixelSize"
    )
  )
}

#' Fixed width (px) for column types sized by hand rather than by content
#'
#' `NULL` for any type `theme_column_width()` should size instead. `"url"`
#' stays at the same 100px floor every other column gets at minimum, since
#' it's clipped to one line regardless of how long the underlying URL is.
#' `"long_text"` gets a bit more room for a short preview of the wrapped
#' paragraph.
#' @keywords internal
#' @noRd
theme_fixed_width <- function(type) {
  switch(type, url = 100L, long_text = 340L, NULL)
}

#' Approximate pixels-per-character for Sheets' default font
#'
#' There's no API call that reads back what `googlesheets4::range_autofit()`
#' (or Sheets' own UI autosize) actually decides for a column's width --
#' short of an extra round trip to re-fetch the sheet's grid metadata after
#' the fact, there's no way to know it, and so no way to guarantee it meets
#' a minimum. So themes size their non-fixed-width columns directly from
#' content instead, using this rough per-character estimate (in the
#' neighborhood of Sheets' own default 10pt font) rather than relying on
#' autofit.
#' @keywords internal
#' @noRd
.gs4_theme_px_per_char <- 7

#' Padding (px) added on top of the raw character-width estimate, so text
#' isn't pressed right up against a column's edges
#' @keywords internal
#' @noRd
.gs4_theme_col_padding <- 24

#' A column's values, formatted the way its theme number format will
#' actually display them
#'
#' Widths should be estimated from what a column *looks like*, not its raw
#' values -- `1234567.891` is narrower to read than the `"1,234,567.89"`
#' `theme_column_format()` will actually display, so number columns are
#' formatted the same way before being measured.
#'
#' @param values A column's values.
#' @param type One of `classify_column()`'s return values.
#' @return Character vector, `NA`/blank entries dropped.
#' @keywords internal
#' @noRd
theme_display_strings <- function(values, type) {
  non_na <- values[!is.na(values)]
  if (length(non_na) == 0) {
    return(character(0))
  }
  switch(type,
    integer = formatC(round(non_na), format = "d", big.mark = ","),
    double = formatC(non_na, format = "f", digits = 2, big.mark = ","),
    as.character(non_na)
  )
}

#' A comfortable column width (px), estimated from header + data content
#'
#' @param header The column's header string.
#' @param display_values The column's values, already formatted the way
#'   they'll actually be displayed (see `theme_display_strings()`).
#' @param min_width Width (px) below which this never goes, regardless of
#'   how short the content is. Default `100`.
#' @return Integer pixel width.
#' @keywords internal
#' @noRd
theme_column_width <- function(header, display_values, min_width = 100L) {
  strings <- c(header, display_values)
  strings <- strings[!is.na(strings) & nzchar(strings)]
  widest <- if (length(strings)) max(nchar(strings)) else 0L
  px <- as.integer(round(widest * .gs4_theme_px_per_char + .gs4_theme_col_padding))
  max(px, as.integer(min_width))
}

#' Build one `updateCells` request giving every cell in a one-column range
#' its own hyperlink, matching that cell's own value
#'
#' Every other per-column theme format is one request applying the *same*
#' formatting across a whole column. A hyperlink can't work that way -- each
#' cell's `link` has to point at *that cell's own* URL, which differs row
#' to row -- but that doesn't require one request *per cell* either.
#' `UpdateCellsRequest` accepts a `rows` list of per-cell `CellData`
#' entries covering `grid_range` top to bottom, all under one shared
#' `fields` mask, so a whole url column -- however many rows -- becomes
#' exactly one request object, not one per row.
#'
#' @param grid_range A `GridRange` covering exactly the column's data rows
#'   (one column wide).
#' @param values The column's URL values (character; `NA`/blank entries are
#'   left unlinked, via an empty `CellData`, rather than erroring).
#' @return A request object, ready for `send_or_queue()`.
#' @keywords internal
#' @noRd
url_hyperlink_request <- function(grid_range, values) {
  rows <- lapply(values, function(url) {
    cell <- if (is.na(url) || !nzchar(url)) {
      empty_json_object()
    } else {
      list(userEnteredFormat = list(textFormat = list(link = list(uri = url))))
    }
    list(values = list(cell))
  })
  list(
    updateCells = list(
      range = grid_range,
      rows = rows,
      fields = "userEnteredFormat.textFormat.link"
    )
  )
}

#' Default field values for a `gs_theme()`'s `header` list
#' @keywords internal
#' @noRd
.theme_header_defaults <- list(bold = TRUE, background_color = NULL, font_color = NULL)

#' Default field values for a `gs_theme()`'s `border` list
#' @keywords internal
#' @noRd
.theme_border_defaults <- list(sides = "outside", color = NULL, style = "SOLID")

#' Validate and fill in defaults for a `gs_theme()`'s `header` argument
#' @keywords internal
#' @noRd
validate_theme_header <- function(header) {
  header <- header %||% list()
  unknown <- setdiff(names(header), names(.theme_header_defaults))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown {.arg header} field{?s}: {.val {unknown}}.")
  }
  utils::modifyList(.theme_header_defaults, header)
}

#' Validate a `gs_theme()`'s `banding` argument
#' @keywords internal
#' @noRd
validate_theme_banding <- function(banding) {
  if (is.null(banding)) {
    return(NULL)
  }
  unknown <- setdiff(names(banding), c("band1", "band2"))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown {.arg banding} field{?s}: {.val {unknown}}.")
  }
  if (is.null(banding$band1) || is.null(banding$band2)) {
    cli::cli_abort("{.arg banding} needs both {.field band1} and {.field band2} colors.")
  }
  banding[c("band1", "band2")]
}

#' Validate and fill in defaults for a `gs_theme()`'s `border` argument
#' @keywords internal
#' @noRd
validate_theme_border <- function(border) {
  if (is.null(border)) {
    return(NULL)
  }
  unknown <- setdiff(names(border), names(.theme_border_defaults))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown {.arg border} field{?s}: {.val {unknown}}.")
  }
  border <- utils::modifyList(.theme_border_defaults, border)
  if (is.null(border$color)) {
    cli::cli_abort("{.arg border} needs a {.field color}.")
  }
  border
}

#' Build a reusable table style for `write_pretty_sheet()`/`gs_theme_*()`
#'
#' A `gs_theme()` is a bundle of formatting decisions -- header style,
#' zebra-striping colors, per-column overrides, a table border, and
#' conditional-format rules -- captured as one inspectable, reusable value
#' instead of a pile of individual [range_format()]/[range_add_conditional_format()]
#' calls. Pass it to [write_pretty_sheet()]'s `theme` argument, or build on
#' one of the built-in presets ([gs_theme_clean()] and friends only *apply*
#' a theme immediately; see `gs_theme_presets` for how they're built)
#' with [modify_theme()].
#'
#' @param header A list with any of `bold` (logical, default `TRUE`),
#'   `background_color`, `font_color` (colors -- hex, named R color, or
#'   [gs4_palette] name). Applied to row 1 directly via [range_format()],
#'   regardless of whether `banding` is also set.
#' @param banding `NULL` (the default, no zebra-striping) or a list with
#'   `band1`/`band2` colors, added as a native Sheets "banded range" (the
#'   same construct as Format > Alternating colors in the Sheets UI). The
#'   banded range's header row color reuses `header$background_color`
#'   (falling back to `band1` if that's `NULL`), so the two mechanisms
#'   never disagree.
#' @param columns A named list, one entry per column you want to override
#'   the type-inferred default for (see `classify_column()`/
#'   `theme_column_format()`), keyed by the column's header name, e.g.
#'   `list(price = list(number_format = "$#,##0.00"))`. Each entry is a
#'   list of [range_format()]-style arguments, layered on top of (not
#'   replacing) that column's type-based default.
#' @param border `NULL` (the default, no border) or a list with `color`
#'   (required), `sides` (default `"outside"`), `style` (default
#'   `"SOLID"`) -- see [range_format_border()].
#' @param wrap_long_text Logical; whether `"long_text"` columns wrap onto
#'   multiple lines instead of clipping to one. Default `FALSE`.
#' @param freeze_header Logical; freeze row 1 via [sheet_freeze()].
#'   Default `TRUE`.
#' @param conditional_formats A list of formatting calls to apply after
#'   theming, each either `list(range = ..., rule = ..., format = ...)`
#'   for a boolean rule (see [cf_cell_value()] and friends, [cf_format()])
#'   or `list(range = ..., gradient = ...)` for a gradient rule (see
#'   [cf_gradient()]). `range` is commonly a [gs4_cols()] selection, so the
#'   rule keeps applying to the right columns regardless of how many rows
#'   of data you write.
#' @return A `prettysheets_theme` object.
#' @export
#' @examples
#' my_theme <- gs_theme(
#'   header = list(bold = TRUE, background_color = "#1F4E79", font_color = "white"),
#'   banding = list(band1 = "white", band2 = "#DCE6F1"),
#'   columns = list(price = list(number_format = "$#,##0.00")),
#'   border = list(color = "gray40")
#' )
#' my_theme
gs_theme <- function(header = list(),
                      banding = NULL,
                      columns = list(),
                      border = NULL,
                      wrap_long_text = FALSE,
                      freeze_header = TRUE,
                      conditional_formats = list()) {
  if (!is.list(columns) || (length(columns) > 0 && is.null(names(columns)))) {
    cli::cli_abort("{.arg columns} must be a named list, e.g. {.code list(price = list(number_format = \"$#,##0.00\"))}.")
  }
  if (!is.list(conditional_formats)) {
    cli::cli_abort("{.arg conditional_formats} must be a list.")
  }

  structure(
    list(
      header = validate_theme_header(header),
      banding = validate_theme_banding(banding),
      columns = columns,
      border = validate_theme_border(border),
      wrap_long_text = isTRUE(wrap_long_text),
      freeze_header = isTRUE(freeze_header),
      conditional_formats = conditional_formats
    ),
    class = "prettysheets_theme"
  )
}

#' Tweak an existing theme
#'
#' Returns a modified copy of `theme` -- the original is untouched.
#' `header`, `banding`, `border`, and `columns` are *merged* into the
#' corresponding part of `theme` (via [utils::modifyList()]) rather than
#' replacing it outright, so e.g. `modify_theme(theme, banding = list(band2
#' = "lightyellow"))` only changes `band2`, leaving `band1` as `theme`
#' already had it, and `modify_theme(theme, columns = list(qty =
#' list(bold = TRUE)))` adds (or overrides) just the `qty` entry without
#' disturbing any other column override already in `theme`.
#' `wrap_long_text`, `freeze_header`, and `conditional_formats` replace the
#' corresponding part of `theme` outright when supplied.
#'
#' @param theme A `gs_theme()` object.
#' @param header,banding,columns,border,wrap_long_text,freeze_header,conditional_formats
#'   Same meaning as in [gs_theme()]. Omit (the default, `NULL`) to leave
#'   that part of `theme` unchanged.
#' @return A new `prettysheets_theme` object.
#' @export
#' @examples
#' \dontrun{
#' modify_theme(gs_theme_professional(), banding = list(band2 = "lightyellow"))
#' }
modify_theme <- function(theme,
                          header = NULL,
                          banding = NULL,
                          columns = NULL,
                          border = NULL,
                          wrap_long_text = NULL,
                          freeze_header = NULL,
                          conditional_formats = NULL) {
  if (!inherits(theme, "prettysheets_theme")) {
    cli::cli_abort("{.arg theme} must be a {.cls prettysheets_theme} object (see {.fn gs_theme}).")
  }

  gs_theme(
    header = if (!is.null(header)) utils::modifyList(theme$header, header) else theme$header,
    banding = if (!is.null(banding)) utils::modifyList(theme$banding %||% list(), banding) else theme$banding,
    columns = if (!is.null(columns)) utils::modifyList(theme$columns, columns) else theme$columns,
    border = if (!is.null(border)) utils::modifyList(theme$border %||% .theme_border_defaults, border) else theme$border,
    wrap_long_text = if (!is.null(wrap_long_text)) wrap_long_text else theme$wrap_long_text,
    freeze_header = if (!is.null(freeze_header)) freeze_header else theme$freeze_header,
    conditional_formats = if (!is.null(conditional_formats)) conditional_formats else theme$conditional_formats
  )
}

#' @export
print.prettysheets_theme <- function(x, ...) {
  cat("<prettysheets_theme>\n")
  cat("  header:  ", paste(sprintf("%s=%s", names(x$header), vapply(x$header, function(v) paste(format(v), collapse = ","), character(1))), collapse = ", "), "\n", sep = "")
  cat("  banding: ", if (is.null(x$banding)) "none" else paste(sprintf("%s=%s", names(x$banding), unlist(x$banding)), collapse = ", "), "\n", sep = "")
  cat("  border:  ", if (is.null(x$border)) "none" else paste(sprintf("%s=%s", names(x$border), unlist(x$border)), collapse = ", "), "\n", sep = "")
  cat("  columns: ", if (length(x$columns) == 0) "none (type-inferred defaults only)" else paste(names(x$columns), collapse = ", "), "\n", sep = "")
  cat("  wrap_long_text: ", x$wrap_long_text, "\n", sep = "")
  cat("  freeze_header:  ", x$freeze_header, "\n", sep = "")
  cat("  conditional_formats: ", length(x$conditional_formats), " rule(s)\n", sep = "")
  invisible(x)
}

#' Apply a themed header/banding/per-column format to a table
#'
#' The shared engine behind [write_pretty_sheet()] and every `gs_theme_*()`
#' function: clears existing formatting on the table, top-aligns every
#' cell, styles the header, optionally adds a native banded range (header
#' color + alternating row colors -- see `?"Add banding"` in the Sheets
#' API docs) for zebra striping, applies type-appropriate alignment/number
#' format/wrap per column (see `classify_column()`), layering on any
#' per-column-name override from `theme$columns`, sizes each column from
#' its content (see `theme_column_width()`, floored at a minimum width so
#' no column ends up too narrow to read), and gives `"url"` columns their
#' own per-cell hyperlink. Optionally draws a border around the whole
#' table, applies `theme$conditional_formats`, and freezes the header row.
#'
#' @param ss,sheet,data See a `gs_theme_*()` function.
#' @param theme A `gs_theme()` object.
#' @return `ss`, invisibly.
#' @keywords internal
#' @noRd
apply_gs_theme <- function(ss, sheet = NULL, data = NULL, theme) {
  ss <- googlesheets4::as_sheets_id(ss)

  if (!inherits(theme, "prettysheets_theme")) {
    cli::cli_abort("{.arg theme} must be a {.cls prettysheets_theme} object (see {.fn gs_theme}).")
  }

  if (is.null(data)) {
    data <- googlesheets4::range_read(ss, sheet = sheet, col_names = TRUE)
  }

  n_row <- nrow(data)
  n_col <- ncol(data)
  if (n_row == 0 || n_col == 0) {
    cli::cli_abort(c(
      "No data found to theme.",
      "i" = "Pass {.arg data} explicitly, or write data to the sheet before theming it."
    ))
  }

  table_range <- cellranger::cell_limits(ul = c(1, 1), lr = c(n_row + 1L, n_col))
  grid_range <- resolve_grid_range(ss, sheet, table_range)

  sheet_clear_format(ss, sheet = sheet, range = table_range)

  range_format(ss, sheet = sheet, range = table_range, vertical_alignment = "TOP")

  header <- theme$header
  if (!is.null(header$bold) || !is.null(header$background_color) || !is.null(header$font_color)) {
    range_format(
      ss, sheet = sheet, range = cellranger::cell_rows(1),
      bold = header$bold, background_color = header$background_color, font_color = header$font_color
    )
  }

  if (!is.null(theme$banding)) {
    send_or_queue(ss, list(
      addBanding = list(
        bandedRange = list(
          range = grid_range,
          rowProperties = list(
            headerColor = gs4_color(header$background_color %||% theme$banding$band1),
            firstBandColor = gs4_color(theme$banding$band1),
            secondBandColor = gs4_color(theme$banding$band2)
          )
        )
      )
    ))
  }

  types <- vapply(data, classify_column, character(1))
  headers <- names(data)

  for (i in seq_len(n_col)) {
    col_range <- cellranger::cell_limits(ul = c(2, i), lr = c(n_row + 1L, i))
    col_format <- theme_column_format(types[[i]], wrap_long_text = theme$wrap_long_text)
    override <- theme$columns[[headers[i]]]
    if (!is.null(override)) {
      col_format <- utils::modifyList(col_format, override)
    }
    do.call(range_format, c(list(ss = ss, sheet = sheet, range = col_range), col_format))

    if (identical(types[[i]], "url")) {
      url_range <- list(
        sheetId = grid_range$sheetId,
        startRowIndex = 1L, # row 2 (0-based), skip the header
        endRowIndex = as.integer(n_row + 1L),
        startColumnIndex = as.integer(i - 1L),
        endColumnIndex = as.integer(i)
      )
      send_or_queue(ss, url_hyperlink_request(url_range, as.character(data[[i]])))
    }

    width <- theme_fixed_width(types[[i]])
    if (is.null(width)) {
      width <- theme_column_width(headers[i], theme_display_strings(data[[i]], types[[i]]))
    }
    send_or_queue(ss, set_column_width_request(grid_range$sheetId, i, width))
  }

  if (!is.null(theme$border)) {
    range_format_border(
      ss, sheet = sheet, range = table_range,
      sides = theme$border$sides, style = theme$border$style, color = theme$border$color
    )
  }

  for (spec in theme$conditional_formats) {
    if (!is.null(spec$gradient)) {
      range_add_gradient_format(ss, sheet = sheet, range = spec$range, gradient = spec$gradient)
    } else {
      range_add_conditional_format(
        ss, sheet = sheet, range = spec$range, rule = spec$rule, format = spec$format
      )
    }
  }

  if (isTRUE(theme$freeze_header)) {
    sheet_freeze(ss, sheet = sheet, n_rows = 1)
  }

  invisible(ss)
}

#' Default "pretty" table themes, pipeable after writing a sheet
#'
#' Apply a consistent look to a table you just wrote: bold header, a fill
#' color for the header and alternating body rows (as a native Sheets
#' "banded range" -- editable afterward via Format > Alternating colors in
#' the Sheets UI), top-aligned cells, and type-appropriate column
#' formatting inferred from the data (whole numbers centered with
#' thousands separators, decimals centered at 2 places, short text
#' left-aligned and sized to its content, URLs left-aligned, clipped to
#' one line, and each one turned into an actual clickable hyperlink to
#' itself (a formatting property, not an `=HYPERLINK()` formula -- the
#' cell's text and value are untouched), and long text (over 50
#' characters) left-aligned and given a fixed width instead of sized to
#' its (unwrapped) content -- clipped to one line by default, or wrapped
#' if `wrap_long_text = TRUE`. Every column is at least 100px wide,
#' however short its content.
#'
#' `data` is optional. Supply it (the same data frame you just wrote) to
#' skip an extra read of the sheet; omit it and the theme reads the
#' written values back live to infer column types -- which is what makes
#' `googlesheets4::sheet_write(data, ss) |> gs_theme_clean()` work as a
#' pipe, since `sheet_write()` returns `ss`, not `data`.
#'
#' Re-theming a table already themed by one of these functions (or already
#' carrying alternating colors applied some other way, e.g. by hand in the
#' Sheets UI) works fine: `sheet_clear_format()` -- which every
#' `gs_theme_*()` call runs first -- removes any existing banded
#' range/alternating colors overlapping the table before the new one is
#' added, since the Sheets API doesn't allow two banded ranges to overlap
#' the same cells.
#'
#' Column widths are estimated from content (header and formatted data,
#' whichever is wider) rather than read back from
#' [googlesheets4::range_autofit()], since there's no API call that reports
#' what autofit actually decided for a column -- which means, unlike that
#' `googlesheets4` function, sizing columns doesn't send its own request
#' outside this package's usual batching: every `gs_theme_*()` call (widths
#' included) goes through the same queueing as the rest of `prettysheets`, so
#' wrapping one in [prettysheets_batch()] *does* reduce it to a single API call.
#'
#' @param ss Something that identifies a Google Sheet, processed through
#'   [googlesheets4::as_sheets_id()].
#' @param sheet Sheet to theme, by name or position. Defaults to the first
#'   visible sheet.
#' @param data Optional; the data frame already written to `sheet`. If
#'   omitted, it's read back live via [googlesheets4::range_read()].
#' @param wrap_long_text Logical; whether `"long_text"` columns (character
#'   data over 50 characters) wrap onto multiple lines. Default `FALSE`,
#'   so they're clipped to one line -- like a `"url"` column -- rather
#'   than growing every row to fit the tallest cell.
#' @return `ss`, invisibly, so this chains with `|>`/`%>%`.
#' @name gs_theme_presets
#' @examples
#' \dontrun{
#' ss <- googlesheets4::gs4_create("prettysheets-theme-demo", sheets = mtcars)
#' gs_theme_clean(ss)
#'
#' # or, piped straight from writing the data, with no `data` argument:
#' googlesheets4::sheet_write(mtcars, ss, sheet = "mtcars") |> gs_theme_professional()
#' }
NULL

#' @describeIn gs_theme_presets Neutral grayscale: dark gray header, white/light
#'   gray alternating rows.
#' @export
gs_theme_clean <- function(ss, sheet = NULL, data = NULL, wrap_long_text = FALSE) {
  apply_gs_theme(ss, sheet = sheet, data = data, theme = gs_theme_preset("clean", wrap_long_text))
}

#' @describeIn gs_theme_presets Corporate blue: dark blue header, white/pale-blue
#'   alternating rows, thin blue border around the whole table.
#' @export
gs_theme_professional <- function(ss, sheet = NULL, data = NULL, wrap_long_text = FALSE) {
  apply_gs_theme(ss, sheet = sheet, data = data, theme = gs_theme_preset("professional", wrap_long_text))
}

#' @describeIn gs_theme_presets Bright and playful: magenta header, alternating
#'   pastel yellow/blue rows.
#' @export
gs_theme_fun <- function(ss, sheet = NULL, data = NULL, wrap_long_text = FALSE) {
  apply_gs_theme(ss, sheet = sheet, data = data, theme = gs_theme_preset("fun", wrap_long_text))
}

#' @describeIn gs_theme_presets Modern and moody: near-black header, white/lavender
#'   alternating rows, thin purple border around the whole table.
#' @export
gs_theme_stylish <- function(ss, sheet = NULL, data = NULL, wrap_long_text = FALSE) {
  apply_gs_theme(ss, sheet = sheet, data = data, theme = gs_theme_preset("stylish", wrap_long_text))
}

#' Build one of the built-in preset themes as a `gs_theme()` object
#'
#' The `gs_theme_*()` functions (`gs_theme_clean()` and friends) apply a
#' preset immediately to a sheet; this builds the same preset as a plain
#' `gs_theme()` object instead, so it can be passed to
#' [write_pretty_sheet()]'s `theme` argument (which also accepts the
#' preset's bare name as a string, e.g. `theme = "professional"`) or
#' tweaked first with [modify_theme()].
#'
#' @param name One of `"clean"`, `"professional"`, `"fun"`, `"stylish"`.
#' @param wrap_long_text Logical; see [gs_theme()].
#' @return A `prettysheets_theme` object.
#' @keywords internal
#' @noRd
gs_theme_preset <- function(name = c("clean", "professional", "fun", "stylish"),
                             wrap_long_text = FALSE) {
  name <- match.arg(name)
  switch(name,
    clean = gs_theme(
      header = list(background_color = gs4_palette_color("gray"), font_color = "black"),
      banding = list(band1 = "white", band2 = gs4_palette_color("light gray 2")),
      wrap_long_text = wrap_long_text
    ),
    professional = gs_theme(
      header = list(background_color = gs4_palette_color("dark cornflower blue 3"), font_color = "white"),
      banding = list(band1 = "white", band2 = gs4_palette_color("light cornflower blue 3")),
      border = list(color = gs4_palette_color("dark cornflower blue 2")),
      wrap_long_text = wrap_long_text
    ),
    fun = gs_theme(
      header = list(background_color = gs4_palette_color("magenta"), font_color = "white"),
      banding = list(band1 = gs4_palette_color("cyan"), band2 = gs4_palette_color("yellow")),
      wrap_long_text = wrap_long_text
    ),
    stylish = gs_theme(
      header = list(background_color = gs4_palette_color("dark gray 4"), font_color = "white"),
      banding = list(band1 = "white", band2 = gs4_palette_color("light purple 3")),
      border = list(color = gs4_palette_color("dark purple 1")),
      wrap_long_text = wrap_long_text
    )
  )
}
