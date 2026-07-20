#' prettysheets: Cell Formatting and Conditional Formatting for googlesheets4
#'
#' `prettysheets` extends `googlesheets4` with a formatting layer for Google
#' Sheets. Function names and argument order mirror `googlesheets4`'s own
#' conventions (`gs4_*` spreadsheet-level, `sheet_*` worksheet-level,
#' `range_*` range-level) so it behaves like a natural extension. It always
#' reuses whatever auth `googlesheets4::gs4_auth()` already has active — no
#' separate auth system.
#'
#' Key functions:
#' \itemize{
#'   \item [range_format()] / [range_format_border()] — basic cell formatting
#'   \item [sheet_freeze()], [sheet_format_header()], [sheet_format_tabcolor()] — sheet-level structure
#'   \item [range_merge()] / [range_unmerge()] — merged cells
#'   \item [gs4_cols()] — reference a range by column header name instead of letter
#'   \item [gs4_glimpse_cols()] — preview a data frame's columns alongside
#'     their spreadsheet column letters
#'   \item [range_add_conditional_format()] + [cf_cell_value()] and friends — boolean conditional formatting
#'   \item [range_add_gradient_format()] + [cf_gradient()] — gradient conditional formatting
#'   \item [range_write_format()] — write a data frame and format it in one call
#'   \item [gs_theme()] / [modify_theme()] + [write_pretty_sheet()] — build a
#'     reusable table style and apply it when writing a data frame
#'   \item [gs_theme_clean()] and friends (see `gs_theme_presets`) — apply a
#'     built-in table style immediately to an existing sheet
#'   \item [read_pretty_sheet()] — read a sheet's data together with a
#'     reconstructed [gs_theme()] describing how it's formatted
#'   \item [prettysheets_batch()] — batch several formatting calls into one API request
#' }
#'
#' Note: `googlesheets4::range_autofit()` already covers column/row
#' auto-sizing, and `googlesheets4::sheet_write()` already applies header
#' styling and freezes row 1 automatically — `prettysheets` doesn't duplicate
#' either, and its own header/freeze helpers are meant as the manual
#' equivalent for data written with `range_write()` instead.
#'
#' Also note: the Sheets API's conditional formatting only supports boolean
#' and gradient rules — there is no data-bar rule in the API (data bars are
#' a Sheets UI-only feature), so `prettysheets` doesn't offer one.
#'
#' @keywords internal
"_PACKAGE"
