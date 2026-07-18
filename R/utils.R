#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Drop NULL elements from a list
#' @keywords internal
#' @noRd
compact <- function(x) x[!vapply(x, is.null, logical(1))]

#' Convert a color specification to the Sheets API `Color` schema
#'
#' Accepts a hex string (e.g. `"#F4CCCC"`), a named R color (e.g. `"steelblue"`,
#' anything [grDevices::col2rgb()] understands), or an already-built list with
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
gs4_color <- function(x, alpha = NULL) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x) && all(c("red", "green", "blue") %in% names(x))) {
    return(x)
  }
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
