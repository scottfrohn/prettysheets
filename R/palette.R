# Data for gs4_palette, in Google Sheets color-picker order (columns), with
# each color's full-strength / light-3..1 / dark-1..3 shades (rows).
.gs4_palette_family_names <- c(
  "red berry", "red", "orange", "yellow", "green",
  "cyan", "cornflower blue", "blue", "purple", "magenta"
)

.gs4_palette_family_hex <- list(
  `red berry`       = c("#980000", "#e6b8af", "#dd7e6b", "#cc4125", "#a61c00", "#85200c", "#5b0f00"),
  red               = c("#ff0000", "#f4cccc", "#ea9999", "#e06666", "#cc0000", "#990000", "#660000"),
  orange            = c("#ff9900", "#fce5cd", "#f9cb9c", "#f6b26b", "#e69138", "#b45f06", "#783f04"),
  yellow            = c("#ffff00", "#fff2cc", "#ffe599", "#ffd966", "#f1c232", "#bf9000", "#7f6000"),
  green             = c("#00ff00", "#d9ead3", "#b6d7a8", "#93c47d", "#6aa84f", "#38761d", "#274e13"),
  cyan              = c("#00ffff", "#d0e0e3", "#a2c4c9", "#76a5af", "#45818e", "#134f5c", "#0c343d"),
  `cornflower blue` = c("#4a86e8", "#c9daf8", "#a4c2f4", "#6d9eeb", "#3c78d8", "#1155cc", "#1c4587"),
  blue              = c("#0000ff", "#cfe2f3", "#9fc5e8", "#6fa8dc", "#3d85c6", "#0b5394", "#073763"),
  purple            = c("#9900ff", "#d9d2e9", "#b4a7d6", "#8e7cc3", "#674ea7", "#351c75", "#20124d"),
  magenta           = c("#ff00ff", "#ead1dc", "#d5a6bd", "#c27ba0", "#a64d79", "#741b47", "#4c1130")
)

# rows 2-8 shade order: full, light 3, light 2, light 1, dark 1, dark 2, dark 3
.gs4_palette_shade_prefix <- c("", "light ", "light ", "light ", "dark ", "dark ", "dark ")
.gs4_palette_shade_suffix <- c("", " 3", " 2", " 1", " 1", " 2", " 3")

#' Google Sheets default color palette
#'
#' The 80 named colors shown in Google Sheets' standard font/fill color
#' picker (the grid you see before opening "Custom"), laid out with the
#' same `row`/`col` coordinates as the picker: row 1 is grayscale
#' (black through white), row 2 is the 10 full-strength colors, rows 3-5
#' are those colors lightened (3 = lightest, 1 = least light), and rows
#' 6-8 are them darkened (1 = least dark, 3 = darkest).
#'
#' @format A data frame with 80 rows and 4 variables:
#' \describe{
#'   \item{row}{Row position (1-8) in the Sheets color picker grid.}
#'   \item{col}{Column position (1-10) in the Sheets color picker grid.}
#'   \item{name}{Google's name for the color, e.g. `"light cornflower blue 1"`.}
#'   \item{hex}{Hex code, e.g. `"#c9daf8"`.}
#' }
#' @source The default color grid in the Google Sheets/Docs/Slides font
#'   and fill color pickers.
#' @export
#' @examples
#' gs4_palette
#' subset(gs4_palette, row == 1)
gs4_palette <- local({
  grays <- data.frame(
    row = 1L,
    col = 1:10,
    name = c(
      "black", "dark gray 4", "dark gray 3", "dark gray 2", "dark gray 1",
      "gray", "light gray 1", "light gray 2", "light gray 3", "white"
    ),
    hex = c(
      "#000000", "#434343", "#666666", "#999999", "#b7b7b7",
      "#cccccc", "#d9d9d9", "#efefef", "#f3f3f3", "#ffffff"
    ),
    stringsAsFactors = FALSE
  )

  colors <- do.call(rbind, lapply(seq_along(.gs4_palette_family_names), function(col_i) {
    family <- .gs4_palette_family_names[col_i]
    data.frame(
      row = 1:7 + 1L,
      col = col_i,
      name = paste0(.gs4_palette_shade_prefix, family, .gs4_palette_shade_suffix),
      hex = .gs4_palette_family_hex[[family]],
      stringsAsFactors = FALSE
    )
  }))

  out <- rbind(grays, colors)
  out <- out[order(out$row, out$col), ]
  rownames(out) <- NULL
  out
})

#' Look up a Google Sheets default palette color by name
#'
#' @param name Color name as it appears in [gs4_palette] (e.g.
#'   `"cornflower blue"`, `"light cornflower blue 1"`). Matching is
#'   case-insensitive, ignores leading/trailing whitespace, and treats
#'   `"grey"` and `"gray"` as the same word (e.g. `"dark grey 1"` matches
#'   [gs4_palette]'s `"dark gray 1"`) -- [gs4_palette]'s own `name` column
#'   only spells it `"gray"`, but the two are interchangeable everywhere
#'   else in R (`"gray"`/`"grey"` are already aliases for
#'   [grDevices::col2rgb()]), so this extends the same equivalence to the
#'   numbered/modified palette names that plain `col2rgb()` doesn't know
#'   about (`"dark gray 1"`, `"light gray 2"`, etc. aren't valid base R
#'   color names on their own).
#' @return The hex code, as a string, e.g. `"#4a86e8"`.
#' @export
#' @examples
#' gs4_palette_color("cornflower blue")
#' gs4_palette_color("Dark Red 2")
#' gs4_palette_color("dark grey 1") # same as "dark gray 1"
gs4_palette_color <- function(name) {
  key <- gsub("grey", "gray", tolower(trimws(name)), fixed = TRUE)
  match_idx <- match(key, tolower(gs4_palette$name))
  if (is.na(match_idx)) {
    guesses <- agrep(key, tolower(gs4_palette$name), value = TRUE, max.distance = 0.2)
    hint <- if (length(guesses)) {
      paste0(" Did you mean: ", paste(unique(guesses), collapse = ", "), "?")
    } else {
      ""
    }
    cli::cli_abort("Unknown gs4_palette color name: {.val {name}}.{hint}")
  }
  gs4_palette$hex[match_idx]
}
