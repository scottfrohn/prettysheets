# prettysheets

<!-- badges: start -->
[![R-CMD-check](https://github.com/scottfrohn/prettysheets/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/scottfrohn/prettysheets/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Cell formatting and conditional formatting for Google Sheets, built as a
natural extension of [`googlesheets4`](https://googlesheets4.tidyverse.org/).

`prettysheets` reuses `googlesheets4`'s own auth, sheet-identification, and
range-specification conventions, and follows its naming scheme
(`gs4_*` / `sheet_*` / `range_*`) so functions are easy to guess if you
already know `googlesheets4`.

```r
library(googlesheets4)
library(prettysheets)

ss <- gs4_create("prettysheets-demo", sheets = mtcars)

ss |>
  range_write_format(
    data = mtcars, range = "A1",
    header_background_color = "#D9E1F2",
    conditional_formats = list(
      list(
        range = "F2:F33",
        rule = cf_cell_value(">", 3.5),
        format = cf_format(background_color = "#F4CCCC")
      )
    )
  )

range_add_gradient_format(
  ss, range = "D2:D33",
  gradient = cf_gradient(min_color = "white", max_color = "forestgreen")
)

sheet_freeze(ss, n_rows = 1)
```

## Installation

```r
<<<<<<< HEAD
pak::pak("scottfrohn/prettysheets")
=======
devtools::install_github("scottfrohn/prettysheets")
>>>>>>> 9bd72ecaa4f7bccf5ca4feb2863a4cbdf3346d44
```

## What's already covered by googlesheets4 (not duplicated here)

- **`range_autofit()`** — auto-sizes columns/rows to fit content.
- **`sheet_write()`** — already bolds the header row and freezes row 1
  automatically when you write a data frame as a "table." `prettysheets`'s
  `sheet_format_header()` / `sheet_freeze()` are for the case where you
  wrote data with `range_write()` instead (which does *not* auto-style).

## Reusable table themes

Beyond one-off formatting calls, `prettysheets` also has a theme system for
building and reapplying a consistent table style:

```r
my_theme <- gs_theme(
  header = list(background_color = "#1F4E79", font_color = "white"),
  banding = list(band1 = "white", band2 = "#DCE6F1"),
  columns = list(price = list(number_format = "$#,##0.00")),
  border = list(color = "gray40")
)

write_pretty_sheet(ss, data = mtcars, sheet = "mtcars", theme = my_theme)

# read the data back together with the theme actually applied to it
styled <- read_pretty_sheet(ss, sheet = "mtcars")
write_pretty_sheet(ss2, data = styled, sheet = "mtcars") # same look, new sheet
```

Four built-in presets (`gs_theme_clean()`, `gs_theme_professional()`,
`gs_theme_fun()`, `gs_theme_stylish()`) apply immediately to an existing
sheet; see `vignette("prettysheets")` for a fuller walkthrough.

## A known API limitation (not a prettysheets limitation)

The Sheets API's conditional formatting only supports **boolean** and
**gradient** rules — there's no data-bar rule in the API at all (data bars
are a Sheets UI-only feature), so `prettysheets` doesn't offer
`range_add_databar()`.
