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
# once pushed to GitHub:
devtools::install_github("scottfrohn/prettysheets")
```

## What's already covered by googlesheets4 (not duplicated here)

- **`range_autofit()`** — auto-sizes columns/rows to fit content.
- **`sheet_write()`** — already bolds the header row and freezes row 1
  automatically when you write a data frame as a "table." `prettysheets`'s
  `sheet_format_header()` / `sheet_freeze()` are for the case where you
  wrote data with `range_write()` instead (which does *not* auto-style).

## A known API limitation (not a prettysheets limitation)

The Sheets API's conditional formatting only supports **boolean** and
**gradient** rules — there's no data-bar rule in the API at all (data bars
are a Sheets UI-only feature), so `prettysheets` doesn't offer
`range_add_databar()`.

## Development status

This package was scaffolded by hand (DESCRIPTION, R/, tests/) without a
local R installation available at the time, so nothing here has been run or
roxygenized yet. Before you rely on it:

1. `devtools::document()` to regenerate `NAMESPACE`/`man/` from the roxygen
   comments already in `R/*.R`.
2. `devtools::load_all()` and `devtools::check()`.
3. `testthat::test_local()` — the offline unit tests (color conversion,
   the `fields`-mask logic, conditional-format builders) should all pass
   with no network or Google auth needed.
4. Everything that actually talks to the Sheets API (`range_format()`,
   `sheet_freeze()`, `range_add_conditional_format()`, etc.) has **not**
   been exercised against a real spreadsheet yet — do that next, ideally
   against a throwaway test Sheet, and watch in particular for:
   - the exact `request_generate()` endpoint nickname
     (`"sheets.spreadsheets.batchUpdate"` — confirm against
     `googlesheets4::gs4_endpoints()`),
   - whether the `fields` mask syntax used for `gridProperties` in
     `sheet_freeze()` (`"gridProperties(frozenRowCount,frozenColumnCount)"`)
     is accepted as-is or needs adjusting.
