# dev/functional_tests.R
#
# Manual, interactive walkthrough of every exported prettysheets function against
# a REAL Google Sheet. This is not an automated test suite (see
# tests/testthat/ for that) -- there's no `expect_equal()` for "does this
# look bold and pink," so the pattern here is: run a section, then look at
# the browser tab that pops open in section 0 and confirm it looks right
# (refresh that tab after each section -- there's no gs4_browse() call
# repeated below, just the one at setup).
#
# Run this section-by-section (Cmd+Enter line by line, or select a section
# and Cmd+Enter), not all at once -- you'll want to actually look at the
# sheet between steps.
#
# Requires: a Google account you're willing to authenticate googlesheets4
# against. The very first googlesheets4 call below will pop a browser tab
# asking you to sign in and grant access -- that's normal and one-time
# (cached after that).

# ----- Prep
library(tidyverse)
library(googlesheets4)
library(googledrive)

gs4_auth()


devtools::load_all()    # Load all changes to package
# library(prettysheets)   # Load package as published
#

# ── 0. Setup: create one throwaway spreadsheet for this whole session ──────
# Everything below writes into this single sheet. Delete it at the very end
# (last section) once you've eyeballed everything.

## Test sheet name
ss <- "https://docs.google.com/spreadsheets/d/11xchp8Ov6MytIKcgvTICThXpWAdAEcc_4cYa-m3V1l8/"

gs4_browse(ss)

## Write sheets
write_sheet(mtcars, ss = ss, sheet = "basic")
write_sheet(starwars, ss = ss, sheet = "complex")
write_sheet(data.frame(score = c(45, 62, 78, 90, 33, 88, 55, 71),
                       flag = c("", "", "urgent", "", "urgent", "", "", "urgent")), 
            ss = ss,
            sheet = "conditional")

# ── 1. gs4_color() ──────────────────────────────────────────────────────────
# Pure/offline -- no sheet interaction, just confirm the conversion looks right
gs4_color("#F4CCCC")
gs4_color("steelblue")
gs4_color("black", alpha = 0.5)
gs4_color(NULL) # should be NULL

# gs4_palette names work too now -- col2rgb() doesn't know these, so
# gs4_color() falls back to gs4_palette_color() when col2rgb() fails
gs4_color("light green 1")
gs4_color("dark yellow 1")
gs4_color("Dark Yellow 1") # case/whitespace-insensitive, same as gs4_palette_color()


# ── 2. gs4_palette / gs4_palette_color() ────────────────────────────────────
# Pure/offline -- the default 80-color Google Sheets picker grid, and a
# name -> hex lookup over it, for easy reference when you don't want to hunt
# down a hex code by hand
gs4_palette
subset(gs4_palette, row == 1) # the grayscale row
gs4_palette_color("cornflower blue")
gs4_palette_color("light cornflower blue 1")
gs4_palette_color("Dark Red 2") # case/whitespace-insensitive
gs4_palette_color("cornflowr blue") # typo close to a real name -- should
                                     # error WITH a "did you mean" hint
gs4_palette_color("not a real color") # not close to anything -- should
                                       # error with NO hint (nothing to guess)


# ── 3. range_format() -- basic cell formatting ─────────────────────────────
# Clear formatting
sheet_clear_format(ss, sheet = "basic")
sheet_clear_format(ss, sheet = "conditional")
sheet_clear_format(ss, sheet = "conditional")

# range = "A1:L1"
# Expect: row 1 of "basic" bold with a light blue background
range_format(ss, sheet = "basic", range = "A1:L1",
             bold = TRUE, background_color = "#D9E1F2")

# range = "1:1"
# Expect: row 1 of "basic" bold with a light pink background
range_format(ss, sheet = "basic", range = "1:1",
             bold = TRUE, background_color = "#D9A1F2")

# range = "B:B"
# Expect: column B (mpg) right-aligned, one decimal place
range_format(ss, sheet = "basic", range = "B2:B33",
             number_format = "0.000", horizontal_alignment = "LEFT")

# Expect: same result as the call above (rows 2:33, NOT row 1) via Google
# Sheets' "B:B" whole-column A1 shorthand -- cellranger's as.cell_limits()
# doesn't understand this shorthand on its own, so prettysheets recognizes it
# first and routes it through cellranger::cell_cols() instead.
# include_header defaults to FALSE, so row 1 (the header) is skipped here
# just like it would be for gs4_cols() -- even though literal A1 notation
# says "B:B" should include row 1
range_format(ss, sheet = "basic", range = "B:B",
             number_format = "0.00", horizontal_alignment = "CENTER")

# Expect: this time row 1 IS included -- include_header = TRUE restores
# "B:B"'s literal A1 meaning (the whole column, header and all)
range_format(ss, sheet = "basic", range = "B:B",
             include_header = TRUE, horizontal_alignment = "RIGHT")

# Expect: rows 2 through 5 turn left-aligned, via the "2:5" whole-row
# shorthand (same mechanism, routed through cellranger::cell_rows())
range_format(ss, sheet = "basic", range = "2:5", horizontal_alignment = "LEFT")

# Expect: the "disp" and "wt" cols turn center-aligned. A BARE column-name string
# does NOT work as `range` (it isn't A1 notation, so this would error) --
# wrap it in gs4_cols() to reference a column by its header name instead
range_format(ss, sheet = "basic", 
             range = gs4_cols("disp", "wt"), 
             horizontal_alignment = "CENTER")

# Confirm calling range_format() again with a DIFFERENT property doesn't
# wipe out the bold/background from the first call above (the fields-mask
# guarantee) -- expect: row 1 still bold+blue, AND now italic too
range_format(ss, sheet = "basic", range = "1:1", italic = TRUE)

# Expect: clear_first = TRUE wipes the bold/italic/background set above
# FIRST, so row 1 ends up with only strikethrough set -- not bold, not
# italic, no blue background
range_format(ss, sheet = "basic", range = "1:1",
             clear_first = TRUE, strikethrough = TRUE)

# Expect: H3:I4 turns bold with a dark yellow background -- a gs4_palette
# name passed straight as background_color, exercising gs4_color()'s
# palette-name fallback
range_format(ss, sheet = "basic", range = "H3:I4",
             bold = TRUE, background_color = "dark yellow 1")

# Expect: A2 becomes a clickable hyperlink to khanacademy.org (its display
# text/value is untouched -- link is a pure textFormat property)
range_format(ss, sheet = "basic", range = "A2", link = "https://www.khanacademy.org")

# Expect: basic goes back to fully unformatted -- bold/background/etc.
# from gs_theme_clean() AND the alternating-color banding it added are both
# gone (banding is a separate sheet-level object, so sheet_clear_format()
# has to remove it explicitly, not just reset cell formatting)
sheet_clear_format(ss, sheet = "basic")


# ── 4. range_format_border() ────────────────────────────────────────────────
# Expect: a solid box around the OUTSIDE of D3:F5 only -- no lines between
# the 9 cells inside it (sides defaults to "outside")
range_format_border(ss, sheet = "basic", range = "D3:F5",
                    style = "SOLID_MEDIUM", color = gs4_palette_color("dark gray 3"))

# Expect: D3:F5's outer box is unchanged, but now every gridline BETWEEN
# the cells inside it is also drawn (sides = "inside")
range_format_border(ss, sheet = "basic", range = "D3:F5",
                    sides = "inside", style = "DOTTED", color = "gray60")

# Expect: same D3:F5, but every border -- outside AND inside -- is gone.
# style = "NONE" clears borders via their own UpdateBordersRequest, without
# touching the cell fill/font formatting applied elsewhere
range_format_border(ss, sheet = "basic", range = "D3:F5",
                    sides = "all", style = "NONE")


# ── 5. sheet_clear_format() ─────────────────────────────────────────────────
# First, put some formatting somewhere so there's something to clear
range_format(ss, sheet = "basic", range = "H3:I4",
             bold = TRUE, background_color = "yellow")

# Expect: H3:I4 goes back to completely unformatted (no bold, no fill).
# range = NULL would clear the WHOLE sheet instead -- scoped here to H3:I4
# so nothing else on "basic" is touched. Also removes any banded range
# (alternating colors) overlapping H3:I4, if there is one -- see the
# basic repro up in section 3 for a banding-specific demo
sheet_clear_format(ss, sheet = "basic", range = "H3:I4")


# ── 6. sheet_freeze() ────────────────────────────────────────────────────────
# Expect: row 1 of "basic" freezes (stays visible while scrolling down)
sheet_freeze(ss, sheet = "basic", n_rows = 1)


# ── 7. sheet_format_header() ────────────────────────────────────────────────
# Expect: row 1 of "conditional" becomes bold with a light grey background,
# AND freezes automatically (freeze = TRUE is the default)
sheet_format_header(ss, sheet = "conditional")


# ── 8. sheet_format_tabcolor() ───────────────────────────────────────────────
# Expect: the "basic" tab at the bottom of the browser turns green
sheet_format_tabcolor(ss, sheet = "basic", color = "forestgreen")

# Expect: the "basic" tab color at the bottom of the browser resets
sheet_format_tabcolor(ss, sheet = "basic", color = NULL)

# Expect: same tab turns a lighter green -- a gs4_palette name this time
sheet_format_tabcolor(ss, sheet = "basic", color = "light green 1")


# ── 9. range_merge() / range_unmerge() ──────────────────────────────────────
# Expect: D1:E1 on "basic" becomes one merged cell
range_merge(ss, sheet = "basic", range = "D2:E2", type = "all")

# Expect: D1:E1 splits back into two separate cells
range_unmerge(ss, sheet = "basic", range = "D2:E2")


# ── 10. gs4_cols() -- reference a range by column name ──────────────────────
# Expect: the "mpg" and "hp" columns on "basic" turn right-aligned with one
# decimal place, same result as the "B2:B33" call in section 3 but found by
# name instead of letter. No `data` argument is supplied, so gs4_cols()
# reads the header row live to locate "mpg" and "hp" -- and because "cyl"
# and "disp" sit between them, this sends two requests under the hood, one
# per contiguous column, so both still get formatted
range_format(ss, sheet = "basic", range = gs4_cols("mpg", "hp"),
             number_format = "0.0", horizontal_alignment = "RIGHT",
             font_color = "white",
             background_color = "cornflower blue")

# Expect: the "wt" column AND its header cell both turn bold -- gs4_cols()
# excludes the header by default, but include_header = TRUE brings it back in
range_format(ss, sheet = "basic", range = gs4_cols("wt"),
             include_header = TRUE, bold = TRUE)


# ── 10b. gs4_glimpse_cols() -- offline/pure ─────────────────────────────────
# Expect: a two-row printout, column letters (A, B, C, ...) above the
# matching data frame column names, wrapped into more than one block if
# there are too many columns to fit the console width
gs4_glimpse_cols(mtcars)
gs4_glimpse_cols(g6_items)


# ── 11. Conditional formatting: rule builders (all offline/pure) ───────────
cf_cell_value(">", 100)
cf_cell_value("between", c(10, 20))
cf_text_contains("urgent")
cf_text_eq("urgent")
cf_custom_formula("=$B2>50")
cf_blank()
cf_not_blank()
cf_format(bold = TRUE, background_color = "#F4CCCC")
cf_gradient(min_color = "white", max_color = "forestgreen")


# ── 12. range_add_conditional_format() -- boolean rule ──────────────────────
# Expect: on "conditional", any score > 75 in column A gets a light red
# background
range_add_conditional_format(
  ss, sheet = "conditional", range = "A2:A9",
  rule = cf_cell_value(">", 75),
  format = cf_format(background_color = "#F4CCCC")
)

# Expect: any cell in column B containing the word "urgent" turns bold
range_add_conditional_format(
  ss, sheet = "conditional", range = "B2:B9",
  rule = cf_text_contains("urgent"),
  format = cf_format(bold = TRUE, font_color = "blue")
)


# ── 13. range_add_gradient_format() -- gradient rule ────────────────────────
# Expect: column A on "conditional" gets a white-to-green color scale based
# on value (low score = white, high score = green)
range_add_gradient_format(
  ss, sheet = "conditional", range = "A2:A9",
  gradient = cf_gradient(min_color = "white", max_color = "forestgreen")
)


# ── 14. range_write_format() -- combined write + format ─────────────────────
# Expect: a brand new "combined" sheet appears, header row styled, and any
# value > 20 in column "x" gets a light red background -- all from one call
combined_data <- data.frame(x = c(5, 15, 25, 35), y = letters[1:4])

range_write_format(
  ss, sheet = "combined",
  data = combined_data,
  header_background_color = "#D9E1F2",
  conditional_formats = list(
    list(range = "A2:A5", rule = cf_cell_value(">", 20),
         format = cf_format(background_color = "#F4CCCC"))
  )
)

# Expect: column "x" on "combined" turns bold. gs4_cols(data = ...) matches
# names(combined_data) locally -- no extra API call to read the header,
# handy right after a range_write_format() call with the same data
range_format(ss, sheet = "combined", range = gs4_cols("x", data = combined_data),
             bold = TRUE)


# ── 15. prettysheets_batch() -- multiple calls as one API request ──────────
# Expect: same visual result as calling these separately, but sent in a
# single batchUpdate (check your network tab / just trust the timing if
# you want to get fancy -- functionally it should look identical either way)
prettysheets_batch({
  range_format(ss, sheet = "basic", range = "A1:L1", strikethrough = FALSE)
  range_format_border(ss, sheet = "basic", range = "A1:L1",
                      sides = "all", style = "DASHED", color = "grey40")
})


# ── 16. gs_theme_*() -- default pretty themes, pipeable after writing a sheet
# Demo data exercising every column type the themes auto-detect: integer,
# double, short character, long text (> 50 chars), and URLs
theme_demo <- data.frame(
  id = 1:5,
  score = c(88.5, 92.25, 76.1, 99.99, 81.4),
  status = c("active", "inactive", "active", "pending", "active"),
  notes = c(
    "ok",
    "This is a much longer note that should trigger the long-text wrap behavior since it's well over fifty characters.",
    "ok",
    "Another long note, also well past the fifty character threshold for wrapping.",
    "ok"
  ),
  homepage = c(
    "https://example.com/one", "https://example.com/two", "https://example.com/three",
    "https://example.com/four", "https://example.com/five"
  ),
  stringsAsFactors = FALSE
)

# Expect: bold dark-gray header, alternating white/light-gray rows, id and
# score centered with number formatting (columns sized to fit that content,
# at least 100px even though "id"/"score" are short), notes clipped to one
# line (wrap_long_text defaults to FALSE now) at its usual fixed width,
# homepage clipped to one line at 100px wide, AND each homepage cell now an
# actual clickable hyperlink to its own URL (click one -- it should
# navigate, and the cell's displayed text is still the plain URL string,
# not a formula). `data` supplied directly here, so no extra read of the
# sheet
googlesheets4::sheet_write(theme_demo, ss, sheet = "theme_clean")
gs_theme_clean(ss, sheet = "theme_clean", data = theme_demo)

# Expect: Take the existing basic tab and make clean format
gs_theme_clean(ss, sheet = "basic")

# Expect: same as above, except notes now WRAPS onto multiple lines (and
# keeps its wider fixed column) instead of being clipped to one line
googlesheets4::sheet_write(theme_demo, ss, sheet = "theme_clean_wrapped")
gs_theme_clean(ss, sheet = "theme_clean_wrapped", data = theme_demo, wrap_long_text = TRUE)

# Expect: same formatting logic with a corporate blue palette and a thin
# border around the table. This time `data` is NOT supplied -- demonstrates
# the write_sheet() |> gs_theme_*() pipe, where the theme reads the sheet
# back live to infer column types
googlesheets4::sheet_write(theme_demo, ss, sheet = "theme_professional") |>
  gs_theme_professional(sheet = "theme_professional", wrap_long_text = T)

# Expect: bright magenta header, playful alternating yellow/blue rows
googlesheets4::sheet_write(theme_demo, ss, sheet = "theme_fun")
gs_theme_fun(ss, sheet = "theme_fun")

# Expect: near-black header, white/lavender alternating rows, thin purple
# border
googlesheets4::sheet_write(theme_demo, ss, sheet = "theme_stylish")
gs_theme_stylish(ss, sheet = "theme_stylish", data = theme_demo)


# ── 17. gs_theme() / write_pretty_sheet() -- reusable theme objects ─────────
# Unlike gs_theme_clean() & co. (which apply immediately), gs_theme() just
# BUILDS a theme -- inspect it, reuse it, hand it to write_pretty_sheet()
my_theme <- gs_theme(
  header = list(background_color = "#0A2A66", font_color = "white"),
  banding = list(band1 = "white", band2 = "#94f7d4ff"),
  columns = list(score = list(number_format = "0.00", horizontal_alignment = "CENTER")),
  border = list(color = "gray40")
)
my_theme # print method -- eyeball the summary before sending anything to the API

# Expect: a new "pretty_custom" sheet, written AND themed in one call -- dark
# blue header/white text, white/pale-blue banding, thin gray border, "score"
# right-aligned with one decimal (the column override winning over the
# type-inferred default), header frozen (freeze_header defaults to TRUE)
googlesheets4::sheet_write(theme_demo, ss, sheet = "pretty_custom")
write_pretty_sheet(ss, data = theme_demo, sheet = "pretty_custom", theme = my_theme)

# Expect: same visual result as gs_theme_professional(), but via the string
# shorthand instead of an object -- confirms write_pretty_sheet() accepts a
# bare preset name
write_pretty_sheet(ss, data = theme_demo, sheet = "pretty_preset_string", theme = "professional")

# Expect: bold header, light gray background, no banding/border -- the plain
# fallback theme, since neither `theme` nor an attached theme is supplied
write_pretty_sheet(ss, data = theme_demo, sheet = "pretty_default")


# ── 18. modify_theme() -- tweak a theme without rebuilding it ───────────────
# gs_theme_preset() is unexported (@noRd), but devtools::load_all() attaches
# internal helpers too, so it's reachable here for dev purposes
tweaked <- modify_theme(
  gs_theme_preset("professional"),
  banding = list(band2 = "lightyellow"), # band1 untouched
  columns = list(id = list(bold = TRUE)) # adds an override without disturbing others
)

# Expect: same corporate-blue header/border as theme_professional, but with
# a pale YELLOW second band instead of pale blue, and "id" bold
write_pretty_sheet(ss, data = theme_demo, sheet = "pretty_tweaked", theme = tweaked)


# ── 19. read_pretty_sheet() -- round-trip a sheet's look ────────────────────
# Give the source sheet a conditional format too, so we can confirm that
# round-trips along with the rest of the theme

sheet_clear_format(ss, sheet = "pretty_custom")
range_add_conditional_format(
  ss, sheet = "pretty_custom", range = gs4_cols("score"),
  rule = cf_cell_value(">", 90),
  format = cf_format(bold = TRUE, background_color = "#F4CCCC")
)

captured <- read_pretty_sheet(ss, sheet = "pretty_custom")
captured # a normal tibble -- data prints exactly like read_sheet() would
attr(captured, "prettysheets_theme") # the reconstructed gs_theme() object --
                                     # expect header/banding/columns$score to
                                     # match my_theme, plus one captured
                                     # conditional_formats entry for "score"


captured_g6_eoy_ia <- read_pretty_sheet("https://docs.google.com/spreadsheets/d/1EntAGbKLhxaqfolfo6ZOtZsF-_sh1Y6i_ESgFK5nvDQ/",
                                        sheet = "Full")

attr(captured_g6_eoy_ia, "prettysheets_theme")


# Expect: a brand-new sheet with FEWER rows than "pretty_custom" (a subset of
# theme_demo), yet it still comes out looking identical -- same header/
# banding/column formatting, AND the conditional format still only highlights
# "score" values > 90 -- proving the gs4_cols()-based range generalizes
# instead of being pinned to the original row count
write_pretty_sheet(
  ss, data = theme_demo[1:3, ], sheet = "pretty_roundtrip",
  theme = attr(captured, "prettysheets_theme")
)

# Equivalent one-liner, since write_pretty_sheet() auto-detects the attached
# theme when `theme` is omitted
write_pretty_sheet(ss, data = read_pretty_sheet(ss, sheet = "pretty_custom"), sheet = "pretty_roundtrip_2")


# ── 20. Cleanup ──────────────────────────────────────────────────────────────
# Once you've confirmed everything above looks right, trash the test sheet.
# drive_trash(ss)
