test_that("gs_theme() fills in header defaults (bold header, no colors)", {
  theme <- gs_theme()
  expect_s3_class(theme, "prettysheets_theme")
  expect_true(theme$header$bold)
  expect_null(theme$header$background_color)
  expect_null(theme$header$font_color)
  expect_null(theme$banding)
  expect_null(theme$border)
  expect_equal(theme$columns, list())
  expect_false(theme$wrap_long_text)
  expect_true(theme$freeze_header)
  expect_equal(theme$conditional_formats, list())
})

test_that("gs_theme() merges partial header input over the defaults", {
  theme <- gs_theme(header = list(background_color = "#1F4E79"))
  expect_true(theme$header$bold) # untouched default
  expect_equal(theme$header$background_color, "#1F4E79")
})

test_that("gs_theme() rejects unknown header fields", {
  expect_error(gs_theme(header = list(bg = "red")), "header")
})

test_that("gs_theme() requires both band1 and band2 when banding is supplied", {
  expect_error(gs_theme(banding = list(band1 = "white")), "band1.*band2|band2.*band1")
  theme <- gs_theme(banding = list(band1 = "white", band2 = "gray"))
  expect_equal(theme$banding, list(band1 = "white", band2 = "gray"))
})

test_that("gs_theme() rejects unknown banding fields", {
  expect_error(gs_theme(banding = list(band1 = "white", band2 = "gray", band3 = "blue")), "banding")
})

test_that("gs_theme() requires a color when border is supplied, and fills in sides/style defaults", {
  expect_error(gs_theme(border = list(sides = "all")), "color")
  theme <- gs_theme(border = list(color = "gray40"))
  expect_equal(theme$border, list(sides = "outside", color = "gray40", style = "SOLID"))
})

test_that("gs_theme() requires columns to be a named list", {
  expect_error(gs_theme(columns = list(1, 2)), "columns")
  theme <- gs_theme(columns = list(price = list(number_format = "$#,##0.00")))
  expect_equal(theme$columns$price$number_format, "$#,##0.00")
})

test_that("gs_theme() requires conditional_formats to be a list", {
  expect_error(gs_theme(conditional_formats = "nope"), "conditional_formats")
})

test_that("modify_theme() requires a prettysheets_theme", {
  expect_error(modify_theme(list(), header = list(bold = FALSE)), "prettysheets_theme")
})

test_that("modify_theme() merges header/border into the existing theme instead of replacing it", {
  theme <- gs_theme(header = list(background_color = "#1F4E79", font_color = "white"))
  out <- modify_theme(theme, header = list(font_color = "black"))
  expect_equal(out$header$background_color, "#1F4E79") # untouched
  expect_equal(out$header$font_color, "black") # changed
  expect_true(out$header$bold) # untouched default
})

test_that("modify_theme() merges banding, adding it fresh if the theme had none", {
  theme <- gs_theme()
  out <- modify_theme(theme, banding = list(band1 = "white", band2 = "lightblue"))
  expect_equal(out$banding, list(band1 = "white", band2 = "lightblue"))

  out2 <- modify_theme(out, banding = list(band2 = "lightyellow"))
  expect_equal(out2$banding, list(band1 = "white", band2 = "lightyellow"))
})

test_that("modify_theme() merges columns, adding/overriding one entry without disturbing others", {
  theme <- gs_theme(columns = list(price = list(number_format = "$#,##0.00")))
  out <- modify_theme(theme, columns = list(qty = list(bold = TRUE)))
  expect_equal(out$columns$price$number_format, "$#,##0.00")
  expect_equal(out$columns$qty$bold, TRUE)
})

test_that("modify_theme() replaces wrap_long_text/freeze_header/conditional_formats outright when supplied", {
  theme <- gs_theme(wrap_long_text = FALSE, freeze_header = TRUE)
  out <- modify_theme(theme, wrap_long_text = TRUE, freeze_header = FALSE)
  expect_true(out$wrap_long_text)
  expect_false(out$freeze_header)
})

test_that("modify_theme() leaves fields untouched when not supplied", {
  theme <- gs_theme(header = list(background_color = "red"), wrap_long_text = TRUE)
  out <- modify_theme(theme, freeze_header = FALSE)
  expect_equal(out$header$background_color, "red")
  expect_true(out$wrap_long_text)
  expect_false(out$freeze_header)
})

test_that("gs_theme_preset() builds the same shape gs_theme_clean() applies", {
  theme <- gs_theme_preset("clean")
  expect_s3_class(theme, "prettysheets_theme")
  expect_equal(theme$header$font_color, "black")
  expect_false(is.null(theme$banding))
})

test_that("print.prettysheets_theme() runs without error and returns its input invisibly", {
  theme <- gs_theme()
  expect_output(print(theme), "prettysheets_theme")
  capture.output(result <- print(theme))
  expect_identical(result, theme)
})

test_that("write_pretty_sheet() accepts a preset name string in place of a theme object", {
  captured <- NULL
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    range_write = function(ss, data, sheet, range, col_names, reformat) invisible(ss),
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    apply_gs_theme = function(ss, sheet, data, theme) {
      captured <<- theme
      invisible(ss)
    },
    ensure_sheet_exists = function(ss, sheet, data = NULL, col_names = TRUE) invisible(ss)
  )

  write_pretty_sheet("fake", data = data.frame(x = 1), theme = "professional")
  expect_s3_class(captured, "prettysheets_theme")
  expect_equal(captured$header$font_color, "white")
})

test_that("write_pretty_sheet() reuses a theme attached to data when theme isn't supplied", {
  captured <- NULL
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    range_write = function(ss, data, sheet, range, col_names, reformat) invisible(ss),
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    apply_gs_theme = function(ss, sheet, data, theme) {
      captured <<- theme
      invisible(ss)
    },
    ensure_sheet_exists = function(ss, sheet, data = NULL, col_names = TRUE) invisible(ss)
  )

  attached_theme <- gs_theme(header = list(background_color = "#123456"))
  data <- data.frame(x = 1)
  attr(data, "prettysheets_theme") <- attached_theme

  write_pretty_sheet("fake", data = data)
  expect_identical(captured, attached_theme)
})

test_that("write_pretty_sheet() falls back to a plain default theme when nothing else is available", {
  captured <- NULL
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    range_write = function(ss, data, sheet, range, col_names, reformat) invisible(ss),
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    apply_gs_theme = function(ss, sheet, data, theme) {
      captured <<- theme
      invisible(ss)
    },
    ensure_sheet_exists = function(ss, sheet, data = NULL, col_names = TRUE) invisible(ss)
  )

  write_pretty_sheet("fake", data = data.frame(x = 1))
  expect_s3_class(captured, "prettysheets_theme")
  expect_equal(captured$header$background_color, "#F3F3F3")
})
