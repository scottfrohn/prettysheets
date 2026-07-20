test_that("color_to_hex converts full-strength and NULL-channel colors correctly", {
  expect_equal(color_to_hex(list(red = 1, green = 0, blue = 0)), "#FF0000")
  expect_equal(color_to_hex(list(red = 0, green = 0, blue = 0)), "#000000")
  expect_equal(color_to_hex(list()), "#000000") # all channels missing = 0
  expect_null(color_to_hex(NULL))
})

test_that("color_to_hex round-trips through gs4_color", {
  original <- "#1F4E79"
  expect_equal(toupper(color_to_hex(gs4_color(original))), toupper(original))
})

test_that("extract_cell_format_args returns an empty list for NULL or an empty format", {
  expect_equal(extract_cell_format_args(NULL), list())
  expect_equal(extract_cell_format_args(list()), list())
})

test_that("extract_cell_format_args pulls out only the fields actually present", {
  fmt <- list(
    textFormat = list(bold = TRUE, foregroundColor = list(red = 1, green = 1, blue = 1)),
    backgroundColor = list(red = 0, green = 0, blue = 0),
    horizontalAlignment = "CENTER"
  )
  args <- extract_cell_format_args(fmt)
  expect_equal(args$bold, TRUE)
  expect_equal(args$font_color, "#FFFFFF")
  expect_equal(args$background_color, "#000000")
  expect_equal(args$horizontal_alignment, "CENTER")
  expect_null(args$number_format)
  expect_null(args$italic)
})

test_that("extract_cell_format_args includes number_format_type only when a pattern is present", {
  with_pattern <- extract_cell_format_args(list(numberFormat = list(type = "CURRENCY", pattern = "$#,##0.00")))
  expect_equal(with_pattern$number_format, "$#,##0.00")
  expect_equal(with_pattern$number_format_type, "CURRENCY")

  no_pattern <- extract_cell_format_args(list(numberFormat = list(type = "CURRENCY")))
  expect_null(no_pattern$number_format)
  expect_null(no_pattern$number_format_type)
})

test_that("matched_column_names maps a GridRange's column bounds to header names", {
  headers <- c("a", "b", "c", "d")
  expect_equal(matched_column_names(list(startColumnIndex = 0L, endColumnIndex = 2L), headers), c("a", "b"))
  expect_equal(matched_column_names(list(startColumnIndex = 2L, endColumnIndex = 3L), headers), "c")
  expect_equal(matched_column_names(list(), headers), headers) # unbounded = whole table
})

test_that("matched_column_names clamps out-of-range bounds to the table's actual width", {
  headers <- c("a", "b")
  expect_equal(matched_column_names(list(startColumnIndex = 0L, endColumnIndex = 100L), headers), c("a", "b"))
  expect_equal(matched_column_names(list(startColumnIndex = 5L, endColumnIndex = 10L), headers), character(0))
})

test_that("translate_banding returns NULL when nothing overlaps the table", {
  table_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
  banded <- list(list(range = list(startRowIndex = 20L, endRowIndex = 25L, startColumnIndex = 0L, endColumnIndex = 3L)))
  expect_null(translate_banding(banded, table_range))
  expect_null(translate_banding(list(), table_range))
  expect_null(translate_banding(NULL, table_range))
})

test_that("translate_banding extracts band1/band2/header_color from an overlapping banded range", {
  table_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
  banded <- list(list(
    range = list(startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L),
    rowProperties = list(
      headerColor = list(red = 0, green = 0, blue = 0),
      firstBandColor = list(red = 1, green = 1, blue = 1),
      secondBandColor = list(red = 0, green = 0, blue = 1)
    )
  ))
  out <- translate_banding(banded, table_range)
  expect_equal(out$band1, "#FFFFFF")
  expect_equal(out$band2, "#0000FF")
  expect_equal(out$header_color, "#000000")
})

test_that("translate_conditional_formats skips rules that don't overlap the table", {
  table_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
  rules <- list(list(
    ranges = list(list(startRowIndex = 50L, endRowIndex = 55L, startColumnIndex = 0L, endColumnIndex = 3L)),
    booleanRule = list(condition = list(type = "NUMBER_GREATER"), format = list())
  ))
  expect_equal(translate_conditional_formats(rules, table_range, c("a", "b", "c")), list())
})

test_that("translate_conditional_formats translates an overlapping boolean rule into a gs4_cols() spec", {
  table_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
  condition <- list(type = "NUMBER_GREATER", values = list(list(userEnteredValue = "100")))
  format <- list(backgroundColor = list(red = 1, green = 0, blue = 0))
  rules <- list(list(
    ranges = list(list(startRowIndex = 1L, endRowIndex = 10L, startColumnIndex = 2L, endColumnIndex = 3L)),
    booleanRule = list(condition = condition, format = format)
  ))
  out <- translate_conditional_formats(rules, table_range, c("a", "b", "c"))
  expect_length(out, 1)
  expect_s3_class(out[[1]]$range, "prettysheets_cols")
  expect_equal(out[[1]]$range$names, "c")
  expect_equal(out[[1]]$rule, condition)
  expect_equal(out[[1]]$format, format)
  expect_null(out[[1]]$gradient)
})

test_that("translate_conditional_formats translates an overlapping gradient rule", {
  table_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
  gradient <- list(minpoint = list(color = list(red = 1, green = 1, blue = 1), type = "MIN"))
  rules <- list(list(
    ranges = list(list(startRowIndex = 1L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)),
    gradientRule = gradient
  ))
  out <- translate_conditional_formats(rules, table_range, c("a", "b", "c"))
  expect_length(out, 1)
  expect_equal(out[[1]]$gradient, gradient)
  expect_null(out[[1]]$rule)
})

test_that("translate_conditional_formats returns an empty list for no rules", {
  table_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
  expect_equal(translate_conditional_formats(list(), table_range, c("a")), list())
  expect_equal(translate_conditional_formats(NULL, table_range, c("a")), list())
})

test_that("fetch_sheet_meta returns empty defaults when no sheet in the response matches sheet_id", {
  local_mocked_bindings(
    request_generate = function(endpoint, params) list(),
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    response_process = function(resp) list(sheets = list(list(properties = list(sheetId = 1)))),
    .package = "gargle"
  )

  out <- fetch_sheet_meta("fake", sheet_id = 999)
  expect_equal(out$bandedRanges, list())
  expect_equal(out$conditionalFormats, list())
})

test_that("fetch_sheet_meta returns the matching sheet's own entry", {
  local_mocked_bindings(
    request_generate = function(endpoint, params) list(),
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    response_process = function(resp) {
      list(sheets = list(
        list(properties = list(sheetId = 0), bandedRanges = list("sheet0-banding")),
        list(properties = list(sheetId = 5), bandedRanges = list("sheet5-banding"))
      ))
    },
    .package = "gargle"
  )

  out <- fetch_sheet_meta("fake", sheet_id = 5)
  expect_equal(out$bandedRanges, list("sheet5-banding"))
})
