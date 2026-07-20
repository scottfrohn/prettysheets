test_that("build_cell_format only sets fields that were actually supplied", {
  built <- build_cell_format(bold = TRUE)
  expect_equal(built$fields, "userEnteredFormat.textFormat.bold")
  expect_equal(built$cell_format$textFormat$bold, TRUE)
  expect_null(built$cell_format$backgroundColor)
})

test_that("a second call for a different property doesn't reintroduce the first", {
  first <- build_cell_format(bold = TRUE)
  second <- build_cell_format(background_color = "red")

  expect_equal(first$fields, "userEnteredFormat.textFormat.bold")
  expect_equal(second$fields, "userEnteredFormat.backgroundColor")
  expect_null(second$cell_format$textFormat)
})

test_that("multiple properties in one call all get masked", {
  built <- build_cell_format(bold = TRUE, background_color = "#FFFFFF", horizontal_alignment = "CENTER")
  expect_setequal(
    built$fields,
    c(
      "userEnteredFormat.textFormat.bold",
      "userEnteredFormat.backgroundColor",
      "userEnteredFormat.horizontalAlignment"
    )
  )
})

test_that("number_format bundles type + pattern under one field", {
  built <- build_cell_format(number_format = "0.0%", number_format_type = "PERCENT")
  expect_equal(built$cell_format$numberFormat, list(type = "PERCENT", pattern = "0.0%"))
  expect_equal(built$fields, "userEnteredFormat.numberFormat")
})

test_that("build_cell_format with no arguments produces no fields", {
  built <- build_cell_format()
  expect_length(built$fields, 0)
})

test_that("link sets textFormat.link.uri and its own field mask entry", {
  built <- build_cell_format(link = "https://example.com")
  expect_equal(built$cell_format$textFormat$link, list(uri = "https://example.com"))
  expect_equal(built$fields, "userEnteredFormat.textFormat.link")
})

test_that("link composes with other textFormat properties without clobbering them", {
  built <- build_cell_format(bold = TRUE, link = "https://example.com")
  expect_true(built$cell_format$textFormat$bold)
  expect_equal(built$cell_format$textFormat$link, list(uri = "https://example.com"))
  expect_setequal(
    built$fields,
    c("userEnteredFormat.textFormat.bold", "userEnteredFormat.textFormat.link")
  )
})

test_that("clear_format_request builds a RepeatCellRequest with an empty userEnteredFormat", {
  grid_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 1L)
  req <- clear_format_request(grid_range)
  expect_equal(req$repeatCell$range, grid_range)
  expect_equal(req$repeatCell$fields, "userEnteredFormat")
  expect_named(req$repeatCell$cell, "userEnteredFormat")
  expect_length(req$repeatCell$cell$userEnteredFormat, 0)
})

test_that("empty_json_object serializes to '{}', not '[]' -- this is what makes clear_format_request() actually clear formatting instead of silently no-op'ing", {
  skip_if_not_installed("jsonlite")
  expect_equal(as.character(jsonlite::toJSON(empty_json_object(), auto_unbox = TRUE)), "{}")
  # the bug this guards against: a bare list() has no names attribute at
  # all, so jsonlite treats it as an unnamed list (a JSON array) instead
  expect_equal(as.character(jsonlite::toJSON(list(), auto_unbox = TRUE)), "[]")
})

test_that("clear_format_request's cell serializes with userEnteredFormat as an object", {
  skip_if_not_installed("jsonlite")
  grid_range <- list(sheetId = 0)
  json <- jsonlite::toJSON(clear_format_request(grid_range)$repeatCell$cell, auto_unbox = TRUE)
  expect_equal(as.character(json), '{"userEnteredFormat":{}}')
})

test_that("resolve_border_sides passes literal side names through", {
  expect_equal(resolve_border_sides("top"), "top")
  expect_setequal(resolve_border_sides(c("top", "left")), c("top", "left"))
})

test_that("resolve_border_sides expands 'outside', 'inside', and 'all'", {
  expect_setequal(resolve_border_sides("outside"), c("top", "bottom", "left", "right"))
  expect_setequal(resolve_border_sides("inside"), c("innerHorizontal", "innerVertical"))
  expect_setequal(
    resolve_border_sides("all"),
    c("top", "bottom", "left", "right", "innerHorizontal", "innerVertical")
  )
})

test_that("resolve_border_sides de-duplicates overlapping shorthand", {
  expect_length(resolve_border_sides(c("outside", "top")), 4)
  expect_length(resolve_border_sides(c("outside", "inside")), 6)
})

test_that("resolve_border_sides rejects unknown side names", {
  expect_error(resolve_border_sides("diagonal"), "Unknown")
  expect_error(resolve_border_sides(c("top", "sideways")), "sideways")
})
