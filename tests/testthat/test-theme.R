test_that("classify_column detects whole-number columns as integer, regardless of R class", {
  expect_equal(classify_column(1:5), "integer")
  expect_equal(classify_column(c(4, 6, 8, 4, 6)), "integer") # doubles that happen to be whole numbers
  expect_equal(classify_column(c(4, 6, NA, 8)), "integer") # NAs don't spoil it
})

test_that("classify_column detects non-whole numbers as double", {
  expect_equal(classify_column(c(1.5, 2.25, 3)), "double")
  expect_equal(classify_column(mtcars$mpg), "double")
})

test_that("classify_column detects a column of URLs", {
  expect_equal(
    classify_column(c("https://example.com", "http://foo.org/bar", "www.baz.com")),
    "url"
  )
})

test_that("classify_column requires ALL non-NA values to look like URLs", {
  expect_equal(classify_column(c("https://example.com", "not a url")), "character")
})

test_that("classify_column detects long text (> 50 characters)", {
  long <- strrep("x", 51)
  expect_equal(classify_column(c("short", long)), "long_text")
})

test_that("classify_column treats short text as plain character", {
  expect_equal(classify_column(c("a", "bb", "ccc")), "character")
})

test_that("classify_column ignores NA/empty strings when classifying character columns", {
  expect_equal(classify_column(c(NA, "", "https://example.com")), "url")
})

test_that("classify_column treats an all-NA or all-empty character column as character", {
  expect_equal(classify_column(c(NA_character_, NA_character_)), "character")
  expect_equal(classify_column(c("", "")), "character")
})

test_that("classify_column falls back to character for factors, logicals, and other types", {
  expect_equal(classify_column(factor(c("a", "b"))), "character")
  expect_equal(classify_column(c(TRUE, FALSE, NA)), "character")
  expect_equal(classify_column(as.Date(c("2024-01-01", "2024-06-01"))), "character")
})

test_that("theme_column_format gives numbers a comma+decimal pattern and centers them", {
  int_fmt <- theme_column_format("integer")
  expect_equal(int_fmt$number_format, "#,##0")
  expect_equal(int_fmt$horizontal_alignment, "CENTER")

  dbl_fmt <- theme_column_format("double")
  expect_equal(dbl_fmt$number_format, "#,##0.00")
  expect_equal(dbl_fmt$horizontal_alignment, "CENTER")
})

test_that("theme_column_format left-aligns text types with type-appropriate wrap", {
  expect_equal(theme_column_format("url")$wrap_strategy, "CLIP")
  expect_equal(theme_column_format("url")$horizontal_alignment, "LEFT")
  expect_equal(theme_column_format("character")$horizontal_alignment, "LEFT")
  expect_null(theme_column_format("character")$wrap_strategy)
})

test_that("theme_column_format clips long_text by default, and only wraps when asked", {
  expect_equal(theme_column_format("long_text")$wrap_strategy, "CLIP")
  expect_equal(theme_column_format("long_text", wrap_long_text = FALSE)$wrap_strategy, "CLIP")
  expect_equal(theme_column_format("long_text", wrap_long_text = TRUE)$wrap_strategy, "WRAP")
})

test_that("theme_column_format falls back to left-aligned for unrecognized types", {
  expect_equal(theme_column_format("something_else"), list(horizontal_alignment = "LEFT"))
})

test_that("theme_fixed_width sets a width for url/long_text, NULL otherwise", {
  expect_equal(theme_fixed_width("url"), 100L)
  expect_equal(theme_fixed_width("long_text"), 340L)
  expect_null(theme_fixed_width("integer"))
  expect_null(theme_fixed_width("double"))
  expect_null(theme_fixed_width("character"))
})

test_that("set_column_width_request builds the expected updateDimensionProperties shape", {
  req <- set_column_width_request(sheet_id = 42, col_index = 3, pixel_size = 300)
  dims <- req$updateDimensionProperties
  expect_equal(dims$range$sheetId, 42)
  expect_equal(dims$range$dimension, "COLUMNS")
  expect_equal(dims$range$startIndex, 2L) # 0-based
  expect_equal(dims$range$endIndex, 3L)
  expect_equal(dims$properties$pixelSize, 300L)
  expect_equal(dims$fields, "pixelSize")
})

test_that("url_hyperlink_request builds one request with one CellData row per value", {
  grid_range <- list(sheetId = 0, startRowIndex = 1L, endRowIndex = 3L, startColumnIndex = 1L, endColumnIndex = 2L)
  req <- url_hyperlink_request(grid_range, c("https://a.com", "https://b.com"))

  expect_named(req, "updateCells")
  expect_equal(req$updateCells$range, grid_range)
  expect_equal(req$updateCells$fields, "userEnteredFormat.textFormat.link")
  expect_length(req$updateCells$rows, 2)
  expect_equal(
    req$updateCells$rows[[1]]$values[[1]]$userEnteredFormat$textFormat$link,
    list(uri = "https://a.com")
  )
  expect_equal(
    req$updateCells$rows[[2]]$values[[1]]$userEnteredFormat$textFormat$link,
    list(uri = "https://b.com")
  )
})

test_that("url_hyperlink_request leaves NA/blank values unlinked with an empty CellData", {
  grid_range <- list(sheetId = 0, startRowIndex = 1L, endRowIndex = 4L, startColumnIndex = 0L, endColumnIndex = 1L)
  req <- url_hyperlink_request(grid_range, c("https://a.com", NA, ""))

  expect_length(req$updateCells$rows[[2]]$values[[1]], 0)
  expect_length(req$updateCells$rows[[3]]$values[[1]], 0)
  expect_equal(
    req$updateCells$rows[[1]]$values[[1]]$userEnteredFormat$textFormat$link,
    list(uri = "https://a.com")
  )
})

test_that("url_hyperlink_request's empty CellData serializes to '{}', matching clear_format_request()'s convention", {
  skip_if_not_installed("jsonlite")
  grid_range <- list(sheetId = 0, startRowIndex = 1L, endRowIndex = 2L, startColumnIndex = 0L, endColumnIndex = 1L)
  req <- url_hyperlink_request(grid_range, NA_character_)
  json <- jsonlite::toJSON(req$updateCells$rows[[1]]$values[[1]], auto_unbox = TRUE)
  expect_equal(as.character(json), "{}")
})

test_that("theme_display_strings formats integer/double columns the way theme_column_format() will display them", {
  expect_equal(theme_display_strings(c(1000, 2500000), "integer"), c("1,000", "2,500,000"))
  expect_equal(theme_display_strings(c(1.5, 1234.5), "double"), c("1.50", "1,234.50"))
})

test_that("theme_display_strings passes character values through unchanged and drops NAs", {
  expect_equal(theme_display_strings(c("a", NA, "bb"), "character"), c("a", "bb"))
})

test_that("theme_display_strings returns character(0) for an all-NA column", {
  expect_equal(theme_display_strings(c(NA, NA), "character"), character(0))
})

test_that("theme_column_width sizes from the widest of header or data, plus padding", {
  narrow <- theme_column_width("id", c("1", "2", "3"))
  wide <- theme_column_width("description", c("a fairly long piece of descriptive text"))
  expect_gt(wide, narrow)
})

test_that("theme_column_width never goes below min_width, even for very short content", {
  expect_equal(theme_column_width("x", c("1")), 100L)
  expect_equal(theme_column_width("", character(0)), 100L)
})

test_that("theme_column_width respects a custom min_width", {
  expect_equal(theme_column_width("x", c("1"), min_width = 80L), 80L)
})
