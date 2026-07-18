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
