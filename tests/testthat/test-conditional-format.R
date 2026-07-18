test_that("cf_cell_value maps shorthand operators to API condition types", {
  expect_equal(cf_cell_value(">", 100)$type, "NUMBER_GREATER")
  expect_equal(cf_cell_value("<=", 5)$type, "NUMBER_LESS_THAN_EQ")
  expect_equal(cf_cell_value("==", 1)$type, "NUMBER_EQ")
})

test_that("cf_cell_value passes raw ConditionType strings through unchanged", {
  expect_equal(cf_cell_value("TEXT_STARTS_WITH", "abc")$type, "TEXT_STARTS_WITH")
})

test_that("cf_cell_value requires exactly 2 values for 'between'", {
  expect_error(cf_cell_value("between", 5), "length 2")
  out <- cf_cell_value("between", c(1, 10))
  expect_length(out$values, 2)
})

test_that("cf_text_contains and cf_custom_formula build expected shapes", {
  expect_equal(cf_text_contains("abc"), list(
    type = "TEXT_CONTAINS", values = list(list(userEnteredValue = "abc"))
  ))
  expect_equal(cf_custom_formula("=A1>1")$type, "CUSTOM_FORMULA")
})

test_that("cf_blank / cf_not_blank need no values", {
  expect_equal(cf_blank(), list(type = "BLANK"))
  expect_equal(cf_not_blank(), list(type = "NOT_BLANK"))
})

test_that("cf_format only exposes the 5 API-supported conditional-format properties", {
  built <- cf_format(bold = TRUE, background_color = "red")
  expect_true(built$textFormat$bold)
  expect_equal(built$backgroundColor, list(red = 1, green = 0, blue = 0))
  # cf_format() has no number_format/alignment args at all -- confirm by formals
  expect_false("number_format" %in% names(formals(cf_format)))
  expect_false("horizontal_alignment" %in% names(formals(cf_format)))
})

test_that("cf_gradient drops NULL points and keeps supplied ones", {
  g <- cf_gradient(min_color = "white", max_color = "forestgreen")
  expect_named(g, c("minpoint", "maxpoint"))
  expect_equal(g$minpoint$type, "MIN")
  expect_equal(g$maxpoint$type, "MAX")
})

test_that("cf_gradient supports an explicit midpoint", {
  g <- cf_gradient(
    min_color = "red", mid_color = "yellow", max_color = "green",
    mid_type = "PERCENTILE", mid_value = 50
  )
  expect_named(g, c("minpoint", "midpoint", "maxpoint"))
  expect_equal(g$midpoint$value, "50")
})
