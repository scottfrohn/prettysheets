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

test_that("range_add_conditional_format() builds one rule covering all of resolve_grid_ranges()'s ranges", {
  # resolve_grid_ranges() (plural) is what understands gs4_cols() -- a
  # single selection can expand into more than one GridRange (one per
  # contiguous run of matched columns), and a ConditionalFormatRule's own
  # `ranges` accepts a list, so this confirms they all land in ONE rule/one
  # request instead of erroring or splitting into separate rules.
  requests <- list()
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    resolve_grid_ranges = function(ss, sheet, range) {
      list(list(sheetId = 0, startColumnIndex = 0L, endColumnIndex = 1L),
           list(sheetId = 0, startColumnIndex = 3L, endColumnIndex = 4L))
    },
    send_or_queue = function(ss, request) {
      requests[[length(requests) + 1]] <<- request
      invisible(ss)
    }
  )

  range_add_conditional_format(
    "fake", range = gs4_cols("mpg", "hp"),
    rule = cf_cell_value(">", 100), format = cf_format(bold = TRUE)
  )

  expect_length(requests, 1)
  rule <- requests[[1]]$addConditionalFormatRule$rule
  expect_length(rule$ranges, 2)
  expect_equal(rule$booleanRule$condition, cf_cell_value(">", 100))
  expect_equal(rule$booleanRule$format, cf_format(bold = TRUE))
})

test_that("range_add_conditional_format() works with a plain A1 range too (a single resolved GridRange)", {
  requests <- list()
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    resolve_grid_ranges = function(ss, sheet, range) list(list(sheetId = 0, startRowIndex = 1L, endRowIndex = 10L)),
    send_or_queue = function(ss, request) {
      requests[[length(requests) + 1]] <<- request
      invisible(ss)
    }
  )

  range_add_conditional_format(
    "fake", range = "A2:A10",
    rule = cf_cell_value(">", 100), format = cf_format(bold = TRUE)
  )

  expect_length(requests[[1]]$addConditionalFormatRule$rule$ranges, 1)
})

test_that("range_add_gradient_format() also resolves a gs4_cols() range into one rule", {
  requests <- list()
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    resolve_grid_ranges = function(ss, sheet, range) {
      list(list(sheetId = 0, startColumnIndex = 0L, endColumnIndex = 1L),
           list(sheetId = 0, startColumnIndex = 3L, endColumnIndex = 4L))
    },
    send_or_queue = function(ss, request) {
      requests[[length(requests) + 1]] <<- request
      invisible(ss)
    }
  )

  gradient <- cf_gradient(min_color = "white", max_color = "forestgreen")
  range_add_gradient_format("fake", range = gs4_cols("mpg", "hp"), gradient = gradient)

  expect_length(requests, 1)
  rule <- requests[[1]]$addConditionalFormatRule$rule
  expect_length(rule$ranges, 2)
  expect_equal(rule$gradientRule, gradient)
})
