test_that("range_write_format() creates the target sheet first, sized to the data, when it doesn't already exist", {
  captured <- NULL
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    range_write = function(ss, data, sheet, range, col_names, reformat) invisible(ss),
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    ensure_sheet_exists = function(ss, sheet, data = NULL, col_names = TRUE) {
      captured <<- list(sheet = sheet, data = data, col_names = col_names)
      invisible(ss)
    },
    sheet_format_header = function(...) invisible(NULL)
  )

  df <- data.frame(x = 1:3)
  range_write_format("fake", data = df, sheet = "combined")
  expect_equal(captured$sheet, "combined")
  expect_identical(captured$data, df)
  expect_true(captured$col_names)
})

test_that("write_pretty_sheet() creates the target sheet first, sized to the data, when it doesn't already exist", {
  captured <- NULL
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    range_write = function(ss, data, sheet, range, col_names, reformat) invisible(ss),
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    ensure_sheet_exists = function(ss, sheet, data = NULL, col_names = TRUE) {
      captured <<- list(sheet = sheet, data = data, col_names = col_names)
      invisible(ss)
    },
    apply_gs_theme = function(...) invisible(NULL)
  )

  df <- data.frame(x = 1:3)
  write_pretty_sheet("fake", data = df, sheet = "combined")
  expect_equal(captured$sheet, "combined")
  expect_identical(captured$data, df)
  expect_true(captured$col_names)
})
