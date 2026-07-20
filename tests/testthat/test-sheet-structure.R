test_that("grid_ranges_overlap detects overlapping bounded ranges", {
  a <- list(startRowIndex = 0L, endRowIndex = 5L, startColumnIndex = 0L, endColumnIndex = 3L)
  b <- list(startRowIndex = 2L, endRowIndex = 8L, startColumnIndex = 1L, endColumnIndex = 4L)
  expect_true(grid_ranges_overlap(a, b))
})

test_that("grid_ranges_overlap treats adjacent (touching, not overlapping) ranges as non-overlapping", {
  a <- list(startRowIndex = 0L, endRowIndex = 5L, startColumnIndex = 0L, endColumnIndex = 3L)
  b <- list(startRowIndex = 5L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
  expect_false(grid_ranges_overlap(a, b))
})

test_that("grid_ranges_overlap requires overlap in both rows AND columns", {
  a <- list(startRowIndex = 0L, endRowIndex = 5L, startColumnIndex = 0L, endColumnIndex = 3L)
  same_rows_different_cols <- list(startRowIndex = 0L, endRowIndex = 5L, startColumnIndex = 10L, endColumnIndex = 15L)
  expect_false(grid_ranges_overlap(a, same_rows_different_cols))
})

test_that("grid_ranges_overlap treats missing bounds as unbounded (whole sheet)", {
  whole_sheet <- list()
  a_cell <- list(startRowIndex = 100L, endRowIndex = 101L, startColumnIndex = 5L, endColumnIndex = 6L)
  expect_true(grid_ranges_overlap(whole_sheet, a_cell))
  expect_true(grid_ranges_overlap(a_cell, whole_sheet))
})

test_that("find_banded_range_ids returns bandedRangeIds overlapping the target range, on the target sheet only", {
  local_mocked_bindings(
    request_generate = function(endpoint, params) list(endpoint = endpoint, params = params),
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    response_process = function(resp) {
      list(
        sheets = list(
          list(
            properties = list(sheetId = 0),
            bandedRanges = list(
              list(
                bandedRangeId = 111,
                range = list(sheetId = 0, startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
              ),
              list(
                bandedRangeId = 222,
                range = list(sheetId = 0, startRowIndex = 20L, endRowIndex = 25L, startColumnIndex = 0L, endColumnIndex = 3L)
              )
            )
          ),
          list(properties = list(sheetId = 1), bandedRanges = list())
        )
      )
    },
    .package = "gargle"
  )

  grid_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 5L, startColumnIndex = 0L, endColumnIndex = 3L)
  expect_equal(find_banded_range_ids(ss = "fake", grid_range = grid_range), 111)
})

test_that("find_banded_range_ids returns empty when the sheet has no banding, or no sheet matches", {
  local_mocked_bindings(
    request_generate = function(endpoint, params) list(),
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    response_process = function(resp) {
      list(sheets = list(list(properties = list(sheetId = 0), bandedRanges = NULL)))
    },
    .package = "gargle"
  )

  expect_equal(
    find_banded_range_ids("fake", list(sheetId = 0, startRowIndex = 0L, endRowIndex = 5L)),
    integer(0)
  )
  expect_equal(
    find_banded_range_ids("fake", list(sheetId = 999)),
    integer(0)
  )
})

test_that("find_conditional_format_indices returns overlapping rule indices, descending, on the target sheet only", {
  local_mocked_bindings(
    request_generate = function(endpoint, params) list(endpoint = endpoint, params = params),
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    response_process = function(resp) {
      list(
        sheets = list(
          list(
            properties = list(sheetId = 0),
            conditionalFormats = list(
              # index 0: overlaps
              list(ranges = list(list(startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L))),
              # index 1: does not overlap
              list(ranges = list(list(startRowIndex = 50L, endRowIndex = 55L, startColumnIndex = 0L, endColumnIndex = 3L))),
              # index 2: overlaps via its SECOND range
              list(ranges = list(
                list(startRowIndex = 50L, endRowIndex = 55L, startColumnIndex = 0L, endColumnIndex = 3L),
                list(startRowIndex = 0L, endRowIndex = 10L, startColumnIndex = 0L, endColumnIndex = 3L)
              ))
            )
          ),
          list(properties = list(sheetId = 1), conditionalFormats = list(list(ranges = list(list()))))
        )
      )
    },
    .package = "gargle"
  )

  grid_range <- list(sheetId = 0, startRowIndex = 0L, endRowIndex = 5L, startColumnIndex = 0L, endColumnIndex = 3L)
  expect_equal(find_conditional_format_indices(ss = "fake", grid_range = grid_range), c(2, 0))
})

test_that("find_conditional_format_indices returns empty when the sheet has no rules, or no sheet matches", {
  local_mocked_bindings(
    request_generate = function(endpoint, params) list(),
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    response_process = function(resp) {
      list(sheets = list(list(properties = list(sheetId = 0), conditionalFormats = NULL)))
    },
    .package = "gargle"
  )

  expect_equal(
    find_conditional_format_indices("fake", list(sheetId = 0, startRowIndex = 0L, endRowIndex = 5L)),
    integer(0)
  )
  expect_equal(
    find_conditional_format_indices("fake", list(sheetId = 999)),
    integer(0)
  )
})

test_that("sheet_clear_format() queues a deleteBanding request for each overlapping banded range", {
  requests <- list()
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range) list(sheetId = 7, startRowIndex = 0L, endRowIndex = 5L),
    find_banded_range_ids = function(ss, grid_range) c(111, 222),
    find_conditional_format_indices = function(ss, grid_range) integer(0),
    send_or_queue = function(ss, request) {
      requests[[length(requests) + 1]] <<- request
      invisible(ss)
    }
  )

  sheet_clear_format("fake", sheet = "Sheet1")

  expect_length(requests, 3) # 1 clear_format_request + 2 deleteBanding
  expect_named(requests[[1]], "repeatCell")
  expect_equal(requests[[2]]$deleteBanding$bandedRangeId, 111)
  expect_equal(requests[[3]]$deleteBanding$bandedRangeId, 222)
})

test_that("sheet_clear_format() queues a deleteConditionalFormatRule request for each overlapping rule, highest index first", {
  requests <- list()
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range) list(sheetId = 7, startRowIndex = 0L, endRowIndex = 5L),
    find_banded_range_ids = function(ss, grid_range) integer(0),
    find_conditional_format_indices = function(ss, grid_range) c(2, 0),
    send_or_queue = function(ss, request) {
      requests[[length(requests) + 1]] <<- request
      invisible(ss)
    }
  )

  sheet_clear_format("fake", sheet = "Sheet1")

  expect_length(requests, 3) # 1 clear_format_request + 2 deleteConditionalFormatRule
  expect_named(requests[[1]], "repeatCell")
  expect_equal(requests[[2]]$deleteConditionalFormatRule, list(sheetId = 7, index = 2L))
  expect_equal(requests[[3]]$deleteConditionalFormatRule, list(sheetId = 7, index = 0L))
})

test_that("sheet_clear_format() sends only the clear request when there's no existing banding or conditional formats", {
  requests <- list()
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range) list(sheetId = 7),
    find_banded_range_ids = function(ss, grid_range) integer(0),
    find_conditional_format_indices = function(ss, grid_range) integer(0),
    send_or_queue = function(ss, request) {
      requests[[length(requests) + 1]] <<- request
      invisible(ss)
    }
  )

  sheet_clear_format("fake")

  expect_length(requests, 1)
  expect_named(requests[[1]], "repeatCell")
})

test_that("sheet_format_tabcolor() sets tabColor when a color is supplied", {
  requests <- list()
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range) list(sheetId = 7),
    send_or_queue = function(ss, request) {
      requests[[length(requests) + 1]] <<- request
      invisible(ss)
    }
  )

  sheet_format_tabcolor("fake", color = "forestgreen")

  props <- requests[[1]]$updateSheetProperties$properties
  expect_equal(props$sheetId, 7)
  expect_equal(props$tabColor, gs4_color("forestgreen"))
  expect_equal(requests[[1]]$updateSheetProperties$fields, "tabColor")
})

test_that("sheet_format_tabcolor(color = NULL) clears the tab color by omitting tabColor, not setting it to NULL", {
  requests <- list()
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range) list(sheetId = 7),
    send_or_queue = function(ss, request) {
      requests[[length(requests) + 1]] <<- request
      invisible(ss)
    }
  )

  sheet_format_tabcolor("fake") # color defaults to NULL

  props <- requests[[1]]$updateSheetProperties$properties
  expect_equal(props, list(sheetId = 7)) # tabColor key genuinely absent
  expect_false("tabColor" %in% names(props))
  expect_equal(requests[[1]]$updateSheetProperties$fields, "tabColor")
})
