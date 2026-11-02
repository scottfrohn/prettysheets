test_that("range_write_format() creates the target sheet first, sized to the data, when it doesn't already exist", {
  captured <- NULL
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    range_write = function(ss, data, sheet, range, col_names, reformat) invisible(ss),
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    clear_sheet_validation_request = function(ss, sheet) list(),
    apply_requests = function(ss, requests) invisible(ss),
    ensure_sheet_exists = function(ss, sheet, data = NULL, col_names = TRUE, ...) {
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
    clear_sheet_validation_request = function(ss, sheet) list(),
    apply_requests = function(ss, requests) invisible(ss),
    ensure_sheet_exists = function(ss, sheet, data = NULL, col_names = TRUE, ...) {
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

test_that("write_pretty_sheet() with no range fits the sheet to the data and clears old validation", {
  fit <- NULL
  requests <- list()
  local_mocked_bindings(
    as_sheets_id = function(x) x,
    range_write = function(...) invisible(NULL),
    sheet_properties = function(ss) data.frame(name = "items", index = 0L, id = 7, visible = TRUE),
    request_generate = function(endpoint, params) list(endpoint = endpoint, params = params),
    request_make = function(req) {
      requests <<- c(requests, req$params$requests)
      req
    },
    .package = "googlesheets4"
  )
  local_mocked_bindings(response_process = function(resp) list(), .package = "gargle")
  local_mocked_bindings(
    ensure_sheet_exists = function(ss, sheet, data = NULL, col_names = TRUE, fit_existing = FALSE) {
      fit <<- fit_existing
      invisible(ss)
    }
  )

  write_pretty_sheet("fake", data = data.frame(a = 1:2), sheet = "items")
  expect_true(fit)
  expect_equal(requests[[1]], list(setDataValidation = list(range = list(sheetId = 7))))

  requests <- list()
  write_pretty_sheet("fake", data = data.frame(a = 1:2), sheet = "items", range = "A5")
  expect_false(fit)
  expect_false(any(vapply(requests, function(r) !is.null(r$setDataValidation), logical(1))))
})
