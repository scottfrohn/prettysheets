test_that("apply_requests() sends a single named request wrapped in a length-1 array", {
  captured <- NULL
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    request_generate = function(endpoint, params) {
      captured <<- params
      list(endpoint = endpoint, params = params)
    },
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(response_process = function(resp) resp, .package = "gargle")

  apply_requests("fake", list(repeatCell = list(range = list(sheetId = 0))))

  expect_length(captured$requests, 1)
  expect_named(captured$requests[[1]], "repeatCell")
})

test_that("apply_requests() passes a list of requests straight through", {
  captured <- NULL
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    request_generate = function(endpoint, params) {
      captured <<- params
      list(endpoint = endpoint, params = params)
    },
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(response_process = function(resp) resp, .package = "gargle")

  apply_requests("fake", list(
    list(repeatCell = list(range = list(sheetId = 0))),
    list(repeatCell = list(range = list(sheetId = 1)))
  ))

  expect_length(captured$requests, 2)
})

test_that("apply_requests() surfaces an API error instead of failing silently", {
  # request_make() returns the raw httr response without checking its status,
  # so apply_requests() must pipe it through gargle::response_process() --
  # whose job is exactly to raise on a bad status code -- rather than
  # discarding it and letting a rejected request fail silently.
  local_mocked_bindings(as_sheets_id = function(x) x, .package = "googlesheets4")
  local_mocked_bindings(
    request_generate = function(endpoint, params) list(endpoint = endpoint, params = params),
    request_make = function(req) "raw-response",
    .package = "googlesheets4"
  )
  local_mocked_bindings(
    response_process = function(resp) cli::cli_abort("API request failed"),
    .package = "gargle"
  )

  expect_error(
    apply_requests("fake", list(repeatCell = list(range = list(sheetId = 0)))),
    "API request failed"
  )
})

local_mock_batch_api <- function(env = parent.frame()) {
  calls <- new.env()
  calls$sheet_properties <- 0L
  calls$batch_updates <- list()
  local_mocked_bindings(
    as_sheets_id = function(x) x,
    sheet_properties = function(ss) {
      calls$sheet_properties <- calls$sheet_properties + 1L
      data.frame(name = c("Sheet1", "Sheet2"), index = 0:1, id = c(0, 99), visible = TRUE)
    },
    request_generate = function(endpoint, params) list(endpoint = endpoint, params = params),
    request_make = function(req) {
      calls$batch_updates[[length(calls$batch_updates) + 1]] <- req$params$requests
      "raw-response"
    },
    .package = "googlesheets4",
    .env = env
  )
  local_mocked_bindings(response_process = function(resp) resp, .package = "gargle", .env = env)
  calls
}

test_that("prettysheets_batch() looks up the sheet list once and sends one batchUpdate", {
  calls <- local_mock_batch_api()
  prettysheets_batch({
    range_format("fake", sheet = "Sheet1", range = "A1:B1", bold = TRUE)
    range_merge("fake", sheet = "Sheet2", range = "A2:B2")
    range_checkbox("fake", sheet = "Sheet1", range = "C2:C10")
  })
  expect_equal(calls$sheet_properties, 1L)
  expect_length(calls$batch_updates, 1)
  expect_length(calls$batch_updates[[1]], 3)
  expect_equal(calls$batch_updates[[1]][[2]]$mergeCells$range$sheetId, 99)
})

test_that("outside a batch, each call looks up the sheet list itself (nothing is cached)", {
  calls <- local_mock_batch_api()
  range_merge("fake", range = "A2:B2")
  range_merge("fake", range = "A3:B3")
  expect_equal(calls$sheet_properties, 2L)
  expect_length(calls$batch_updates, 2)
})

test_that("nested prettysheets_batch() calls add to the outer batch", {
  calls <- local_mock_batch_api()
  prettysheets_batch({
    range_merge("fake", range = "A2:B2")
    prettysheets_batch({
      range_merge("fake", range = "A3:B3")
    })
    range_merge("fake", range = "A4:B4")
  })
  expect_length(calls$batch_updates, 1)
  expect_length(calls$batch_updates[[1]], 3)
})

test_that("functions that make several requests batch themselves", {
  calls <- local_mock_batch_api()
  sheet_format_header("fake", sheet = "Sheet1")
  expect_length(calls$batch_updates, 1)
  expect_named(calls$batch_updates[[1]][[1]], "repeatCell")
  expect_named(calls$batch_updates[[1]][[2]], "updateSheetProperties")
  expect_equal(calls$sheet_properties, 1L)
})

test_that("the sheet-list cache is cleared when a batch ends, even on error", {
  calls <- local_mock_batch_api()
  expect_error(prettysheets_batch({
    range_merge("fake", range = "A2:B2")
    stop("boom")
  }), "boom")
  expect_false(.prettysheets_env$collecting)
  expect_length(.prettysheets_env$sheet_props, 0)

  range_merge("fake", range = "A2:B2")
  expect_equal(calls$sheet_properties, 2L)
})

test_that("forget_sheet_properties() makes the next lookup in a batch re-read the sheet list", {
  calls <- local_mock_batch_api()
  prettysheets_batch({
    range_merge("fake", range = "A2:B2")
    forget_sheet_properties("fake")
    range_merge("fake", range = "A3:B3")
  })
  expect_equal(calls$sheet_properties, 2L)
})

test_that("a gs_theme_*() call sends all its formatting in one batchUpdate with one sheet lookup", {
  calls <- new.env()
  calls$sheet_properties <- 0L
  calls$gets <- 0L
  calls$batch_updates <- list()
  local_mocked_bindings(
    as_sheets_id = function(x) x,
    sheet_properties = function(ss) {
      calls$sheet_properties <- calls$sheet_properties + 1L
      data.frame(name = "Sheet1", index = 0L, id = 0, visible = TRUE)
    },
    request_generate = function(endpoint, params) list(endpoint = endpoint, params = params),
    request_make = function(req) {
      if (req$endpoint == "sheets.spreadsheets.batchUpdate") {
        calls$batch_updates[[length(calls$batch_updates) + 1]] <- req$params$requests
      } else {
        calls$gets <- calls$gets + 1L
      }
      req
    },
    .package = "googlesheets4"
  )
  local_mocked_bindings(response_process = function(resp) list(), .package = "gargle")

  df <- data.frame(a = 1:3, b = c("x", "y", "z"), c = c(1.5, 2.5, 3.5), d = 4:6, e = letters[1:3])
  gs_theme_clean("fake", data = df)

  expect_length(calls$batch_updates, 1)
  expect_gt(length(calls$batch_updates[[1]]), ncol(df))
  expect_equal(calls$sheet_properties, 1L)
  expect_equal(calls$gets, 2L) # existing banding + conditional formats, to clear them
})
