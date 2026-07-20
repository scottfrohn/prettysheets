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
  # Regression test: apply_requests() used to call request_make() and discard
  # the result without ever checking the response -- a rejected request (e.g.
  # a malformed body) would fail completely silently, no R error at all. It
  # must now pipe the response through gargle::response_process(), whose job
  # is exactly to raise on a bad status code.
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
