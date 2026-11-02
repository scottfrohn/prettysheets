local_mock_cols_api <- function(header = c("queue", "item_id", "p_wt", "rpb_wt"), env = parent.frame()) {
  calls <- new.env()
  calls$header_reads <- 0L
  calls$requests <- list()
  local_mocked_bindings(
    as_sheets_id = function(x) x,
    sheet_properties = function(ss) data.frame(name = "Sheet1", index = 0L, id = 0, visible = TRUE),
    range_read = function(ss, sheet, range, col_names, col_types) {
      calls$header_reads <- calls$header_reads + 1L
      as.data.frame(as.list(stats::setNames(header, paste0("...", seq_along(header)))))
    },
    request_generate = function(endpoint, params) list(endpoint = endpoint, params = params),
    request_make = function(req) {
      calls$requests <- c(calls$requests, req$params$requests)
      req
    },
    .package = "googlesheets4",
    .env = env
  )
  local_mocked_bindings(response_process = function(resp) list(), .package = "gargle", .env = env)
  calls
}

test_that("cols targets the data rows of the named columns, with no range needed", {
  calls <- local_mock_cols_api()
  range_format("fake", cols = c("p_wt", "rpb_wt"), horizontal_alignment = "CENTER")
  expect_equal(
    calls$requests[[1]]$repeatCell$range,
    list(sheetId = 0, startRowIndex = 1L, startColumnIndex = 2L, endColumnIndex = 4L)
  )
})

test_that("cols works for every range function", {
  calls <- local_mock_cols_api()
  range_checkbox("fake", cols = "queue")
  range_remove_checkbox("fake", cols = "queue")
  range_merge("fake", cols = "item_id")
  range_unmerge("fake", cols = "item_id")
  range_format_border("fake", cols = "p_wt")
  range_add_conditional_format("fake", cols = "p_wt",
                               rule = cf_cell_value("not between", c(0.2, 0.9)),
                               format = cf_format(background_color = "#990000"))
  range_add_gradient_format("fake", cols = "rpb_wt", gradient = cf_gradient("white", max_color = "green"))
  sheet_clear_format("fake", cols = "p_wt")

  first_range <- function(r) {
    body <- r[[1]]
    body$range %||% body$rule$ranges[[1]]
  }
  cols_hit <- vapply(calls$requests, function(r) first_range(r)$startColumnIndex, numeric(1))
  expect_equal(cols_hit, c(0, 0, 1, 1, 2, 2, 3, 2))
})

test_that("non-adjacent cols become one request per run of columns", {
  calls <- local_mock_cols_api()
  range_checkbox("fake", cols = c("queue", "p_wt"))
  expect_length(calls$requests, 2)
  expect_equal(calls$requests[[2]]$setDataValidation$range$startColumnIndex, 2L)
})

test_that("supplying both range and cols is an error", {
  local_mock_cols_api()
  expect_error(range_format("fake", range = "A1", cols = "p_wt", bold = TRUE), "not both")
})

test_that("functions with no sensible whole-sheet default need range or cols", {
  local_mock_cols_api()
  expect_error(range_checkbox("fake"), "range.*cols")
  expect_error(range_merge("fake"), "range.*cols")
  expect_error(range_add_conditional_format("fake", rule = cf_blank(), format = cf_format(bold = TRUE)), "range.*cols")
})

test_that("an unknown column name is reported", {
  local_mock_cols_api()
  expect_error(range_format("fake", cols = "nope", bold = TRUE), "nope")
})

test_that("inside a batch the header row is read once", {
  calls <- local_mock_cols_api()
  prettysheets_batch({
    range_format("fake", cols = "p_wt", number_format = "0.00")
    range_format("fake", cols = "rpb_wt", number_format = "0.00")
    range_checkbox("fake", cols = "queue")
  })
  expect_equal(calls$header_reads, 1L)
  expect_length(calls$requests, 3)
})

test_that("theme conditional_formats specs accept cols", {
  calls <- local_mock_cols_api(header = c("a", "p_wt"))
  df <- data.frame(a = 1:3, p_wt = c(0.1, 0.5, 0.95))
  theme <- gs_theme(conditional_formats = list(
    list(cols = "p_wt", rule = cf_cell_value("not between", c(0.2, 0.9)),
         format = cf_format(background_color = "#990000"))
  ))
  local_mocked_bindings(
    range_write = function(...) invisible(NULL),
    sheet_resize = function(...) invisible(NULL),
    .package = "googlesheets4"
  )
  write_pretty_sheet("fake", data = df, sheet = "Sheet1", theme = theme)
  cf <- Filter(function(r) !is.null(r$addConditionalFormatRule), calls$requests)
  expect_length(cf, 1)
  expect_equal(cf[[1]]$addConditionalFormatRule$rule$ranges[[1]]$startColumnIndex, 1L)
})
