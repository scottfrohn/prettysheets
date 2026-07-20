test_that("ensure_sheet_exists() adds the sheet when its name isn't already present", {
  added <- NULL
  local_mocked_bindings(
    sheet_properties = function(ss) data.frame(name = c("Sheet1", "other")),
    sheet_add = function(ss, sheet) {
      added <<- sheet
      invisible(ss)
    },
    .package = "googlesheets4"
  )

  ensure_sheet_exists("fake", "combined")
  expect_equal(added, "combined")
})

test_that("ensure_sheet_exists() does nothing when the sheet already exists", {
  local_mocked_bindings(
    sheet_properties = function(ss) data.frame(name = c("Sheet1", "combined")),
    sheet_add = function(ss, sheet) cli::cli_abort("sheet_add() should not have been called"),
    .package = "googlesheets4"
  )

  expect_no_error(ensure_sheet_exists("fake", "combined"))
})

test_that("ensure_sheet_exists() does nothing for sheet = NULL or a numeric position", {
  local_mocked_bindings(
    sheet_properties = function(ss) cli::cli_abort("sheet_properties() should not have been called"),
    sheet_add = function(ss, sheet) cli::cli_abort("sheet_add() should not have been called"),
    .package = "googlesheets4"
  )

  expect_no_error(ensure_sheet_exists("fake", NULL))
  expect_no_error(ensure_sheet_exists("fake", 1))
})

test_that("ensure_sheet_exists() sizes a newly created sheet to fit data (plus a header row)", {
  resized <- NULL
  local_mocked_bindings(
    sheet_properties = function(ss) data.frame(name = "Sheet1"),
    sheet_add = function(ss, sheet) invisible(ss),
    sheet_resize = function(ss, sheet, nrow, ncol, exact) {
      resized <<- list(sheet = sheet, nrow = nrow, ncol = ncol, exact = exact)
      invisible(ss)
    },
    .package = "googlesheets4"
  )

  ensure_sheet_exists("fake", "combined", data = data.frame(x = 1:3, y = 1:3, z = 1:3))

  expect_equal(resized$sheet, "combined")
  expect_equal(resized$nrow, 4) # 3 data rows + 1 header row
  expect_equal(resized$ncol, 3)
  expect_true(resized$exact)
})

test_that("ensure_sheet_exists() doesn't add a header row to the size when col_names = FALSE", {
  resized <- NULL
  local_mocked_bindings(
    sheet_properties = function(ss) data.frame(name = "Sheet1"),
    sheet_add = function(ss, sheet) invisible(ss),
    sheet_resize = function(ss, sheet, nrow, ncol, exact) {
      resized <<- list(nrow = nrow, ncol = ncol)
      invisible(ss)
    },
    .package = "googlesheets4"
  )

  ensure_sheet_exists("fake", "combined", data = data.frame(x = 1:3), col_names = FALSE)
  expect_equal(resized$nrow, 3)
  expect_equal(resized$ncol, 1)
})

test_that("ensure_sheet_exists() doesn't resize when data is NULL (the default)", {
  local_mocked_bindings(
    sheet_properties = function(ss) data.frame(name = "Sheet1"),
    sheet_add = function(ss, sheet) invisible(ss),
    sheet_resize = function(...) cli::cli_abort("sheet_resize() should not have been called"),
    .package = "googlesheets4"
  )

  expect_no_error(ensure_sheet_exists("fake", "combined"))
})

test_that("ensure_sheet_exists() doesn't resize an already-existing sheet, even if data is supplied", {
  local_mocked_bindings(
    sheet_properties = function(ss) data.frame(name = "combined"),
    sheet_add = function(ss, sheet) cli::cli_abort("sheet_add() should not have been called"),
    sheet_resize = function(...) cli::cli_abort("sheet_resize() should not have been called"),
    .package = "googlesheets4"
  )

  expect_no_error(ensure_sheet_exists("fake", "combined", data = data.frame(x = 1:3)))
})

test_that("ensure_sheet_exists() skips resizing when there's nothing meaningful to size to", {
  local_mocked_bindings(
    sheet_properties = function(ss) data.frame(name = "Sheet1"),
    sheet_add = function(ss, sheet) invisible(ss),
    sheet_resize = function(...) cli::cli_abort("sheet_resize() should not have been called"),
    .package = "googlesheets4"
  )

  # zero data rows and no header row requested -> nrow would be 0
  expect_no_error(ensure_sheet_exists("fake", "combined", data = data.frame(x = integer(0)), col_names = FALSE))
  # zero columns entirely, with or without a header row
  expect_no_error(ensure_sheet_exists("fake", "combined", data = data.frame(), col_names = FALSE))
  expect_no_error(ensure_sheet_exists("fake", "combined", data = data.frame()))
})
