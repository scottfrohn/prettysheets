test_that("parse_range_shorthand passes cell_limits objects through unchanged", {
  limits <- cellranger::cell_limits(ul = c(1, 1), lr = c(2, 2))
  expect_identical(parse_range_shorthand(limits), limits)
})

test_that("parse_range_shorthand parses ordinary A1 strings via cellranger", {
  limits <- parse_range_shorthand("A1:B2")
  expect_equal(limits$ul, c(1, 1))
  expect_equal(limits$lr, c(2, 2))
})

test_that("parse_range_shorthand understands whole-column shorthand like 'B:B'", {
  limits <- parse_range_shorthand("B:B")
  expect_true(is.na(limits$ul[1]))
  expect_true(is.na(limits$lr[1]))
  expect_equal(limits$ul[2], 2) # column B
  expect_equal(limits$lr[2], 2)
})

test_that("parse_range_shorthand understands a whole-column range like 'B:D'", {
  limits <- parse_range_shorthand("B:D")
  expect_equal(limits$ul[2], 2) # B
  expect_equal(limits$lr[2], 4) # D
  expect_true(is.na(limits$ul[1]))
  expect_true(is.na(limits$lr[1]))
})

test_that("parse_range_shorthand understands whole-row shorthand like '2:5'", {
  limits <- parse_range_shorthand("2:5")
  expect_equal(limits$ul[1], 2)
  expect_equal(limits$lr[1], 5)
  expect_true(is.na(limits$ul[2]))
  expect_true(is.na(limits$lr[2]))
})

test_that("parse_range_shorthand errors the same way as before on nonsense strings", {
  # cellranger's own guess_fo() emits a "NAs generated." warning en route to
  # the error for input this malformed -- harmless, but silenced so it
  # doesn't get flagged as an unhandled warning alongside the expected error.
  expect_error(suppressWarnings(parse_range_shorthand("disp")))
  expect_error(suppressWarnings(parse_range_shorthand("not a range")))
})

# resolve_grid_range() itself calls googlesheets4::sheet_properties(), which
# needs a live sheet -- mock it so the include_header behavior can be
# checked without a real spreadsheet. `.env` defaults to the *caller's*
# frame (not this function's own), so the mock's cleanup is deferred to the
# end of whichever test_that() block calls this helper, rather than firing
# immediately when this helper itself returns.
local_fake_sheet_properties <- function(.env = parent.frame()) {
  local_mocked_bindings(
    sheet_properties = function(ss) {
      data.frame(id = 0, name = "Sheet1", index = 0, visible = TRUE)
    },
    .package = "googlesheets4",
    .env = .env
  )
}

test_that("resolve_grid_range keeps the literal whole column by default (include_header = TRUE)", {
  local_fake_sheet_properties()
  gr <- resolve_grid_range(ss = NULL, sheet = NULL, range = "B:B")
  expect_null(gr$startRowIndex)
  expect_null(gr$endRowIndex)
})

test_that("resolve_grid_range skips row 1 for a whole-column range when include_header = FALSE", {
  local_fake_sheet_properties()
  gr <- resolve_grid_range(ss = NULL, sheet = NULL, range = "B:B", include_header = FALSE)
  expect_equal(gr$startRowIndex, 1) # zero-based row index for row 2
  expect_null(gr$endRowIndex) # still open-ended at the bottom
})

test_that("include_header = FALSE doesn't touch a range that already names explicit rows", {
  local_fake_sheet_properties()
  gr <- resolve_grid_range(ss = NULL, sheet = NULL, range = "B2:B33", include_header = FALSE)
  expect_equal(gr$startRowIndex, 1) # B2, unaffected by include_header
  expect_equal(gr$endRowIndex, 33)
})

test_that("include_header = FALSE doesn't touch whole-row shorthand (no header row to exclude)", {
  local_fake_sheet_properties()
  gr <- resolve_grid_range(ss = NULL, sheet = NULL, range = "2:5", include_header = FALSE)
  expect_equal(gr$startRowIndex, 1)
  expect_equal(gr$endRowIndex, 5)
  expect_null(gr$startColumnIndex)
  expect_null(gr$endColumnIndex)
})
