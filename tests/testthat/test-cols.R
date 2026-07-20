test_that("gs4_cols builds a prettysheets_cols object", {
  out <- gs4_cols("mpg", "hp")
  expect_s3_class(out, "prettysheets_cols")
  expect_equal(out$names, c("mpg", "hp"))
  expect_null(out$data)
  expect_equal(out$header_row, 1L)
})

test_that("gs4_cols respects header_row and data", {
  out <- gs4_cols("mpg", data = mtcars, header_row = 3)
  expect_equal(out$header_row, 3L)
  expect_identical(out$data, mtcars)
})

test_that("gs4_cols requires at least one character name", {
  expect_error(gs4_cols(), "column name")
  expect_error(gs4_cols(1), "column name")
})

test_that("resolve_cols_header uses names(data) for a data frame", {
  cols <- gs4_cols("mpg", data = mtcars)
  expect_equal(resolve_cols_header(ss = NULL, sheet = NULL, cols), names(mtcars))
})

test_that("resolve_cols_header uses a character vector as-is", {
  cols <- gs4_cols("mpg", data = c("mpg", "cyl", "hp"))
  expect_equal(resolve_cols_header(ss = NULL, sheet = NULL, cols), c("mpg", "cyl", "hp"))
})

test_that("cols_to_cell_limits keeps contiguous columns in one range", {
  cols <- gs4_cols("mpg", "cyl")
  limits <- cols_to_cell_limits(cols, header = names(mtcars))
  expect_length(limits, 1)
  expect_equal(limits[[1]]$ul, c(2, 1))
  expect_equal(limits[[1]]$lr, c(NA, 2))
})

test_that("cols_to_cell_limits returns an unnamed list, even for a single contiguous run", {
  # Regression test: split() (used internally to group matched column
  # positions into contiguous runs) names each group by its run id ("1",
  # "2", ...), and that name used to survive all the way out through
  # lapply() into cols_to_cell_limits()'s/resolve_grid_ranges()'s return
  # value. Harmless for a caller that uses each element on its own
  # (range_format()), but silently broke any caller that bundles every
  # element into one field (range_add_conditional_format()'s
  # `ranges = grid_ranges`) -- a *named* list serializes to a JSON object
  # instead of a JSON array, so the request landed malformed with no error.
  cols <- gs4_cols("mpg")
  limits <- cols_to_cell_limits(cols, header = names(mtcars))
  expect_null(names(limits))
})

test_that("cols_to_cell_limits splits non-adjacent columns into separate ranges", {
  cols <- gs4_cols("mpg", "hp") # mpg = col 1, hp = col 4 in mtcars, cyl/disp between
  limits <- cols_to_cell_limits(cols, header = names(mtcars))
  expect_length(limits, 2)

  bounds <- lapply(limits, function(l) c(l$ul[2], l$lr[2]))
  # ul[2]/lr[2] come from match()/min()/max() on integer positions, so these
  # are integer vectors -- identical() distinguishes integer from double, so
  # the comparison targets need to be integer literals (1L, not 1) too.
  expect_true(any(vapply(bounds, function(b) identical(b, c(1L, 1L)), logical(1))))
  expect_true(any(vapply(bounds, function(b) identical(b, c(4L, 4L)), logical(1))))
})

test_that("cols_to_cell_limits respects a custom header_row", {
  cols <- gs4_cols("mpg", header_row = 3)
  limits <- cols_to_cell_limits(cols, header = names(mtcars))
  expect_equal(limits[[1]]$ul, c(4, 1))
})

test_that("cols_to_cell_limits errors on names not in the header", {
  cols <- gs4_cols("not_a_column")
  expect_error(cols_to_cell_limits(cols, header = names(mtcars)), "not_a_column")
})

test_that("cols_to_cell_limits defaults to excluding the header row", {
  cols <- gs4_cols("mpg")
  limits <- cols_to_cell_limits(cols, header = names(mtcars))
  expect_equal(limits[[1]]$ul[1], 2) # one row below header_row = 1
})

test_that("cols_to_cell_limits includes the header row when include_header = TRUE", {
  cols <- gs4_cols("mpg")
  limits <- cols_to_cell_limits(cols, header = names(mtcars), include_header = TRUE)
  expect_equal(limits[[1]]$ul[1], 1) # header_row itself, not header_row + 1
})

test_that("cols_to_cell_limits respects include_header alongside a custom header_row", {
  cols <- gs4_cols("mpg", header_row = 3)
  limits <- cols_to_cell_limits(cols, header = names(mtcars), include_header = TRUE)
  expect_equal(limits[[1]]$ul[1], 3)
})

test_that("resolve_grid_ranges passes non-cols ranges straight through as a length-1 list", {
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range, include_header = TRUE) {
      list(sheetId = 0, range = range, include_header = include_header)
    }
  )
  out <- resolve_grid_ranges(ss = NULL, sheet = NULL, range = "A1:B2")
  expect_length(out, 1)
  expect_equal(out[[1]]$range, "A1:B2")
})

test_that("resolve_grid_ranges defaults include_header to TRUE for non-cols ranges when not supplied", {
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range, include_header = TRUE) include_header
  )
  expect_true(resolve_grid_ranges(ss = NULL, sheet = NULL, range = "B:B")[[1]])
})

test_that("resolve_grid_ranges passes an explicit include_header through for non-cols ranges", {
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range, include_header = TRUE) include_header
  )
  expect_false(resolve_grid_ranges(ss = NULL, sheet = NULL, range = "B:B", include_header = FALSE)[[1]])
})

test_that("resolve_grid_ranges expands a prettysheets_cols range into one GridRange per contiguous run", {
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range) list(sheetId = 0, limits = range),
    resolve_cols_header = function(ss, sheet, cols) names(mtcars)
  )
  out <- resolve_grid_ranges(ss = NULL, sheet = NULL, range = gs4_cols("mpg", "hp"))
  expect_length(out, 2)
})

test_that("resolve_grid_ranges passes include_header through to cols_to_cell_limits", {
  local_mocked_bindings(
    resolve_grid_range = function(ss, sheet, range) list(sheetId = 0, limits = range),
    resolve_cols_header = function(ss, sheet, cols) names(mtcars)
  )
  out <- resolve_grid_ranges(
    ss = NULL, sheet = NULL, range = gs4_cols("mpg"), include_header = TRUE
  )
  expect_equal(out[[1]]$limits$ul[1], 1)
})

test_that("resolve_grid_ranges resolves a live gs4_cols() selection to the correct numeric GridRange end-to-end", {
  # Unlike the tests above (which mock resolve_grid_range()/resolve_cols_header()
  # themselves), this one only mocks the two genuine API boundaries --
  # sheet_properties() and range_read() -- so it exercises the real
  # resolve_cols_header() -> cols_to_cell_limits() -> resolve_grid_range()
  # chain, the same path range_add_conditional_format() uses for something
  # like gs4_cols("score") against a 5-column sheet ("id", "score", "status",
  # "notes", "homepage" -- score is column 2/B, 5 data rows below the header).
  local_mocked_bindings(
    sheet_properties = function(ss) {
      data.frame(id = 7, name = "pretty_custom", index = 0, visible = TRUE)
    },
    range_read = function(ss, sheet, range, col_names, col_types) {
      data.frame(
        ...1 = "id", ...2 = "score", ...3 = "status",
        ...4 = "notes", ...5 = "homepage",
        stringsAsFactors = FALSE
      )
    },
    .package = "googlesheets4"
  )

  out <- resolve_grid_ranges(ss = NULL, sheet = "pretty_custom", range = gs4_cols("score"))

  expect_null(names(out)) # must be unnamed -- see the split()/unname() regression test above
  expect_length(out, 1)
  gr <- out[[1]]
  expect_equal(gr$sheetId, 7)
  expect_equal(gr$startColumnIndex, 1L) # column B (0-based)
  expect_equal(gr$endColumnIndex, 2L) # exclusive upper bound -- column B only
  expect_equal(gr$startRowIndex, 1L) # row 2 (0-based) -- one below the header
  expect_null(gr$endRowIndex) # open-ended -- "to the end of the sheet"
})
