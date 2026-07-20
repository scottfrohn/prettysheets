test_that("col_index_to_letter handles single letters", {
  expect_equal(col_index_to_letter(1), "A")
  expect_equal(col_index_to_letter(26), "Z")
})

test_that("col_index_to_letter handles the rollover into double letters", {
  expect_equal(col_index_to_letter(27), "AA")
  expect_equal(col_index_to_letter(52), "AZ")
  expect_equal(col_index_to_letter(53), "BA")
  expect_equal(col_index_to_letter(702), "ZZ")
  expect_equal(col_index_to_letter(703), "AAA")
})

test_that("col_index_to_letter is vectorized", {
  expect_equal(col_index_to_letter(1:3), c("A", "B", "C"))
})

test_that("gs4_glimpse_cols requires a data frame", {
  expect_error(gs4_glimpse_cols(list(a = 1)), "data frame")
  expect_error(gs4_glimpse_cols("nope"), "data frame")
})

test_that("gs4_glimpse_cols rejects a data frame with no columns", {
  expect_error(gs4_glimpse_cols(data.frame()), "no columns")
})

test_that("gs4_glimpse_cols returns a letter/name mapping invisibly", {
  df <- data.frame(item = 1, item_id = 2, ctt_difficulty = 3)
  capture.output(out <- gs4_glimpse_cols(df))
  expect_equal(out$letter, c("A", "B", "C"))
  expect_equal(out$name, c("item", "item_id", "ctt_difficulty"))
})

test_that("gs4_glimpse_cols is actually invisible (doesn't auto-print its return value)", {
  df <- data.frame(x = 1, y = 2)
  capture.output(result <- withVisible(gs4_glimpse_cols(df)))
  expect_false(result$visible)
})

test_that("print_glimpse_cols prints letters above names, aligned", {
  cols <- data.frame(letter = c("A", "B"), name = c("item", "item_id"), stringsAsFactors = FALSE)
  out <- capture.output(print_glimpse_cols(cols, width = 80))
  expect_length(out, 2)
  expect_match(out[1], "^A\\s+B")
  expect_match(out[2], "^item\\s+item_id")
})

test_that("print_glimpse_cols wraps into multiple blocks when columns don't fit the width", {
  cols <- data.frame(
    letter = c("A", "B", "C"),
    name = c("a_fairly_long_column_name", "another_long_one", "z"),
    stringsAsFactors = FALSE
  )
  out <- capture.output(print_glimpse_cols(cols, width = 20))
  # 3 columns don't fit on one line at width 20, so there should be more
  # than one 2-line block, separated by a blank line
  expect_true(any(out == ""))
  expect_true(length(out) > 2)
})

test_that("print_glimpse_cols puts everything on one block when it all fits", {
  cols <- data.frame(letter = c("A", "B"), name = c("x", "y"), stringsAsFactors = FALSE)
  out <- capture.output(print_glimpse_cols(cols, width = 80))
  expect_false(any(out == ""))
  expect_length(out, 2)
})
