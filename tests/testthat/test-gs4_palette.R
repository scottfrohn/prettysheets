test_that("gs4_palette has the expected shape", {
  expect_s3_class(gs4_palette, "data.frame")
  expect_equal(nrow(gs4_palette), 80)
  expect_named(gs4_palette, c("row", "col", "name", "hex"))
  expect_true(all(grepl("^#[0-9a-f]{6}$", gs4_palette$hex)))
  expect_equal(anyDuplicated(gs4_palette$name), 0)
})

test_that("gs4_palette includes known reference colors", {
  expect_equal(
    gs4_palette$hex[gs4_palette$name == "black"],
    "#000000"
  )
  expect_equal(
    gs4_palette$hex[gs4_palette$name == "cornflower blue"],
    "#4a86e8"
  )
  expect_equal(
    gs4_palette$hex[gs4_palette$name == "light cornflower blue 1"],
    "#6d9eeb"
  )
  expect_equal(
    gs4_palette$hex[gs4_palette$name == "dark magenta 3"],
    "#4c1130"
  )
})

test_that("gs4_palette_color looks up hex codes", {
  expect_equal(gs4_palette_color("cornflower blue"), "#4a86e8")
  expect_equal(gs4_palette_color("Dark Red 2"), "#990000")
  expect_equal(gs4_palette_color("  white  "), "#ffffff")
})

test_that("gs4_palette_color errors on unknown names", {
  expect_error(gs4_palette_color("not a color"), class = "rlang_error")
})

test_that("gs4_palette_color treats 'grey' and 'gray' as the same word", {
  expect_equal(gs4_palette_color("grey"), gs4_palette_color("gray"))
  expect_equal(gs4_palette_color("dark grey 1"), gs4_palette_color("dark gray 1"))
  expect_equal(gs4_palette_color("light grey 2"), gs4_palette_color("light gray 2"))
  expect_equal(gs4_palette_color("Dark Grey 1"), gs4_palette_color("dark gray 1")) # case-insensitive too
})

test_that("gs4_color accepts 'grey'-spelled gs4_palette names via the col2rgb() fallback", {
  expect_equal(gs4_color("dark grey 1"), gs4_color("dark gray 1"))
})

test_that("gs4_palette_color output feeds into gs4_color", {
  expect_equal(
    gs4_color(gs4_palette_color("red")),
    gs4_color("#ff0000")
  )
})
