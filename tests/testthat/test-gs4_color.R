test_that("gs4_color converts hex strings", {
  out <- gs4_color("#FFFFFF")
  expect_equal(out, list(red = 1, green = 1, blue = 1))

  out2 <- gs4_color("#000000")
  expect_equal(out2, list(red = 0, green = 0, blue = 0))
})

test_that("gs4_color converts named R colors", {
  out <- gs4_color("red")
  expect_equal(out$red, 1)
  expect_equal(out$green, 0)
  expect_equal(out$blue, 0)
})

test_that("gs4_color passes through an already-built Color list", {
  built <- list(red = 0.1, green = 0.2, blue = 0.3)
  expect_identical(gs4_color(built), built)
})

test_that("gs4_color returns NULL for NULL input", {
  expect_null(gs4_color(NULL))
})

test_that("gs4_color respects the alpha argument", {
  out <- gs4_color("black", alpha = 0.5)
  expect_equal(out$alpha, 0.5)
})

test_that("gs4_color falls back to gs4_palette names col2rgb() doesn't know", {
  out <- gs4_color("light green 1")
  expect_equal(out, gs4_color(gs4_palette_color("light green 1")))

  out2 <- gs4_color("dark yellow 1")
  expect_equal(out2, gs4_color(gs4_palette_color("dark yellow 1")))
})

test_that("gs4_color's palette fallback is case/whitespace-insensitive, like gs4_palette_color()", {
  expect_equal(gs4_color("Light Green 1"), gs4_color("light green 1"))
  expect_equal(gs4_color("  dark yellow 1  "), gs4_color("dark yellow 1"))
})

test_that("gs4_color prefers col2rgb() over the palette when both would match", {
  # "red" is both a base R color and (as a substring) close to gs4_palette
  # names -- col2rgb() should win, so this is exactly base R red
  out <- gs4_color("red")
  expect_equal(out, list(red = 1, green = 0, blue = 0))
})

test_that("gs4_color still errors on a genuinely unknown color", {
  expect_error(gs4_color("not a real color"), "Unknown color")
})
