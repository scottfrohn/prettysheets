#' Convert a 1-based column position to its spreadsheet column letter(s)
#'
#' The same base-26 sequence Google Sheets/Excel use for column headers:
#' `1` -> `"A"`, ..., `26` -> `"Z"`, `27` -> `"AA"`, `52` -> `"AZ"`, and so on.
#'
#' @param n A positive integer vector of 1-based column positions.
#' @return A character vector of column letters, the same length as `n`.
#' @keywords internal
#' @noRd
col_index_to_letter <- function(n) {
  vapply(n, function(x) {
    s <- ""
    while (x > 0) {
      rem <- (x - 1) %% 26
      s <- paste0(LETTERS[rem + 1], s)
      x <- (x - 1) %/% 26
    }
    s
  }, character(1))
}

#' Print a `letter`/`name` mapping in aligned, "glimpse"-style blocks,
#' wrapping to fit the console width
#' @keywords internal
#' @noRd
print_glimpse_cols <- function(cols, width = getOption("width", 80L), gap = 4L) {
  col_widths <- pmax(nchar(cols$letter), nchar(cols$name))

  print_block <- function(idx) {
    # formatC()'s width argument is a single scalar applied to every
    # element of x, even if you pass it a vector -- it does NOT left-pad
    # each element to its own width. sprintf() genuinely vectorizes/
    # recycles all of its arguments elementwise, so "%-*s" applies each
    # element's own width from col_widths[idx].
    pad <- function(x) sprintf("%-*s", col_widths[idx], x[idx])
    sep <- strrep(" ", gap)
    cat(paste(pad(cols$letter), collapse = sep), "\n", sep = "")
    cat(paste(pad(cols$name), collapse = sep), "\n", sep = "")
  }

  block <- integer(0)
  line_width <- 0L
  for (i in seq_len(nrow(cols))) {
    added <- col_widths[i] + if (length(block) > 0) gap else 0L
    if (length(block) > 0 && line_width + added > width) {
      print_block(block)
      cat("\n")
      block <- integer(0)
      line_width <- 0L
      added <- col_widths[i]
    }
    block <- c(block, i)
    line_width <- line_width + added
  }
  print_block(block)
}

#' Glimpse a data frame's columns alongside their spreadsheet column letters
#'
#' A quick reference for which spreadsheet column letter each of `data`'s
#' columns will land in when written with `googlesheets4::range_write()`/
#' `sheet_write()`/[write_pretty_sheet()] -- handy right before writing an
#' A1-style `range` by hand instead of using [gs4_cols()] (which sidesteps
#' this entirely by matching on column name rather than letter).
#'
#' @param data A data frame.
#' @return A data frame with one row per column of `data` and columns
#'   `letter`/`name`, returned invisibly. Also printed as a compact,
#'   aligned preview -- wrapped across multiple blocks if every column
#'   doesn't fit on one line, the way [utils::str()] (or dplyr's
#'   `glimpse()`) wrap wide data.
#' @export
#' @examples
#' gs4_glimpse_cols(mtcars)
gs4_glimpse_cols <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (ncol(data) == 0) {
    cli::cli_abort("{.arg data} has no columns.")
  }

  names <- names(data)
  cols <- data.frame(
    letter = col_index_to_letter(seq_along(names)),
    name = names,
    stringsAsFactors = FALSE
  )

  print_glimpse_cols(cols)
  invisible(cols)
}
