#' Conditional-format rule builders
#'
#' Small, composable helpers that build the `condition` object for a boolean
#' conditional-format rule, in the spirit of `dplyr`'s `across()`/`n()` — you
#' pass the result straight into [range_add_conditional_format()]'s `rule`
#' argument rather than hand-writing nested lists.
#'
#' @param operator One of `">"`, `">="`, `"<"`, `"<="`, `"=="`, `"!="`, or
#'   `"between"`. Raw Sheets API `ConditionType` enum strings (e.g.
#'   `"NUMBER_GREATER"`) are also accepted, in case you need a condition type
#'   this wrapper doesn't have a shorthand for.
#' @param value For most operators, a single number. For `"between"`, a
#'   length-2 vector giving the low and high bounds.
#' @return A list suitable as the `rule` argument of
#'   [range_add_conditional_format()].
#' @export
#' @examples
#' cf_cell_value(">", 100)
#' cf_cell_value("between", c(10, 20))
cf_cell_value <- function(operator, value) {
  op_map <- c(
    ">" = "NUMBER_GREATER",
    ">=" = "NUMBER_GREATER_THAN_EQ",
    "<" = "NUMBER_LESS",
    "<=" = "NUMBER_LESS_THAN_EQ",
    "==" = "NUMBER_EQ",
    "!=" = "NUMBER_NOT_EQ",
    "between" = "NUMBER_BETWEEN"
  )
  condition_type <- if (operator %in% names(op_map)) op_map[[operator]] else operator

  values <- if (condition_type == "NUMBER_BETWEEN") {
    if (length(value) != 2) {
      cli::cli_abort("{.arg value} must have length 2 (low, high) when {.arg operator} is {.val between}.")
    }
    lapply(value, function(v) list(userEnteredValue = as.character(v)))
  } else {
    list(list(userEnteredValue = as.character(value)))
  }

  list(type = condition_type, values = values)
}

#' @rdname cf_cell_value
#' @param text Text to match.
#' @export
cf_text_contains <- function(text) {
  list(type = "TEXT_CONTAINS", values = list(list(userEnteredValue = text)))
}

#' @rdname cf_cell_value
#' @export
cf_text_eq <- function(text) {
  list(type = "TEXT_EQ", values = list(list(userEnteredValue = text)))
}

#' @rdname cf_cell_value
#' @param formula A spreadsheet formula string, e.g. `"=$B2>$C2"`, evaluated
#'   relative to the top-left cell of the range.
#' @export
cf_custom_formula <- function(formula) {
  list(type = "CUSTOM_FORMULA", values = list(list(userEnteredValue = formula)))
}

#' @rdname cf_cell_value
#' @export
cf_blank <- function() list(type = "BLANK")

#' @rdname cf_cell_value
#' @export
cf_not_blank <- function() list(type = "NOT_BLANK")

#' Format to apply when a conditional-format rule matches
#'
#' Per the Sheets API docs, conditional formatting only supports five
#' properties — bold, italic, strikethrough, and foreground/background
#' color. (Number formats, alignment, and font family/size are *not*
#' honored inside a conditional rule, even though they work fine in
#' [range_format()] for plain cell formatting — so they're deliberately not
#' exposed here.)
#'
#' @param bold,italic,strikethrough Logical.
#' @param font_color,background_color A color: hex string, named R color,
#'   or [gs4_palette] name.
#' @return A `CellFormat` list suitable as the `format` argument of
#'   [range_add_conditional_format()].
#' @export
#' @examples
#' cf_format(bold = TRUE, background_color = "#F4CCCC")
cf_format <- function(bold = NULL,
                       italic = NULL,
                       strikethrough = NULL,
                       font_color = NULL,
                       background_color = NULL) {
  built <- build_cell_format(
    bold = bold, italic = italic, strikethrough = strikethrough,
    font_color = font_color, background_color = background_color
  )
  built$cell_format
}

#' Add a boolean conditional-format rule to a range
#'
#' @inheritParams range_format
#' @param rule A condition built with [cf_cell_value()], [cf_text_contains()],
#'   [cf_text_eq()], [cf_custom_formula()], [cf_blank()], or [cf_not_blank()].
#' @param format A format built with [cf_format()], applied to cells that
#'   match `rule`.
#' @param index Rule priority; `0` (the default) puts the new rule first,
#'   i.e. highest priority.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes an `AddConditionalFormatRuleRequest` with a `BooleanRule`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#addconditionalformatrulerequest>
#' @export
#' @examples
#' \dontrun{
#' range_add_conditional_format(
#'   ss, range = "C2:C100",
#'   rule = cf_cell_value(">", 100),
#'   format = cf_format(background_color = "#F4CCCC")
#' )
#' range_add_conditional_format(
#'   ss, range = gs4_cols("score"),
#'   rule = cf_cell_value(">", 90),
#'   format = cf_format(bold = TRUE, background_color = "#F4CCCC")
#' )
#' }
range_add_conditional_format <- function(ss, sheet = NULL, range, rule, format, index = 0) {
  ss <- googlesheets4::as_sheets_id(ss)
  # resolve_grid_ranges() (plural), not resolve_grid_range() -- range can be
  # a gs4_cols() selection, which may expand into more than one GridRange
  # (one per contiguous run of matched columns). A single
  # ConditionalFormatRule's `ranges` accepts a list of GridRanges, so every
  # matched range is folded into one rule/one request rather than one rule
  # per range.
  grid_ranges <- resolve_grid_ranges(ss, sheet, range)

  request <- list(
    addConditionalFormatRule = list(
      rule = list(
        ranges = grid_ranges,
        booleanRule = list(condition = rule, format = format)
      ),
      index = index
    )
  )
  send_or_queue(ss, request)
}

#' Build a gradient (color-scale) rule
#'
#' @param min_color,mid_color,max_color Colors for the low, middle (optional),
#'   and high end of the scale.
#' @param min_type,mid_type,max_type How each point's value is determined:
#'   one of `"MIN"`, `"MAX"`, `"NUMBER"`, `"PERCENT"`, `"PERCENTILE"`.
#'   Defaults are `"MIN"` for the low end and `"MAX"` for the high end.
#' @param min_value,mid_value,max_value Numeric value for the point, required
#'   when the corresponding `*_type` is `"NUMBER"`, `"PERCENT"`, or
#'   `"PERCENTILE"`.
#' @return A list suitable as the `gradient` argument of
#'   [range_add_gradient_format()].
#' @export
#' @examples
#' cf_gradient(min_color = "white", max_color = "forestgreen")
cf_gradient <- function(min_color,
                         mid_color = NULL,
                         max_color,
                         min_type = "MIN",
                         mid_type = NULL,
                         max_type = "MAX",
                         min_value = NULL,
                         mid_value = NULL,
                         max_value = NULL) {
  point <- function(type, color, value) {
    if (is.null(color)) {
      return(NULL)
    }
    p <- list(color = gs4_color(color), type = type)
    if (!is.null(value)) p$value <- as.character(value)
    p
  }
  drop_nulls(list(
    minpoint = point(min_type, min_color, min_value),
    midpoint = point(mid_type, mid_color, mid_value),
    maxpoint = point(max_type, max_color, max_value)
  ))
}

#' Add a gradient conditional-format rule to a range
#'
#' @inheritParams range_format
#' @param gradient A gradient built with [cf_gradient()].
#' @param index Rule priority; `0` (the default) is highest priority.
#' @return The input `ss`, as a `sheets_id`, invisibly.
#' @seealso Makes an `AddConditionalFormatRuleRequest` with a `GradientRule`:
#'   <https://developers.google.com/workspace/sheets/api/reference/rest/v4/spreadsheets/request#addconditionalformatrulerequest>
#' @export
#' @examples
#' \dontrun{
#' range_add_gradient_format(
#'   ss, range = "D2:D100",
#'   gradient = cf_gradient(min_color = "white", max_color = "forestgreen")
#' )
#' range_add_gradient_format(
#'   ss, range = gs4_cols("score"),
#'   gradient = cf_gradient(min_color = "white", max_color = "forestgreen")
#' )
#' }
range_add_gradient_format <- function(ss, sheet = NULL, range, gradient, index = 0) {
  ss <- googlesheets4::as_sheets_id(ss)
  # See range_add_conditional_format() -- resolve_grid_ranges() (plural) so
  # a gs4_cols() selection (possibly several contiguous runs) is supported,
  # all folded into this one rule's `ranges` list.
  grid_ranges <- resolve_grid_ranges(ss, sheet, range)

  request <- list(
    addConditionalFormatRule = list(
      rule = list(
        ranges = grid_ranges,
        gradientRule = gradient
      ),
      index = index
    )
  )
  send_or_queue(ss, request)
}
