
#' Table value rounding
#'
#' @param x a vector of numeric values
#'
#' @param round_half_to_even a logical value. If `TRUE`, then when the
#'   fractional part of `x` is half-way between two rounding boundaries,
#'   it will be rounded to the even boundary nearest to x. For example,
#'   +23.5 becomes +24, as does +24.5; while −23.5 becomes −24,
#'   as does −24.5. If `FALSE`, halves are automatically rounded to
#'   the boundary with highest absolute value.
#'
#' @param breaks a positive, monotonically increasing numeric vector
#'   designating rounding boundaries. With `breaks = c(1, 10, 100, 1000)`
#'   and `decimals = c(2, 1, 0)`
#'
#' @param decimals a numeric vector of equal length to `breaks` that
#'   indicates how many decimals to round to in the numeric range
#'   designated by `breaks`. (see notes for example). Allowed values
#'   are 0 <= `decimals` <= 20.
#'
#' @param big_mark a character value used to separate number groups to the
#'   left of the decimal point. See [prettyNum] for more details on this.
#'   Set this input to '' to negate it's effect.
#'
#' @param miss_replace a character string that replaces missing values.
#'
#' @param big_interval a numeric value indicating the size of number groups
#'   for numbers before (hence big) the decimal.
#'
#' @param small_mark a character value used to separate number groups to
#'   the right of the decimal point.
#'
#' @param small_interval a numeric value indicating the size of number groups
#'   for numbers after (hence small) the decimal.
#'
#' @param decimal_mark the character to be used to indicate the numeric
#'   decimal point.
#'
#' @param zero_print a logical value, character string or NULL value
#'   specifying if and how zeros should be formatted specially.
#'   Useful for pretty printing 'sparse' objects.
#'
#' @param trim a logical value; if `FALSE`, logical, numeric and complex values
#'   are right-justified to a common width: if `TRUE` the leading blanks
#'   for justification are suppressed.
#'
#' @return a character vector with rounded values
#'
#' @export
#'
#' @note `tbl_val` converts `NA` values to non-missing character values.
#'   This is because different R packages that tabulate numbers handle
#'   true `NA` values differently, but handle character values the same
#'   way.
#'
#' @details Using `round_half_to_even = TRUE` minimizes the expected
#'  error when summing over rounded figures, even when the inputs are
#'  mostly positive or mostly negative. This variant of the
#'  round-to-nearest method is also called convergent rounding,
#'  statistician's rounding, Dutch rounding, Gaussian rounding,
#'  odd–even rounding, or bankers' rounding.
#'
#' @examples
#'
#' tbl_val(x = c(0.1234, 1.234, 12.34, 123.4, 1234))
#'
#' tbl_val(x = c(0.995, 9.995, 99.95, 2003.5),
#'         breaks = c(1,10,100,Inf),
#'         decimals = c(2,2,1,0))
#'

tbl_val <- function(
  x,
  round_half_to_even = FALSE,
  breaks = c(1, 10, Inf),
  decimals = c(2, 1, 0),
  miss_replace = '--',
  big_mark = ',',
  big_interval = 3L,
  small_mark = '',
  small_interval = 5L,
  decimal_mark = getOption('OutDec'),
  zero_print = NULL,
  trim = TRUE
) {

  check_call(
    match.call(),
    expected = list(
      'x' = list(
        type = 'numeric',
        length = NULL,
        lwr = NULL,
        upr = NULL
      ),
      'round_half_to_even' = list(
        type = 'logical',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'breaks' = list(
        type = 'numeric',
        length = c(1, 3, length(decimals)),
        lwr = 0,
        upr = NULL
      ),
      'decimals' = list(
        type = 'numeric',
        length = c(1, 3, length(breaks)),
        lwr = -20,
        upr = 20
      ),
      'miss_replace' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'big_mark' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'big_interval' = list(
        type = 'numeric',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'small_mark' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'small_interval' = list(
        type = 'numeric',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'decimal_mark' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'zero_print' = list(
        type = c('logical', 'character'),
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'trim' = list(
        type = 'logical',
        length = 1,
        lwr = NULL,
        upr = NULL
      )
    )
  )

  # recycle decimals if length is 1
  if(length(decimals) == 1) decimals <- rep(decimals, length(breaks))

  if (length(breaks) != length(decimals))
    stop('breaks and decimals should have equal length', call. = FALSE)

  if (is_empty(x)) stop("cannot format empty vectors", call. = FALSE)

  if (!is.numeric(x))
    stop("x should be numeric", call. = FALSE)

  if (is.integer(x)) return(format(x, big.mark = big_mark))

  if(any(diff(breaks) < 0))
    stop("breaks should be strictly monotonically increasing", call. = FALSE)

  ..round <- if(round_half_to_even) base::round else .round

  out <- rep(miss_replace, length(x))

  if (all(is.na(x))) return(out)

  # take absolute value to round based only on magnitudes
  x_abs <- abs(x)

  # the breaks are based on rounded x instead of x itself
  breaks_smallest_10 <- sapply(breaks, find_smallest_10)

  # rounding to 0 decimals, 9.5 should be considered as if it were 10
  # rounding to 1 decimals, 9.95 should be considered as if it were 10
  # rounding to 2 decimals, 9.995 should be considered as if it were 10
  # in general the formula for bump down value is (1/2) / 10^decimals

  bump_down <- 0.5 / (10^decimals)

  x_brks <- c(0, breaks - bump_down)

  if(max(x_brks) < Inf){
    x_brks <- c(x_brks, Inf)
    decimals <- duplicate_last(decimals)
  }

  # x_cuts create boundary categories for rounding
  x_cuts <- cut(
    x_abs,
    breaks = x_brks,
    include.lowest = TRUE,
    right = FALSE
  )

  out_breaks <- lapply(
    levels(x_cuts),
    function(.x) which(x_cuts == .x)
  )

  for (i in seq_along(out_breaks)) {

    ob <- out_breaks[[i]]

    if(!is_empty(ob)){

      ob_rounded <- ..round(x[ob], digits = decimals[i])

      out[ob] <- format(
        ob_rounded,
        nsmall = safe_nsmall(decimals[i]),
        big.mark = big_mark,
        big.interval = big_interval,
        small.mark = small_mark,
        small.interval = small_interval,
        decimal.mark = decimal_mark,
        zero.print = zero_print,
        #justify = justify,
        trim = trim,
      )

    }

  }

  out

}




