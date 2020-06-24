
.round = function(x, digits = 0) {

  # Why do we need this?
  # default behavior of the round() function is not ideal for tables
  # (inconsistency with 0.5 sometimes rounding to 0 instead of 1)

  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg

}

safe_nsmall <- function(x){
  x <- max(x, 0)
  x <- min(x, 20)
  x
}

find_smallest_10 <- function(x, y = 1e-10){

  if(x < 1e-10) stop(
    'the number you are attempting to round is too small',
    call. = FALSE
  )

  if(x == Inf) return(Inf)

  if (x < y) { return(y/10) } else { find_smallest_10(x, y*10) }

}

duplicate_last <- function(x) c(x, x[length(x)])

is_empty <- function (x) length(x) == 0

#' Table value rounding
#'
#' @param x a vector of numeric values
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
#' @param zero_print logical, character string or NULL specifying if and
#'   how zeros should be formatted specially. Useful for pretty printing
#'  'sparse' objects.
#'
#' @param trim logical; if `FALSE`, logical, numeric and complex values
#'   are right-justified to a common width: if `TRUE` the leading blanks
#'   for justification are suppressed.
#'
#' @return a character vector with rounded values
#'
#' @export
#'
#' @note `tbl_val` converts `NA` values to non-missing character values.
#'   This is because different R packages that tabulate numbers handle
#'   true `NA` values differently, but handle character values the same way.
#'
#' @examples
#'
#' tbl_val( c(0.1234, 1.234, 12.34, 123.4, 1234) )
#'
#' tbl_val( c(0.995, 9.995, 99.95, 2003.5), c(1,10,100,Inf), c(2,2,1,0) )
#'

tbl_val <- function (
  x,
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

  if (length(breaks) != length(decimals))
    stop('breaks and decimals should have equal length', call. = FALSE)

  if (is_empty(x)) stop("cannot format empty vectors", call. = FALSE)

  if (!is.numeric(x))
    stop("x should be numeric", call. = FALSE)

  if (is.integer(x)) return(format(x, big.mark = big_mark))

  if(!(all(breaks>0))) stop("all breaks should be > 0", call. = FALSE)

  if(any(diff(breaks) < 0))
    stop("breaks should be strictly monotonically increasing", call. = FALSE)

  out <- rep(miss_replace, vctrs::vec_size(x))

  if (all(is.na(x))) return(out)

  # take absolute value to round based only on magnitudes
  x_abs <- abs(x)

  # the breaks are based on rounded x instead of x itself
  breaks_smallest_10 <- sapply(breaks, find_smallest_10)

  # rounding to 0 decimals, 9.5 should be considered as if it were 10
  # rounding to 1 decimals, 9.95 should be considered as if it were 10
  # rounding to 2 decimals, 9.995 should be considered as if it were 10
  # in general...

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
      out[ob] <- .round(x[ob], digits = decimals[i]) %>%
        format(
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



