
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

is_empty <- function (x) length(x) == 0

#' Table value rounding
#'
#' @param x a vector of numeric values
#'
#' @param decimals_0_to_1 number of decimals to display for
#'   numbers < 1
#'
#' @param decimals_1_to_10 number of decimals to display for
#'   numbers >= 1 and < 10
#'
#' @param decimals_10_to_100 number of decimals to display for
#'   numbers >= 10 and < 100
#'
#' @param decimals_100_plus number of decimals to display for
#'   numbers >= 100
#'
#' @param big_mark a character value used to separate number groups to the
#'   left of the decimal point. See [prettyNum] for more details on this.
#'   Set this input to '' to negate it's effect.
#'
#' @param .missing a character string that replaces missing values.
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
#' tbl_val( c(0.1234, 1.234, 12.34, 123.4, 1234), 2, 1, 1, 0 )
#'

tbl_val <- function (
  x,
  decimals_0_to_1    = 2,
  decimals_1_to_10   = 1,
  decimals_10_to_100 = 0,
  decimals_100_plus  = 0,
  big_mark = ',',
  .missing = '--'
) {

  if (is_empty(x)) return(.missing)

  decimals <- c(
    decimals_0_to_1,
    decimals_1_to_10,
    decimals_10_to_100,
    decimals_100_plus
  )

  if (is.integer(x)) return(format(x, big.mark = big_mark))

  if (!is.numeric(x))
    stop("x should be numeric", call. = FALSE)

  out <- rep(.missing, vctrs::vec_size(x))

  if (all(is.na(x))) return(out)

  # take absolute value to round based only on magnitudes
  x_abs <- abs(x)

  # the breaks are based on rounded x instead of x itself
  x_brks <- c(0, 0.995, 9.995, 99.95, Inf)

  # x_cuts create boundary categories for rounding
  x_cuts <- cut(
    x_abs,
    breaks = x_brks,
    include.lowest = TRUE,
    right = FALSE
  )

  out_breaks <- list(
    lt1 = which(x_cuts == '[0,0.995)'),
    lt10 = which(x_cuts == '[0.995,9.99)'),
    lt100 = which(x_cuts == '[9.99,100)'),
    gt100 = which(x_cuts == '[100,Inf]')
  )

  for (i in seq_along(out_breaks)) {

    ob <- out_breaks[[i]]

    if(!is_empty(ob)){
      out[ob] <- format(.round(x[ob], decimals[i]), justify = 'c',
        nsmall = safe_nsmall(decimals[i]), big.mark = big_mark)
    }

  }

  trimws(out)

}



# In presenting p-values, please limit to 2 decimal places for .99 ≥ p ≥ .01; limit to 3 decimal places for .01 > p ≥ .001; and for smaller values express as "p<.001"
