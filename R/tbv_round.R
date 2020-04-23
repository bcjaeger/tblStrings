
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

#' Table rounding
#'
#' @param x a vector of numeric values
#'
#' @param max_decimals an integer value that will determine the maximum
#'   number of decimals in the output. Larger numbers will not use the
#'   maximum number of decimals in order to maintain the same, or at
#'   least similar length as smaller numbers.
#'
#' @param big_mark a character value used to separate number groups to the
#'   left of the decimal point. See [prettyNum] for more details on this.
#'   Set this input to '' to negate it's effect.
#'
#' @return a character vector with rounded values
#' @export
#'
#' @note `NA` values are automatically converted to character `'NA'` values.
#'   This is because different R packages that tabulate numbers handle
#'   true `NA` values differently, but handle character values the same way.
#'
#' @examples
#'
#' tbv_round( c(0.1234, 1.234, 12.34, 123.4, 1234) )
#'

tbv_round <- function (x, max_decimals = 2, big_mark = ',') {

  if (is_empty(x)) return("NA")

  if (is.integer(x)) warning(
    "x is an integer - are you sure you want to round x?",
    call. = FALSE
  )

  if (!is.numeric(x))
    stop("x should be numeric", call. = FALSE)

  out <- rep("NA", vctrs::vec_size(x))

  if (all(is.na(x))) return(out)

  # if rounded x is in 0 - 1 ----> 2 decimals
  # if rounded x is in 1 - 10 ---> 1 decimals
  # if rounded x is in 10 - 100 -> 0 decimals

  # take absolute value to round based only on magnitudes
  x_abs <- abs(x)

  # the breaks are based on rounded x instead of x itself
  x_brks <- c(0, 9.995, 99.95, Inf)

  # x_cuts create boundary categories for rounding
  x_cuts <- cut(
    x_abs,
    breaks = x_brks,
    include.lowest = TRUE,
    right = FALSE,
    labels = c(2, 1, 0)
  )

  i <- as.numeric(as.character(x_cuts))

  i_2 <- which(i == 2)
  i_1 <- which(i == 1)
  i_0 <- which(i == 0)


  if (!is_empty(i_2)){
    out[i_2] <- format(.round(x[i_2], max_decimals), justify = 'c',
      nsmall = safe_nsmall(max_decimals), big.mark = big_mark)
  }

  if (!is_empty(i_1)){
    md <- max(max_decimals - 1, 0)
    out[i_1] <- format(.round(x[i_1], md), justify = 'c',
      nsmall = safe_nsmall(max_decimals - 1), big.mark = big_mark)
  }

  if (!is_empty(i_0)){
    md <- max(max_decimals - 2, 0)
    out[i_0] <- format(.round(x[i_0], md), justify = 'c',
      nsmall = safe_nsmall(max_decimals - 2), big.mark = big_mark)
  }

  trimws(out)

}
