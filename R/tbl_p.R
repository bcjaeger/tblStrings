
#' P-value rounding
#'
#' @param x a vector of numeric values
#' @param decimals number of decimals to print
#' @param thresh_lo the lowest value printed. Values lower than the
#'   threshold will be printed as <threshold.
#' @param thresh_hi the highest value printed. Values higher than the
#'   threshold will be printed as >threshold.
#'
#' @return a character vector
#' @export
#'
#' @examples
#'
#' pvals <- c(0.01, 0.001, 0.0001, 0.9, 0.99, 0.9999)
#' tbl_p(pvals)
#'
tbl_p <- function(x, decimals = 3, thresh_lo = 0.001, thresh_hi = 0.999){

  vctrs::vec_assert(x, ptype = double())
  out <- tbl_val(x, max_decimals = decimals)
  out[x < thresh_lo] <- paste0("<", thresh_lo)
  out[x > thresh_hi] <- paste0(">", thresh_hi)
  out[is.na(x)] <- NA_character_
  out

}
