
#' P-value rounding
#'
#' In presenting p-values, journals are likely to request
#'   2 decimal places for .99 >= p >= .01; 3 decimal places for
#'   .01 > p >= .001; and for smaller values express as "p<.001"
#'
#' @param x a vector of numeric values.
#' @param decimals_outer number of decimals to print when
#'   p > thresh_hi or p < thresh_lo.
#' @param decimals_inner number of decimals to print when
#'   `thresh_lo` < p < `thresh_hi`.
#' @param boundary_lo the lower bound of the inner range.
#' @param boundary_hi the upper bound of the inner range.
#' @param thresh_lo the lowest value printed. Values lower than the
#'   threshold will be printed as <threshold.
#' @param thresh_hi the highest value printed. Values higher than the
#'   threshold will be printed as >threshold.
#' @param miss_replace a character string that replaces missing values.
#'
#' @return a character vector
#'
#'
#' @export
#'
#' @examples
#'
#' pvals <- c(0.00999, 0.01, 0.0052, 0.0011, 0.0001, 0.9, 0.999, 0.9999)
#' tbl_pval(pvals)
#'
tbl_pval <- function(x,
  decimals_outer = 3,
  decimals_inner = 2,
  boundary_lo = 0.01,
  boundary_hi = 0.99,
  thresh_lo = 0.001,
  thresh_hi = 0.999,
  miss_replace = '--'
){

  out <- rep(miss_replace, length(x))
  out[x < thresh_lo] <- paste0("<", thresh_lo)
  out[x > thresh_hi] <- paste0(">", thresh_hi)

  x_in_lower_bound <- x <  boundary_lo & x >= thresh_lo
  x_in_middle      <- x >= boundary_lo & x <= boundary_hi
  x_in_upper_bound <- x >  boundary_hi & x <= thresh_hi

  if ( any(x_in_lower_bound) )
    out[x_in_lower_bound] <- tbl_val(x,
      breaks = c(boundary_lo, boundary_hi),
      decimals = c(decimals_outer, decimals_inner)
    )[x_in_lower_bound]

  if ( any(x_in_middle) )
    out[x_in_middle] <- tbl_val(x,
      breaks = c(boundary_lo, boundary_hi),
      decimals = c(decimals_outer, decimals_inner)
    )[x_in_middle]

  if( any(x_in_upper_bound) )
    out[x_in_upper_bound] <- tbl_val(x,
      breaks = c(boundary_lo, boundary_hi, 1),
      decimals = c(decimals_outer, decimals_inner, decimals_outer)
    )[x_in_upper_bound]

  out

}
