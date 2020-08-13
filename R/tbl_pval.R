
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
#' @param miss_replace a character value that replaces missing values.
#' @param drop_leading_zero a logical value. If `TRUE`, the leading 0
#'   is dropped for all p-values. So, '0.04' will become '.04'. If `FALSE`,
#'   no leading zeroes are dropped.
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
  decimals_outer = 3L,
  decimals_inner = 2L,
  boundary_lo = 0.01,
  boundary_hi = 0.99,
  thresh_lo = 0.001,
  thresh_hi = 0.999,
  miss_replace = '--',
  drop_leading_zero = TRUE
){

  check_call(
    match.call(),
    expected = list(
      'x' = list(
        type = 'double',
        length = NULL,
        lwr = 0,
        upr = 1
      ),
      'decimals_outer' = list(
        type = 'numeric',
        length = 1,
        lwr = 1,
        upr = 20
      ),
      'decimals_inner' = list(
        type = 'numeric',
        length = 1,
        lwr = 1,
        upr = 20
      ),
      'boundary_lo' = list(
        type = 'double',
        length = 1,
        lwr = 0,
        upr = 1
      ),
      'boundary_hi' = list(
        type = 'double',
        length = 1,
        lwr = 0,
        upr = 1
      ),
      'thresh_lo' = list(
        type = 'double',
        length = 1,
        lwr = 0,
        upr = 1
      ),
      'thresh_hi' = list(
        type = 'double',
        length = 1,
        lwr = 0,
        upr = 1
      ),
      'miss_replace' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'drop_leading_zero' = list(
        type = 'logical',
        length = 1,
        lwr = NULL,
        upr = NULL
      )
    )
  )

  if(decimals_outer != as.integer(decimals_outer))
    stop("decimals_outer should be integer valued")
  if(decimals_inner != as.integer(decimals_inner))
    stop("decimals_inner should be integer valued")

  if( any(x <= 0) ) stop("some values in x are <= 0", call. = FALSE)
  if( any(x >= 1) ) stop("some values in x are >= 1", call. = FALSE)

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

  # The following guidelines are based on information in the AMA
  # Manual of Style: Round P values to 2 or 3 digits after the
  # decimal point, depending on the number of zeros. Change
  # .157 to .16. Change .037 to .04. Don't change .047 to .05,
  # because it will no longer be significant. Keep .003 as is
  # because 2 zeros after the decimal are fine. Change .0003 or
  # .00003 or .000003 to <.001, because "expressing P to more
  # than 3 significant digits does not add useful information."

  if( any(out == '0.05') ) {

    x_in_yellow_zone <- which(out == "0.05" & x < 0.05)
    out[x_in_yellow_zone] <- tbl_val(x[x_in_yellow_zone], decimals = 3)

  }

  if (drop_leading_zero ) {
    out <- gsub(pattern = '0.', replacement = '.', x = out, fixed = TRUE)
  }

  out

}
