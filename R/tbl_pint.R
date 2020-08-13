

#' Point and interval (pint)
#'
#' @param estimate point estimate values
#' @param lower lower values of interval estimate
#' @param upper upper values of interval estimate
#' @param bracket_left character value that will close the interval on the left
#' @param bracket_right character value that will close the interval on the right
#' @param bracket_sep character value that will separate the lower and upper interval estimates
#' @inheritDotParams tbl_string
#'
#' @return a character vector of length equal to the estimate vector.
#' @export
#'
#' @examples
#'
#' tbl_pint(1.12, 1.00, 1.25)
#'
tbl_pint <- function(estimate, lower, upper,
                     bracket_left = '(',
                     bracket_right = ')',
                     bracket_sep = ', ',
                     ...){

  if(any(lower < estimate)) warning('some lower values are < their estimate')
  if(any(upper > estimate)) warning('some upper values are > their estimate')

  tbl_string(
    "{estimate} {bracket_left}{lower}{bracket_sep}{upper}{bracket_right}",
    ...
  )

}
