

new_pval <- function(
  x = double(),
  print_thresh = double(),
  column_label = character(),
  alpha_level = double()
) {

  vctrs::vec_assert(x, double())
  vctrs::vec_assert(print_thresh, double())
  vctrs::vec_assert(column_label, character())
  vctrs::vec_assert(alpha_level, double())

  max_decimals <- min(which(print_thresh * 10^seq(20) >= 1))

  vctrs::new_vctr(.data = x,
    max_decimals = max_decimals,
    print_thresh = print_thresh,
    label = column_label,
    signif = x < alpha_level,
    class = "tblStrings_pval"
  )

}


#' @describeIn pointGap
#'
#' @param x a numeric value or vector with values greater than 0
#'   and less than 1.
#'
#' @param print_thresh a numeric value indicating the lowest number that
#'   should be displayed. Any numbers below this threshold will be
#'   written as < threshold in the output.
#'
#' @param column_label a character value that could be used as a
#'   column header for the p-value in a published table.
#'
#' @param alpha_level a numeric value indicating the nominal type
#'   1 error rate for your analysis.
#'
#' @return a `pval` object
#'
#' @note the number of decimal digits to display is determined based
#'   on the threshold value. For example, if you set threshold as 0.01,
#'   then only 2 digits are shown. Similarly, a threshold of 0.001 will
#'   lead to displaying 3 digits.
#'
#' @export
#'
#' @examples
#'
#' pval(1/2)
#' pval(0.0001)
#' pval(c(0.1, 0.01, 0.001, 0.0001))
#' pval(c(0.1, 0.01, 0.001, 0.0001), print_thresh = 0.01)
#'

pval <- function(x = double(),
  print_thresh = 0.001,
  column_label = 'P-value',
  alpha_level = 0.05
) {

  x <- vctrs::vec_cast(x, double())
  p <- vctrs::vec_cast(print_thresh, double())
  a <- vctrs::vec_cast(alpha_level, double())
  l <- vctrs::vec_cast(column_label, character())

  validate_proportion(x, 'p-value')
  validate_proportion(p, 'print_thresh')
  validate_proportion(a, 'alpha_level')

  new_pval(x, p, l, a)

}

methods::setOldClass(c("tblStrings_pval", "vctrs_vctr"))


#' P-values
#'
#' @param x Object to be tested.
#'
#' @return `TRUE` if `x` inherits from the `pval` class, `FALSE` otherwise
#'
#' @export
#'
#' @examples
#'
#' is_pval(1/2)
#'
#' is_pval(pval(1/2))
#'

is_pval <- function(x) {
  inherits(x, "tblStrings_pval")
}

is_signif <- function(x){

  if(!is_pval(x)) stop("x must a vector with type <tblStrings_pval>. \n",
    "Instead, it has type <", typeof(x), ">")

  attr(x, 'signif')

}


#' @rdname obj_print_data.tblStrings_pointGap
#' @method format tblStrings_pval
#' @export
#' @export format.tblStrings_pval
format.tblStrings_pval <- function(x, ...) {

  vec_dat <- vctrs::vec_data(x)

  pthresh_lo <- print_thresh(x)
  pthresh_hi <- 1 - pthresh_lo

  out <- tbv_round(
    x = vec_dat,
    max_decimals = max_decimals(x)
  )

  out[vec_dat < pthresh_lo] <- paste0("<", pthresh_lo)
  out[vec_dat > pthresh_hi] <- paste0(">", pthresh_hi)
  out[is.na(x)] <- NA_character_
  out

}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method obj_print_data tblStrings_pval
#' @export
#' @export obj_print_data.tblStrings_pval
obj_print_data.tblStrings_pval <- function(x) {

  .x <- format(x)
  .less_than <- grepl(pattern = c("<"), .x, fixed = FALSE)
  .grtr_than <- grepl(pattern = c(">"), .x, fixed = FALSE)
  .add_space <- !.less_than & !.grtr_than
  .x[.add_space] <- paste0(" ", .x[.add_space])

  cat(.x, sep = '\n')

  # in case you want to add color at some point
  # add_color <- is_signif(x)
  #
  # for(i in seq_along(.x)){
  #
  #   if(add_color[i]){
  #     cat(crayon::cyan(.x[i]), '\n')
  #   } else {
  #     cat(.x[i], '\n')
  #   }
  #
  # }

}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_ptype_abbr tblStrings_pval
#' @export
#' @export vec_ptype_abbr.tblStrings_pval
vec_ptype_abbr.tblStrings_pval <- function(x, ...) {
  "pval"
}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_ptype2 tblStrings_pval
#' @export
#' @export vec_ptype2.tblStrings_pval
vec_ptype2.tblStrings_pval <- function(x, y, ...){
  UseMethod("vec_ptype2.tblStrings_pval", y)
}

#' @method vec_ptype2.tblStrings_pval default
#' @export
vec_ptype2.tblStrings_pval.default <-
  function(x, y, ..., x_arg = "x", y_arg = "y") {
    vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  }

#' @method vec_ptype2.tblStrings_pval tblStrings_pval
#' @export
vec_ptype2.tblStrings_pval.tblStrings_pval <- function(x, y, ...) {
  new_pval(print_thresh = min(print_thresh(x), print_thresh(y)),
    column_label = column_label(x))
}

#' @method vec_ptype2.tblStrings_pval double
#' @export
vec_ptype2.tblStrings_pval.double <- function(x, y, ...) double()

#' @method vec_ptype2.double tblStrings_pval
#' @export
vec_ptype2.double.tblStrings_pval <- function(x, y, ...) double()

#' @method vec_ptype2.tblStrings_pval character
#' @export
vec_ptype2.tblStrings_pval.character <- function(x, y, ...) character()

#' @method vec_ptype2.character tblStrings_pval
#' @export
vec_ptype2.character.tblStrings_pval <- function(x, y, ...) character()


#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_cast tblStrings_pval
#' @export
#' @export vec_cast.tblStrings_pval
vec_cast.tblStrings_pval <- function(x, to, ...){
  UseMethod("vec_cast.tblStrings_pval")
}

#' @method vec_cast.tblStrings_pval default
#' @export
vec_cast.tblStrings_pval.default <- function(x, to, ...){
  vctrs::vec_default_cast(x, to)
}

#' @method vec_cast.double tblStrings_pval
#' @export
vec_cast.double.tblStrings_pval <- function(x, to, ...) vctrs::vec_data(x)

#' @method vec_cast.character tblStrings_pval
#' @export
vec_cast.character.tblStrings_pval <- function(x, to, ...) format(x)

#' @method vec_cast.tblStrings_pval tblStrings_pval
#' @export
vec_cast.tblStrings_pval.tblStrings_pval <- function(x, to, ...) {
  new_pval(vctrs::vec_data(x), print_thresh = print_thresh(to),
    column_label = column_label(to))
}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_proxy_equal tblStrings_pval
#' @export
#' @export vec_proxy_equal.tblStrings_pval
vec_proxy_equal.tblStrings_pval <- function(x, ...) vctrs::vec_data(x)

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_proxy_compare tblStrings_pval
#' @export
#' @export vec_proxy_compare.tblStrings_pval
vec_proxy_compare.tblStrings_pval <- function(x, ...) vctrs::vec_data(x)

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_math tblStrings_pval
#' @export
#' @export vec_math.tblStrings_pval
vec_math.tblStrings_pval <- function(.fn, .x, ...) {

  stop("Doing math with p-values is not allowed.", call. = FALSE)

}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_arith tblStrings_pval
#' @export
#' @export vec_arith.tblStrings_pval
vec_arith.tblStrings_pval <- function(op, x, y, ...) {
  UseMethod("vec_arith.tblStrings_pval", y)
}

#' @method vec_arith.tblStrings_pval default
#' @export
vec_arith.tblStrings_pval.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}




