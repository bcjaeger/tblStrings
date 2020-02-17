

# Construction ---------------------------------------------------------------

new_numPer <- function(
  numerator    = integer(),
  denominator  = integer(),
  point_label  = "Count",
  gap_label    = "%",
  brac_left    = "(",
  brac_right   = ")",
  max_decimals = 2,
  big_mark     = ",",
  show_percent = TRUE
) {

  # main arguments
  vctrs::vec_assert(numerator,   integer())
  vctrs::vec_assert(denominator, integer())


  # attributes
  vctrs::vec_assert(point_label,  character())
  vctrs::vec_assert(gap_label,    character())
  vctrs::vec_assert(brac_left,    character())
  vctrs::vec_assert(brac_right,   character())
  vctrs::vec_assert(big_mark,     character())
  vctrs::vec_assert(max_decimals, numeric())
  vctrs::vec_assert(show_percent, logical())

  column_label <- paste0(point_label, ' ', brac_left, gap_label, brac_right)

  # use vctrs package vctr template
  vctrs::new_vctr(
    # main data
    .data = dbl_to_chr(list(numerator, denominator)),
    # S3 class
    class = "tblStrings_numPer",
    # attributes
    point_label  = point_label,
    gap_label    = gap_label,
    label        = column_label,
    brac_left    = brac_left,
    brac_right   = brac_right,
    max_decimals = max_decimals,
    big_mark     = big_mark,
    show_percent = show_percent
  )

}

methods::setOldClass(c("tblStrings_pointGap", "vctrs_vctr"))

validate_numPer <- function(numer, denom){

  if(any(numer < 0, na.rm = TRUE) | any(denom < 0, na.rm = TRUE)){
    stop("numerator and denominator must be positive",
      call. = FALSE)
  }

  if(any(denom == 0, na.rm = TRUE))
    stop("denominator cannot contain zero values", call. = FALSE)

}

#' @describeIn pointGap
#'
#' @param numerator positive count values.
#'
#' @param denominator positive size values.
#'
#' @param show_percent logical value. If `TRUE`, then a percent
#'   sign is placed into the brackets of the `numPer` printed output.
#'   if `FALSE`, then no percent sign is added to printed output.
#'
#' @return `numPer()` returns a `numPer` vector of length equal to
#'   the number of numerator/denominator pairs.
#'
#' @export
#'
#' @examples
#'
#' counts <- c(1, 20, 40, 50)
#'
#' denoms <- c(100, 100, 100, 100)
#'
#' numPer(counts, denoms)
#'

numPer <- function(
  numerator    = integer(),
  denominator  = integer(),
  point_label  = "Count",
  gap_label    = "%",
  brac_left    = "(",
  brac_right   = ")",
  max_decimals = 2,
  big_mark     = ",",
  show_percent = TRUE
) {

  # recycle denominator value if only one value is given
  if(vctrs::vec_size(denominator) == 1)
    denominator %<>% rep(times = vctrs::vec_size(numerator))

  # check input validity
  validate_numPer(numerator, denominator)

  # cast inputs to integer values
  numerator   <- vctrs::vec_cast(numerator,   integer())
  denominator <- vctrs::vec_cast(denominator, integer())

  # initiate numPer object
  new_numPer(
    # main arguments
    numerator   = numerator,
    denominator = denominator,
    # attributes
    point_label  = point_label,
    gap_label    = gap_label,
    brac_left    = brac_left,
    brac_right   = brac_right,
    max_decimals = max_decimals,
    big_mark     = big_mark,
    show_percent = show_percent
  )

}

# Formatting -----------------------------------------------------------------

#' @describeIn format.tblStrings_pointGap
#'
#' @method format tblStrings_numPer
#'
#' @export
#'
#' @export format.tblStrings_numPer
#'
format.tblStrings_numPer <- function(x, ...) {

  .dat <- chr_to_dbl(vctrs::vec_data(x))

  .dat[[2]] <- 100 * (.dat[[1]] / .dat[[2]])

  .dat[[1]] %<>%
    tbv_round(max_decimals = 0, big_mark = big_mark(x))

  .dat[[2]] %<>%
    tbv_round(max_decimals = max_decimals(x), big_mark = big_mark(x))

  paste0(
    .dat[[1]], ' ',  brac_left(x),
    .dat[[2]], if (show_percent(x)) '%', brac_right(x)
  )

}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method obj_print_data tblStrings_numPer
#' @export
#' @export obj_print_data.tblStrings_numPer
obj_print_data.tblStrings_numPer <- function(x) {
  cat(format(x), sep = "\n")
}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_ptype_abbr tblStrings_numPer
#' @export
#' @export vec_ptype_abbr.tblStrings_numPer
vec_ptype_abbr.tblStrings_numPer <- function(x, ...) {
  "numPer"
}

# Casting --------------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_cast tblStrings_numPer
#' @export
#' @export vec_cast.tblStrings_numPer
vec_cast.tblStrings_numPer <- function(x, to, ...){
  UseMethod("vec_cast.tblStrings_numPer")
}

#' @method vec_cast.tblStrings_numPer default
#' @export
vec_cast.tblStrings_numPer.default <- function(x, to, ...){
  vctrs::vec_default_cast(x, to)
}

#' @method vec_cast.tblStrings_numPer tblStrings_numPer
#' @export
vec_cast.tblStrings_numPer.tblStrings_numPer <- function(x, to, ...) x

#' @method vec_cast.character tblStrings_numPer
#' @export
vec_cast.character.tblStrings_numPer <- function(x, to, ...){
  format(x)
}

#' @method vec_cast.tblStrings_numPer numeric
#' @export
vec_cast.tblStrings_numPer.numeric <- function(x, to, ...){
  stopifnot(vctrs::vec_size(x) == 2L)
  numPer(x[1], x[2])
}

#' @method vec_cast.tblStrings_numPer matrix
#' @export
vec_cast.tblStrings_numPer.matrix <- function(x, to, ...){
  stopifnot(ncol(x) == 2L)
  numPer(x[, 1], x[, 2])
}

#' @method vec_cast.tblStrings_numPer list
#' @export
vec_cast.tblStrings_numPer.list <- function(x, to, ...){
  stopifnot(vctrs::vec_size(x) == 2L)
  numPer(x[[1]], x[[2]])
}

#' @method vec_cast.tblStrings_numPer data.frame
#' @export
vec_cast.tblStrings_numPer.data.frame <- function(x, to, ...){
  vec_cast.tblStrings_numPer(as.list(x))
}

#' @rdname as_pointGap
#' @export
#' @examples
#' as_numPer(list(1,2))
#' as_numPer(list(c(1:10), 100))
as_numPer <- function(x) {
  vec_cast.tblStrings_numPer(x)
}

# Coercion -------------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_ptype2 tblStrings_numPer
#' @export
#' @export vec_ptype2.tblStrings_numPer
vec_ptype2.tblStrings_numPer <- function(x, y, ...) {
  UseMethod("vec_ptype2.tblStrings_numPer", y)
}

#' @method vec_ptype2.tblStrings_numPer default
#' @export
vec_ptype2.tblStrings_numPer.default <-
  function(x, y, ..., x_arg = "x", y_arg = "y") {
    vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  }

#' @method vec_ptype2.tblStrings_numPer tblStrings_numPer
#' @export
vec_ptype2.tblStrings_numPer.tblStrings_numPer <- function(x, y, ...)
  new_numPer(
    point_label  = point_label(x),
    gap_label    = gap_label(x),
    brac_left    = brac_left(x),
    brac_right   = brac_right(x),
    max_decimals = max_decimals(x),
    big_mark     = big_mark(x),
    show_percent = show_percent(x)
  )

# Comparisons ----------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_proxy_equal tblStrings_numPer
#' @export
#' @export vec_proxy_equal.tblStrings_numPer
vec_proxy_equal.tblStrings_numPer <- function(x, ...) {

  matrix(
    unlist(chr_to_dbl(vctrs::vec_data(x))),
    nrow = vctrs::vec_size(x),
    byrow = FALSE
  )

}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_proxy_compare tblStrings_numPer
#' @export
#' @export vec_proxy_compare.tblStrings_numPer
vec_proxy_compare.tblStrings_numPer <- function(x, ...) {

  # sort by percentage rather than counts
  .dat <- chr_to_dbl(vctrs::vec_data(x))
  .dat[[1]] / .dat[[2]]

}

# Math -----------------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_math tblStrings_numPer
#' @export
#' @export vec_math.tblStrings_numPer
vec_math.tblStrings_numPer <- function(.fn, .x, ...) {

  # get character strings from x
  vctrs::vec_data(.x) %>%
    # convert to numeric data
    chr_to_dbl() %>%
    # apply operations to point/lower/upper, separately
    lapply(vctrs::vec_math_base, .fn = .fn, ...) %>%
    # convert back to number and percentage
    as_numPer() %>%
    # restore the original attributes
    vctrs::vec_restore(to = .x)

}


# Arithmetic -----------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_arith tblStrings_numPer
#' @export
#' @export vec_arith.tblStrings_numPer
vec_arith.tblStrings_numPer <- function(op, x, y, ...) {
  UseMethod("vec_arith.tblStrings_numPer", y)
}

#' @method vec_arith.tblStrings_numPer default
#' @export
vec_arith.tblStrings_numPer.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}


# @method vec_arith.tblStrings_numPer tblStrings_numPer
# @export
# vec_arith.tblStrings_numPer.tblStrings_numPer <- function(op,x,y,...){
#
#   .x <- chr_to_dbl(vctrs::vec_data(x))
#   .y <- chr_to_dbl(vctrs::vec_data(y))
#
#   numPer(
#     vctrs::vec_arith_base(op = op, x = .x[[1]], y = .y[[1]]),
#     vctrs::vec_arith_base(op = op, x = .x[[2]], y = .y[[2]]),
#     point_label  = point_label(x),
#     gap_label    = gap_label(x),
#     brac_left    = brac_left(x),
#     brac_right   = brac_right(x),
#     max_decimals = max_decimals(x),
#     big_mark     = big_mark(x),
#     show_percent = show_percent(x)
#   )
#
# }


# Front-end ------------------------------------------------------------------

#' @rdname is_pointGap
#' @export
is_numPer <- function(x) {
  inherits(x, "tblStrings_numPer")
}

#' numPer comparisons
#'
#' @description Compare the counts and percentages comprised by a `numPer`
#'   object to standard values. The names of these functions indicate their
#'   behavior. Each functions starts with `np` to indicate that it is
#'   specifically for `numPer` objects. The middle term indicates which
#'   part of the `numPer` is compared to `value`.
#'
#'    - `perc`: compare the `numPer`'s percent to `value`
#'
#'    - `nums`: compare the `numPer`'s counts to `value`
#'
#'  The last term indicates the comparison operator.
#'
#'    - `gt`: strictly greater than
#'
#'    - `gteq`: greater than or equal to
#'
#'    - `lt`: strictly less than
#'
#'    - `lteq`: less than or equal to
#'
#' @note the percent value for a `numPer` vector is between 0 and 1.
#'   Comparisons between percents and `value` should be made accordingly.
#'
#' @param x a `numPer` vector
#'
#' @param value a numeric value to compare with the `numPer` vector's data.
#'
#' @return a logical vector of length equal to the size of `x`
#'
#' @export
#'
#' @examples
#'
#' # 20 out of 100 - 20%
#' np <- numPer(20, 100)
#'
#' np_perc_gt(np, 1/5)
#' np_perc_lt(np, 1/5)
#' np_perc_gteq(np, 1/5)
#' np_perc_lteq(np, 1/5)
#'
#'
np_perc_gt <- function(x, value){

  .x   <- chr_to_dbl(vctrs::vec_data(x))
  perc <- .x[[1]] / .x[[2]]

  perc > value

}
#' @rdname np_perc_gt
#' @export
np_nums_gt <- function(x, value){

  .x   <- chr_to_dbl(vctrs::vec_data(x))
  nums <- .x[[1]]

  nums > value

}
#' @rdname np_perc_gt
#' @export
np_perc_gteq <- function(x, value){

  .x   <- chr_to_dbl(vctrs::vec_data(x))
  perc <- .x[[1]] / .x[[2]]

  perc >= value

}
#' @rdname np_perc_gt
#' @export
np_nums_gteq <- function(x, value){

  .x   <- chr_to_dbl(vctrs::vec_data(x))
  nums <- .x[[1]]

  nums >= value

}
#' @rdname np_perc_gt
#' @export
np_perc_lt <- function(x, value){
  !np_perc_gteq(x, value)
}
#' @rdname np_perc_gt
#' @export
np_nums_lt <- function(x, value){
  !np_nums_gteq(x, value)
}
#' @rdname np_perc_gt
#' @export
np_perc_lteq <- function(x, value){
  !np_perc_gt(x, value)
}
#' @rdname np_perc_gt
#' @export
np_nums_lteq <- function(x, value){
  !np_nums_gt(x, value)
}
