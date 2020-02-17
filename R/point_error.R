

# Construction ---------------------------------------------------------------

new_pointErr <- function(
  point = numeric(),
  error = numeric(),
  style = 'pm',
  point_label = "Estimate",
  error_label = "SE",
  brac_left   = "(",
  brac_right  = ")",
  max_decimals = 2,
  big_mark = ","
) {

  # main arguments
  vctrs::vec_assert(point, numeric())
  vctrs::vec_assert(error, numeric())
  vctrs::vec_assert(style, character())

  # attributes
  vctrs::vec_assert(point_label,  character())
  vctrs::vec_assert(error_label,  character())
  vctrs::vec_assert(brac_left,    character())
  vctrs::vec_assert(brac_right,   character())
  vctrs::vec_assert(big_mark,     character())
  vctrs::vec_assert(max_decimals, numeric())

  column_label <- switch(
    style,
    'pm'   = paste0(point_label, ' \u00B1 ', error_label),
    'brac' = paste0(point_label, ' ', brac_left, error_label, brac_right),
    stop("style <", style, "> is unrecognized.",
      "Please use 'pm' or 'brac'", call. = FALSE)
  )

  # use vctrs package vctr template
  vctrs::new_vctr(
    # main data
    .data = dbl_to_chr(list(point, error)),
    # S3 class
    class = "tblStrings_pointErr",
    # attributes
    style = style,
    label = column_label,
    point_label = point_label,
    error_label = error_label,
    brac_left = brac_left,
    brac_right = brac_right,
    max_decimals = max_decimals,
    big_mark = big_mark
  )

}

methods::setOldClass(c("tblStrings_pointErr", "vctrs_vctr"))

validate_pointErr <- function(point, error) {

  stopifnot(vctrs::vec_size(point) ==  vctrs::vec_size(error))

  if(any(error < 0, na.rm = TRUE)) stop('Error values must be >0',
    call. = FALSE)
}

#' @describeIn pointGap
#'
#' @param error numeric vector of error estimates
#'
#' @param style a character value indicating the style of the point error
#'   vectors.
#'
#' @param error_label character value describing the error value.
#'
#' @export

pointErr <- function(
  point = numeric(),
  error = numeric(),
  style = 'pm',
  point_label = "Estimate",
  error_label = "SE",
  brac_left   = "(",
  brac_right  = ")",
  max_decimals = 2,
  big_mark = ","
) {

  point <- vctrs::vec_cast(point, double())
  error <- vctrs::vec_cast(error, double())
  style <- vctrs::vec_cast(style, character())

  validate_pointErr(point = point, error = error)

  new_pointErr(
    point = point,
    error = error,
    style = style,
    point_label = point_label,
    error_label = error_label,
    brac_left = brac_left,
    brac_right = brac_right,
    max_decimals = max_decimals,
    big_mark = big_mark
  )

}

# Formatting -----------------------------------------------------------------

#' @describeIn format.tblStrings_pointGap
#'
#' @method format tblStrings_pointErr
#'
#' @export
#'
#' @export format.tblStrings_pointErr
#'

format.tblStrings_pointErr <- function(x, ...) {

  .dat <- chr_to_dbl(vctrs::vec_data(x)) %>%
    lapply(tbv_round, max_decimals = max_decimals(x), big_mark = big_mark(x))

  switch(
    style(x),
    'pm' = paste(.dat[[1]], '\u00B1', .dat[[2]]),
    'brac' = paste0(.dat[[1]], ' ',  brac_left(x), .dat[[2]], brac_right(x)),
    stop("style <", style, "> is unrecognized.", "Please use 'pm' or 'brac'",
      call. = FALSE)
  )

}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method obj_print_data tblStrings_pointErr
#' @export
#' @export obj_print_data.tblStrings_pointErr
obj_print_data.tblStrings_pointErr <- function(x) {
  cat(format(x), sep = "\n")
}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_ptype_abbr tblStrings_pointErr
#' @export
#' @export vec_ptype_abbr.tblStrings_pointErr
vec_ptype_abbr.tblStrings_pointErr <- function(x, ...) {
  "pointErr"
}

# Casting --------------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_cast tblStrings_pointErr
#' @export
#' @export vec_cast.tblStrings_pointErr
vec_cast.tblStrings_pointErr <- function(x, to, ...){
  UseMethod("vec_cast.tblStrings_pointErr")
}

#' @method vec_cast.tblStrings_pointErr default
#' @export
vec_cast.tblStrings_pointErr.default <- function(x, to, ...){
  vctrs::vec_default_cast(x, to)
}

#' @method vec_cast.tblStrings_pointErr tblStrings_pointErr
#' @export
vec_cast.tblStrings_pointErr.tblStrings_pointErr <- function(x, to, ...) x

#' @method vec_cast.character tblStrings_pointErr
#' @export
vec_cast.character.tblStrings_pointErr <- function(x, to, ...){
  format(x)
}

#' @method vec_cast.tblStrings_pointErr numeric
#' @export
vec_cast.tblStrings_pointErr.numeric <- function(x, to, ...){
  stopifnot(vctrs::vec_size(x) == 2L)
  pointErr(x[1], x[2])
}

#' @method vec_cast.tblStrings_pointErr matrix
#' @export
vec_cast.tblStrings_pointErr.matrix <- function(x, to, ...){
  stopifnot(ncol(x) == 2L)
  pointErr(x[, 1], x[, 2])
}

#' @method vec_cast.tblStrings_pointErr list
#' @export
vec_cast.tblStrings_pointErr.list <- function(x, to, ...){
  stopifnot(vctrs::vec_size(x) == 2L)
  pointErr(x[[1]], x[[2]])
}

#' @method vec_cast.tblStrings_pointErr data.frame
#' @export
vec_cast.tblStrings_pointErr.data.frame <- function(x, to, ...){
  vec_cast.tblStrings_pointErr(as.list(x))
}

#' @rdname as_pointGap
#' @export
#' @examples
#' as_pointErr(list(1,2))
#' as_pointErr(list(c(1:10), (1:10)*10))
as_pointErr <- function(x) {
  vec_cast.tblStrings_pointErr(x)
}

# Coercion -------------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_ptype2 tblStrings_pointErr
#' @export
#' @export vec_ptype2.tblStrings_pointErr
vec_ptype2.tblStrings_pointErr <- function(x, y, ...) {
  UseMethod("vec_ptype2.tblStrings_pointErr", y)
}

#' @method vec_ptype2.tblStrings_pointErr default
#' @export
vec_ptype2.tblStrings_pointErr.default <-
  function(x, y, ..., x_arg = "x", y_arg = "y") {
    vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  }

#' @method vec_ptype2.tblStrings_pointErr tblStrings_pointErr
#' @export
vec_ptype2.tblStrings_pointErr.tblStrings_pointErr <- function(x, y, ...)
  new_pointErr(
    style = style(x),
    point_label = point_label(x),
    error_label = error_label(x),
    brac_left = brac_left(x),
    brac_right = brac_right(x),
    max_decimals = max_decimals(x),
    big_mark = big_mark(x)
  )

# Comparisons ----------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_proxy_equal tblStrings_pointErr
#' @export
#' @export vec_proxy_equal.tblStrings_pointErr
vec_proxy_equal.tblStrings_pointErr <- function(x, ...) {

  matrix(
    unlist(chr_to_dbl(vctrs::vec_data(x))),
    nrow = vctrs::vec_size(x),
    byrow = FALSE
  )

}

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_proxy_compare tblStrings_pointErr
#' @export
#' @export vec_proxy_compare.tblStrings_pointErr
vec_proxy_compare.tblStrings_pointErr <- function(x, ...) {

  # sort by percentage rather than counts
  .dat <- chr_to_dbl(vctrs::vec_data(x))
  .dat[[1]] / .dat[[2]]

}

# Math -----------------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_math tblStrings_pointErr
#' @export
#' @export vec_math.tblStrings_pointErr
vec_math.tblStrings_pointErr <- function(.fn, .x, ...) {

  # get character strings from x
  vctrs::vec_data(.x) %>%
    # convert to numeric data
    chr_to_dbl() %>%
    # apply operations to point/lower/upper, separately
    lapply(vctrs::vec_math_base, .fn = .fn, ...) %>%
    # convert back to number and percentage
    as_pointErr() %>%
    # restore the original attributes
    vctrs::vec_restore(to = .x)

}


# Arithmetic -----------------------------------------------------------------

#' @rdname obj_print_data.tblStrings_pointGap
#' @method vec_arith tblStrings_pointErr
#' @export
#' @export vec_arith.tblStrings_pointErr
vec_arith.tblStrings_pointErr <- function(op, x, y, ...) {
  UseMethod("vec_arith.tblStrings_pointErr", y)
}

#' @method vec_arith.tblStrings_pointErr default
#' @export
vec_arith.tblStrings_pointErr.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}


# Front-end ------------------------------------------------------------------

#' @rdname is_pointGap
#' @export
is_pointErr <- function(x) {
  inherits(x, "tblStrings_pointErr")
}
