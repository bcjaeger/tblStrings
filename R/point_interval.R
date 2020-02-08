
new_pintr <- function(
  point = numeric(),
  lower = numeric(),
  upper = numeric(),
  brac_left   = character(),
  brac_right  = character(),
  brac_middle = character()
) {

  vctrs::vec_assert(point, numeric())
  vctrs::vec_assert(lower, numeric())
  vctrs::vec_assert(upper, numeric())
  vctrs::vec_assert(brac_left,   character())
  vctrs::vec_assert(brac_right,  character())
  vctrs::vec_assert(brac_middle, character())

  vctrs::new_rcrd(
    list(
      'point'       = point,
      'lower'       = lower,
      'upper'       = upper,
      'brac_left'   = brac_left,
      'brac_right'  = brac_right,
      'brac_middle' = brac_middle
    ),
    class = "vctrs_pintr"
  )

}


methods::setOldClass(c("vctrs_pintr", "vctrs_vctr"))

format.vctrs_pintr <- function(x, ...) {

  .est <- tbv_round(vctrs::field(x, "point"))
  .lwr <- tbv_round(vctrs::field(x, "lower"))
  .upr <- tbv_round(vctrs::field(x, "upper"))

  paste0(
    .est, ' ', vctrs::field(x, 'brac_left'),
    .lwr, vctrs::field(x, 'brac_middle'),
    .upr, vctrs::field(x, 'brac_right')
  )
}

obj_print_data.vctrs_pintr <- function(x) {
  cat(format(x), sep = "\n")
}

vec_ptype_abbr.vctrs_pintr <- function(x, ...) {
  "pintr"
}

validate_pintr <- function(point, lower, upper) {

  stopifnot(
    vctrs::vec_size(point) ==  vctrs::vec_size(lower),
    vctrs::vec_size(point) ==  vctrs::vec_size(upper)
  )

  if(any(lower > point)){

    stop("lower values must be less than corresponding point values",
      call. = FALSE)

  }

  if(any(upper < point)){

    stop("upper values must be greater than corresponding point values",
      call. = FALSE)

  }

}

pintr <- function(
  point = 0,
  lower = 0,
  upper = 0,
  brac_left = '(',
  brac_right = ')',
  brac_middle = ', '
) {

  validate_pintr(point = point, lower = lower, upper = upper)

  point <- vctrs::vec_cast(point, double())
  lower <- vctrs::vec_cast(lower, double())
  upper <- vctrs::vec_cast(upper, double())

  if(vctrs::vec_size(brac_left) == 1){
    brac_left <- rep(brac_left, vctrs::vec_size(point))
  }

  if(vctrs::vec_size(brac_right) == 1){
    brac_right <- rep(brac_right, vctrs::vec_size(point))
  }

  if(vctrs::vec_size(brac_middle) == 1){
    brac_middle <- rep(brac_middle, vctrs::vec_size(point))
  }

  new_pintr(
    point = point,
    lower = lower,
    upper = upper,
    brac_left   = brac_left,
    brac_right  = brac_right,
    brac_middle = brac_middle
  )

}

vec_ptype2.vctrs_pintr <- function(x, y, ...) {
  UseMethod("vec_ptype2.vctrs_pintr", y)
}
vec_ptype2.vctrs_pintr.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

vec_ptype2.vctrs_pintr.vctrs_pintr <- function(x, y, ...) new_pintr()
vec_ptype2.vctrs_pintr.double <- function(x, y, ...) new_pintr()
vec_ptype2.double.vctrs_pintr <- function(x, y, ...) new_pintr()
vec_ptype2.vctrs_pintr.integer <- function(x, y, ...) new_pintr()
vec_ptype2.integer.vctrs_pintr <- function(x, y, ...) new_pintr()

vec_cast.vctrs_pintr <- function(x, to, ...){
  UseMethod("vec_cast.vctrs_pintr")
}
vec_cast.vctrs_pintr.default <- function(x, to, ...){
  vctrs::vec_default_cast(x, to)
}

vec_cast.vctrs_pintr.vctrs_pintr <- function(x, to, ...) x
vec_cast.vctrs_pintr.double <- function(x, to, ...) pintr(x, x, x)
vec_cast.double.vctrs_pintr <- function(x, to, ...) vctrs::vec_data(x)$point

vec_cast.vctrs_pintr.integer <- function(x, to, ...) pintr(x, x, x)
vec_cast.integer.vctrs_pintr <- function(x, to, ...)
  as.integer(vctrs::vec_data(x)$point)

get_points <- function(x) vctrs::vec_data(x)$point
get_lowers <- function(x) vctrs::vec_data(x)$lower
get_uppers <- function(x) vctrs::vec_data(x)$upper

set_points <- function(x, values) {

  vd <- vctrs::vec_data(x)

  pintr(
    point = values,
    lower = vd$lower,
    upper = vd$upper,
    brac_left = vd$brac_left,
    brac_right = vd$brac_right,
    brac_middle = vd$brac_middle
  )


}

set_lowers <- function(x, values) {

  vd <- vctrs::vec_data(x)

  pintr(
    point = vd$point,
    lower = values,
    upper = vd$upper,
    brac_left = vd$brac_left,
    brac_right = vd$brac_right,
    brac_middle = vd$brac_middle
  )


}

set_uppers <- function(x, values) {

  vd <- vctrs::vec_data(x)

  pintr(
    point = vd$point,
    lower = vd$lower,
    upper = values,
    brac_left = vd$brac_left,
    brac_right = vd$brac_right,
    brac_middle = vd$brac_middle
  )


}

as_pintr <- function(x) {
  vctrs::vec_cast(x, new_pintr())
}

vec_math.vctrs_pintr <- function(.fn, .x, ...) {

  vd <- vctrs::vec_data(.x)

  pintr(
    point = vctrs::vec_math_base(.fn, vd$point),
    lower = vctrs::vec_math_base(.fn, vd$lower),
    upper = vctrs::vec_math_base(.fn, vd$upper),
    brac_left   = vd$brac_left,
    brac_right  = vd$brac_right,
    brac_middle = vd$brac_middle
  )

}

vec_proxy_compare.vctrs_pintr <- function(x, ...) {

  x_raw <- vctrs::vec_data(x)

  data.frame(
    point = x_raw$point,
    lower = x_raw$lower,
    upper = x_raw$upper
  )

}

vec_arith.vctrs_pintr <- function(op, x, y, ...) {
  UseMethod("vec_arith.vctrs_meter", y)
}

vec_arith.vctrs_pintr.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

