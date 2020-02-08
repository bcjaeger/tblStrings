
new_percent <- function(x = double()) {
  vctrs::vec_assert(x, double())
  vctrs::new_vctr(x, class = "vctrs_percent")
}

percent <- function(x = double()) {
  x <- vctrs::vec_cast(x, double())
  new_percent(x)
}

methods::setOldClass(c("vctrs_percent", "vctrs_vctr"))

is_percent <- function(x) {
  inherits(x, "vctrs_percent")
}

format.vctrs_percent <- function(x, ...) {
  out <- tbv_round(vctrs::vec_data(x) * 100)
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

vec_ptype_abbr.vctrs_percent <- function(x, ...) {
  "prcnt"
}

vec_ptype2.vctrs_percent <- function(x, y, ...){
  UseMethod("vec_ptype2.vctrs_percent", y)
}

vec_ptype2.vctrs_percent.default <-
  function(x, y, ..., x_arg = "x", y_arg = "y") {
    vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  }

vec_ptype2.vctrs_percent.vctrs_percent <- function(x, y, ...) new_percent()

vec_ptype2.vctrs_percent.double <- function(x, y, ...) double()
vec_ptype2.double.vctrs_percent <- function(x, y, ...) double()

vec_cast.vctrs_percent <- function(x, to, ...){
  UseMethod("vec_cast.vctrs_percent")
}

vec_cast.vctrs_percent.default <- function(x, to, ...){
  vctrs::vec_default_cast(x, to)
}

vec_cast.vctrs_percent.double <- function(x, to, ...) percent(x)
vec_cast.double.vctrs_percent <- function(x, to, ...) vec_data(x)

as_percent <- function(x) {
  vctrs::vec_cast(x, new_percent())
}
