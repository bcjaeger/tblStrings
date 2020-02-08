


new_rational <- function(n = integer(), d = integer()) {

  vctrs::vec_assert(n, ptype = integer())
  vctrs::vec_assert(d, ptype = integer())

  vctrs::new_rcrd(list(n = n, d = d), class = "vctrs_rational")

}

rational <- function(n, d) {

  c(n, d) %<-% vctrs::vec_cast_common(n, d, .to = integer())
  c(n, d) %<-% vctrs::vec_recycle_common(n, d)

  new_rational(n, d)

}

