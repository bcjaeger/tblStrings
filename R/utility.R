
.round = function(x, digits = 0) {

  # Why do we need this?
  # default behavior of the round() function is not ideal for tables
  # (inconsistency with 0.5 sometimes rounding to 0 instead of 1)

  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg

}

safe_nsmall <- function(x){

  # why do we need this?
  # prevents hard stops when using format

  x <- max(x, 0)
  x <- min(x, 20)
  x
}

find_smallest_10 <- function(x, y = 1e-10){

  if(x < 1e-10) stop(
    'the number you are attempting to round is too small',
    call. = FALSE
  )

  if(x == Inf) return(Inf)

  if (x < y) { return(y/10) } else { find_smallest_10(x, y*10) }

}

duplicate_last <- function(x) c(x, x[length(x)])

is_empty <- function (x) length(x) == 0
