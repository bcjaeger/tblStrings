

#' Confidence intervals
#'
#' @param est numeric vector of point estimates.
#' @param lwr numeric vector of lower interval estimates.
#' @param upr numeric vector of upper interval estimates.
#' @param fun a function that will be applied to all numeric values before
#'   they are multiplied by `mult_by`.
#'   inputs before they are passed into [tbv_round], but after `fun()`.
#' @param brac_left a character value that will close the left side of
#'   the interval.
#' @param brac_mid a character value that will separate the lower and
#'  upper portion of the intervals
#' @param brac_right a character value that will close the right side of
#'   the interval.
#'
#' @return a character vector with each value comprising estimate and
#'   interval values.
#'
#' @export
#'
#' @examples
#'
#' tbv_ci(1.111, 0.555, 1.555)
#'

tbv_ci <- function(
  est,
  lwr,
  upr,
  fun = function(x) x,
  brac_left = '(',
  brac_mid = ', ',
  brac_right = ')'
) {

  if(any(lwr > est)){

    check_index <- which(lwr > est)
    n_print <- min(5, length(check_index))

    error_msg <- if(n_print < length(check_index)){
      paste(
        "found some lower values > estimates at these indices:",
        paste(check_index[seq(n_print)], collapse = ', '),
        'and others'
      )
    } else {
      paste(
        "found some lower values > estimates at these indices:",
        list_things(check_index)
      )
    }

    stop(error_msg, call. = FALSE)

  }

  if(any(upr < est)){

    check_index <- which(upr < est)
    n_print <- min(5, length(check_index))

    error_msg <- if(n_print < length(check_index)){
      paste(
        "found some upper values < estimates at these indices:",
        paste(check_index[seq(n_print)], collapse = ', '),
        'and others'
      )
    } else {
      paste(
        "found some upper values < estimates at these indices:",
        list_things(check_index)
      )
    }

    stop(error_msg, call. = FALSE)

  }

  .est <- tbv_round(fun(est))
  .lwr <- tbv_round(fun(lwr))
  .upr <- tbv_round(fun(upr))

  paste0(
    .est, ' ', brac_left, .lwr, brac_mid, .upr, brac_right
  )

}


