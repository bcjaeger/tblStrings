

#' P-values
#'
#' @param x a numeric vector, ideally one containing p-values.
#'
#' @param threshold a numeric value indicating the lowest number that
#'   should be displayed. Any numbers below this threshold will be
#'   written as < threshold in the output.
#'
#' @return a character vector containing formatted p-values.
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
#' tbv_pval(c(0.1, 0.01, 0.001, 0.0001))
#' tbv_pval(c(0.1, 0.01, 0.001, 0.0001), threshold = 0.01)
#'

tbv_pval <- function(x, threshold = 0.001){

  digit <- min(which(threshold * 10^seq(20) >= 1))

  if(any(x < 0) | any(x > 1))
    stop("p-values should be < 1 and > 0", call. = FALSE)

  output <- format(round(x, digit), nsmall = digit)
  output[x < threshold] <- paste0("<", as.character(threshold))
  output[x > 1-threshold] <- paste(">", as.character(1-threshold))
  output

}
