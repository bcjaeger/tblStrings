


#' Table strings
#'
#' @param ... Strings to format. Multiple inputs are concatenated
#'   together before formatting. Named arguments are not supported.
#'
#' @param .sep Separator used to separate elements
#'
#' @param .envir environment to evaluate each expression in.
#'
#' @inheritParams tbl_val
#'
#' @return a character vector
#' @export
#'
#' @examples
#'
#' x <- runif(10)
#' y <- runif(10)
#'
#' tbl_string("{x} / {y} = {x/y}")
#'
#' tbl_string("{x}", "({100 * y}%)", .sep = ' ')
#'
#' df = data.frame(x = 1:10, y=1:10)
#'
#' tbl_string("{x} / {y} = {as.integer(x/y)}", .envir = df)
#' tbl_string("{x} / {y} = {as.integer(x/y)}", .envir = NULL)
#'
#' with(df, tbl_string("{x} / {y} = {as.integer(x/y)}"))
#'

tbl_string <- function(
  ...,
  .sep = '',
  .envir = parent.frame(),
  round_half_to_even = FALSE,
  breaks = c(1, 10, Inf),
  decimals = c(2, 1, 0),
  miss_replace = '--',
  big_mark = ',',
  big_interval = 3L,
  small_mark = '',
  small_interval = 5L,
  decimal_mark = getOption('OutDec'),
  zero_print = NULL,
  trim = TRUE
){

  .dots <- substitute(alist(...))

  string <- Reduce(base::c, eval(.dots))
  string <- paste(string, collapse = .sep)

  # objects inside of {}
  pattern <- "(?<=\\{).+?(?=\\})"
  objects <- stringi::stri_extract_all_regex(string, pattern)
  objects <- objects[[1]]
  objects <- unique(objects)

  if(is.data.frame(.envir)) .envir <- list2env(.envir)

  .envir$..f <- function(x){

    if (is.numeric(x)){
      trimws(tbl_val(x,
        breaks = breaks,
        decimals = decimals,
        round_half_to_even = round_half_to_even,
        miss_replace = miss_replace,
        big_mark = big_mark,
        big_interval = big_interval,
        small_mark = small_mark,
        small_interval = small_interval,
        decimal_mark = decimal_mark,
        zero_print = zero_print,
        trim = trim
      ))
    } else {
      x
    }

  }

  # _p_attern
  p <- paste0("{", objects, "}")

  # _r_eplacement
  r <- paste0("{..f(", objects, ")}")

  for( i in seq_along(objects) ) string <- gsub(string,
    pattern = p[i], replacement = r[i], fixed = TRUE)

  # make output a character
  # (avoid potential issues with attributes of glue objects)
  as.character(glue::glue(string, .envir = .envir))


}




