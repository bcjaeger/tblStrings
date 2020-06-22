


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

tbl_string <- function(...,
  .sep = '',
  .envir = parent.frame(),
  decimals_0_to_1    = 2,
  decimals_1_to_10   = 1,
  decimals_10_to_100 = 0,
  decimals_100_plus  = 0,
  big_mark = ",",
  .missing = '--'
){

  decimals <- c(2, 2, 1, 0)

  string <- Reduce(base::c, eval(substitute(alist(...)))) %>%
    paste(collapse = .sep)

  # objects inside of {}
  objects <- string %>%
    stringr::str_extract_all(pattern = "(?<=\\{).+?(?=\\})") %>%
    .[[1]] %>%
    unique()

  if(is.data.frame(.envir)) .envir <- list2env(.envir)

  .envir$..f <- function(x){
    if (is.numeric(x)){
      trimws(
        tblStrings::tbl_val(x,
          decimals_0_to_1    = decimals_0_to_1,
          decimals_1_to_10   = decimals_1_to_10,
          decimals_10_to_100 = decimals_10_to_100,
          decimals_100_plus  = decimals_100_plus,
          big_mark = big_mark,
          .missing = .missing
        )
      )
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




