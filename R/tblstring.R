


#' Table strings
#'
#' @param ... Strings to format. Multiple inputs are concatenated
#'   together before formatting. Named arguments are not supported.
#' @param .sep Separator used to separate elements
#' @param format_fun a function that will be applied to all numeric
#'   (integer or double) values (see example). Default is [tbl_val].
#' @param format_args a names list. Names and values of the list are
#'   the names and values of input arguments for `format_fun`, respectively.
#' @param .envir Environment to evaluate each expression in.
#'
#' @return Character valued vector
#' @export
#'
#' @examples
#'
#' x <- runif(10)
#' y <- runif(10)
#'
#' tbl_string("{x} / {y} = {x/y}")
#'
#' custom <- function(a) format(a, digits=1, nsmall=1)
#' tbl_string("{x} / {y} = {x/y}", format_fun = custom)
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
#'
tbl_string <- function(..., .sep = '',
  format_fun = tblStrings::tbl_val,
  format_args = list(),
  .envir = parent.frame()
){

  string <- Reduce(base::c, eval(substitute(alist(...)))) %>%
    paste(collapse = .sep)

  first_arg_name <- names(as.list(args(format_fun)))[1]

  # objects inside of {}
  objects <- string %>%
    stringr::str_extract_all(pattern = "(?<=\\{).+?(?=\\})") %>%
    .[[1]] %>%
    unique()

  if(is.data.frame(.envir)) .envir <- list2env(.envir)

  .envir$..format_args <- format_args
  .envir$..first_arg_name <- first_arg_name
  .envir$..f <- function(x){

    if (is.numeric(x)){
      args <- ..format_args
      args[[..first_arg_name]] <- x
      trimws(do.call(format_fun, args = args))
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




