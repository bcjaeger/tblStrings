


#' Table strings
#'
#' @param ... Strings to format. Multiple inputs are concatenated
#'   together before formatting. Named arguments are not supported.
#' @param .sep Separator used to separate elements
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
#' tbl_string("{x}", "({100 * y}%)", .sep = ' ')
#'
#' df = data.frame(x = 1:10, y=1:10)
#'
#' tbl_string("{x} / {y} = {as.integer(x/y)}", .envir = df)
#'
#' with(df, tbl_string("{x} / {y} = {as.integer(x/y)}"))
#'
tbl_string <- function(..., .sep = '', .envir = parent.frame()){

  string <- Reduce(base::c, eval(substitute(alist(...)))) %>%
    paste(collapse = .sep)

  objects <- string %>%
    stringr::str_extract_all(pattern = "(?<=\\{).+?(?=\\})") %>%
    .[[1]] %>%
    unique()

  if(is.data.frame(.envir)) .envir <- list2env(.envir)

  .envir$.f <- function(x)
    if (is.numeric(x)) trimws(tblStrings::tbv_round(x))  else x

  p <- paste0("{", objects, "}")
  r <- paste0("{.f(", objects, ")}")

  for( i in seq_along(objects) ) string <- gsub(string,
    pattern = p[i], replacement = r[i], fixed = TRUE)

  as.character(glue::glue(string, .envir = .envir))

}




