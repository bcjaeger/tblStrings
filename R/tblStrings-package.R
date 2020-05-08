

#' @keywords internal
"_PACKAGE"

if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      ".",
      ".x",
      "level",
      "value",
      "col_keys",
      "term",
      "variable",
      "num_in_model"
    )
  )

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom rlang %||% is_empty
#' @importFrom magrittr %<>% %>%
#' @importFrom tibble tibble
#' @importFrom vctrs
#'   vec_ptype_abbr
#'   vec_cast
#'   vec_cast.character
#'   vec_cast.double
#'   vec_cast.integer
#'   vec_cast.data.frame
#'   vec_arith
#'   vec_math
#'   vec_proxy_compare
#'   vec_proxy_equal
#'   vec_ptype2
#'   vec_ptype2.character
#'   vec_ptype2.double
#'   vec_ptype2.integer
#'   obj_print_data
#'
## usethis namespace: end
NULL
