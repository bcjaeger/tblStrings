list_things <- function(things){

  glue::glue_collapse(things, sep = ', ', last = ' and ')

}


brac_left    <- function(x) attr(x, 'brac_left')
brac_right   <- function(x) attr(x, 'brac_right')
brac_sep     <- function(x) attr(x, 'brac_sep')
brac_middle  <- function(x) attr(x, 'brac_middle')
max_decimals <- function(x) attr(x, 'max_decimals')
big_mark     <- function(x) attr(x, 'big_mark')
print_thresh <- function(x) attr(x, 'print_thresh')
column_label <- function(x) attr(x, 'label')
brac_label   <- function(x) attr(x, 'brac_label')
gap_label    <- function(x) attr(x, 'gap_label')
point_label  <- function(x) attr(x, 'point_label')
error_label  <- function(x) attr(x, 'error_label')
ref_value    <- function(x) attr(x, 'ref_value')
ref_label    <- function(x) attr(x, 'ref_label')
show_percent <- function(x) attr(x, 'show_percent')
style        <- function(x) attr(x, 'style')

validate_proportion <- function(x, label = 'x'){

  x_lt_0 <- x < 0
  x_gt_1 <- x > 1

  if(any(x_lt_0)){

    show <- which(x_lt_0)
    if(length(show) > 5) show = show[1:5]

    msg = paste("Some", label, "values are less than 0: e.g.",
      list_things(x[show]), 'at indices', list_things(show))

    stop(msg, call. = FALSE)

  }

  if(any(x_gt_1)){

    show <- which(x_gt_1)
    if(length(show) > 5) show = show[1:5]

    msg = paste("Some", label, "values are greater than 1: e.g.",
      list_things(x[show]), 'at indices', list_things(show))

    stop(msg, call. = FALSE)

  }

}


all_equal <- function(x) diff(range(x)) < .Machine$double.eps^0.5
quietly_numeric <- function(x) suppressWarnings(as.numeric(x))

mat_to_list <- function(x){
  lst <- vector(mode = 'list', length = ncol(x))
  for(i in seq_along(lst)) lst[[i]] <- x[, i]
  lst
}

dbl_to_chr <- function(numerics){

  # check lengths
  if(!all_equal(vapply(numerics, length, 1L)))
    stop("all arguments must be the same length", call. = FALSE)

  # concatenate vectors
  do.call(what = base::c, args = numerics) %>%
    # form matrix (1 column for each vector)
    matrix(byrow = FALSE, ncol = length(numerics)) %>%
    # collapse by row using '_' pattern
    apply(1L, paste, collapse = '_')

}

chr_to_dbl <- function(string){

  vctrs::vec_assert(string, character())

  # split vector of strings up into pieces separated by _
  strsplit(string, split = '_') %>%
    # turn each piece into a numeric value
    lapply(quietly_numeric) %>%
    # bind numerics to create a matrix
    do.call(rbind, args = .) %>%
    # turn matrix columns into a list
    mat_to_list()

}
