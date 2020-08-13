


# tbl_esp <- function(estimate,
#                     precision,
#                     bracket_left = '(',
#                     bracket_right = ')',
#                     miss_replace = '--',
#                     round_half_to_even = FALSE,
#                     big_mark = ',',
#                     big_interval = 3L,
#                     small_mark = '',
#                     small_interval = 5L,
#                     decimal_mark = getOption('OutDec'),
#                     zero_print = NULL,
#                     trim = TRUE) {
#
#   out <- rep(miss_replace, length(estimate))
#
#   for(i in seq_along(estimate)) {
#
#     browser()
#
#     first_sig_dig <- find_10s_digit(precision[i])
#
#     if(first_sig_dig < 0){
#       estimate_decimal  <- first_sig_dig + 1
#       precision_decimal <- first_sig_dig + 2
#     } else {
#       estimate_decimal <- find_10s_digit(estimate[i]) + first_sig_dig
#       precision_decimal <- find_10s_digit() + first_sig_dig
#     }
#
#
#
#
#     .estimate <- tbl_val(estimate[i],
#                          decimals = estimate_decimal,
#                          round_half_to_even = round_half_to_even,
#                          miss_replace = miss_replace,
#                          big_mark = big_mark,
#                          big_interval = big_interval,
#                          small_mark = small_mark,
#                          small_interval = small_interval,
#                          decimal_mark = decimal_mark,
#                          zero_print = zero_print,
#                          trim = trim)
#
#     .precision <- tbl_val(precision[i],
#                           decimals = precision_decimal,
#                           round_half_to_even = round_half_to_even,
#                           miss_replace = miss_replace,
#                           big_mark = big_mark,
#                           big_interval = big_interval,
#                           small_mark = small_mark,
#                           small_interval = small_interval,
#                           decimal_mark = decimal_mark,
#                           zero_print = zero_print,
#                           trim = trim)
#
#     out[i] <- tbl_string(
#       "{.estimate} {bracket_left}{.precision}{bracket_right}"
#     )
#
#   }
#
#   out
#
# }
#
# find_10s_digit <- function(x){
#
#   if(x == 1) return(-1)
#   if(x < 1) return(find_10s_digit_lt1(x))
#   if(x > 1) return(find_10s_digit_gt1(x))
#
# }
#
# find_10s_digit_lt1 <- function(x){
#   pow <- 0
#   while((x * 10^pow) %% 1 != 0){ pow <- pow + 1 }
#   pow
# }
#
# find_10s_digit_gt1 <- function(x){
#   pow <- 0
#   while((x * 10^pow) > 1){ pow <- pow - 1 }
#   pow
# }
