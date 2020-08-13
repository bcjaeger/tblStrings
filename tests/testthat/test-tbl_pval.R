

test_that("rounding works", {

  pvalues <- c(0.00001, 0.003234, 0.044, 0.0458, 0.1234, 0.99899, 0.9995)

  correct_answer <- c("<.001", ".003", ".04", ".046", ".12", ".999", ">.999")

  # use explicit arguments in case defaults change
  tbl_pval_answer <- tbl_pval(x = pvalues,
                              decimals_outer = 3,
                              decimals_inner = 2,
                              boundary_lo = 0.01,
                              boundary_hi = 0.99,
                              thresh_lo = 0.001,
                              thresh_hi = 0.999,
                              miss_replace = '--',
                              drop_leading_zero = TRUE)

  expect_equal(correct_answer,
               tbl_pval_answer)

})

test_that('hard stops work', {

  expect_error(tbl_pval(x = 0), 'values in x')
  expect_error(tbl_pval(x = 1), 'values in x')

  # tests specifically for check_args

  expect_error(tbl_pval(x = 'a character'), 'has type <character>')

  expect_error(tbl_pval(x = 1/2, decimals_outer = 1/2), '>= 1')
  expect_error(tbl_pval(x = 1/2, decimals_outer = 3/2), 'integer valued')
  expect_error(tbl_pval(x = 1/2, miss_replace = 1L), '<integer>')

  expect_error(tbl_pval(x = not_defined), 'not found')

})
