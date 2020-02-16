
# test_that("vector behavior works", {
#
#   numerics <- 0:10/10
#
#   prc <- percent(numerics)
#
#   expected_print <-
#     c(
#       "0.00%",
#       "10.0%",
#       "20.0%",
#       "30.0%",
#       "40.0%",
#       "50.0%",
#       "60.0%",
#       "70.0%",
#       "80.0%",
#       "90.0%",
#       "100%"
#     )
#
#   expect_error(percent(-1), regexp = '-1 at indices 1')
#   expect_error(percent(2), regexp = '2 at indices 1')
#
#   expect_true(is_percent(prc))
#   expect_true(prc[1] < 0.1)
#   expect_true(prc[1] == 0)
#
#   expect_equal(sort(prc, decreasing = TRUE), rev(prc))
#
#   expect_equal(as.character(prc), expected_print)
#   expect_equal(as.numeric(prc), numerics)
#   expect_equal(as.character(prc[1] + 1/2), "50.0%")
#   expect_equal(as.character(prc[11] / 10), "10.0%")
#   expect_equal(prc[1]+prc[2], prc[2])
#   expect_equal(+prc, prc)
#
#   expect_error(-prc, 'Some x values are less than 0:')
#
#   expect_is( c(prc, 1), 'numeric')
#   expect_is( c(prc, "1"), 'character')
#   expect_is( c(1, prc), 'numeric')
#   expect_is( c("1", prc), 'character')
#   expect_is( c(percent(1), prc), 'tblStrings_percent' )
#
#
# })
