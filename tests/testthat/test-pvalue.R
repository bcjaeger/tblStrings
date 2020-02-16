
test_that("vector behavior works",
  {

    x <- 0:10 / 10

    pv <- pval(x)

    expected_print <-
      c(
        "<0.001",
        "0.100",
        "0.200",
        "0.300",
        "0.400",
        "0.500",
        "0.600",
        "0.700",
        "0.800",
        "0.900",
        ">0.999"
      )


    expect_true(is_pval(pv))
    expect_true(pv[1] < 0.1)
    expect_true(pv[1] == 0)
    expect_error(mean(pv), regexp = 'not allowed')
    expect_error(pv[1] + pv[2])
    expect_error(pv[1] + 1/2)
    expect_error(-pv)

    expect_equal(sort(pv, decreasing = TRUE), rev(pv))
    expect_equal(as.character(pv), expected_print)
    expect_equal(as.numeric(pv), x)

    expect_is( c(pv, 1), 'numeric')
    expect_is( c(pv, "1"), 'character')
    expect_is( c(1, pv), 'numeric')
    expect_is( c("1", pv), 'character')
    expect_is( c(pval(1), pv), 'tblStrings_pval' )

  }
)
