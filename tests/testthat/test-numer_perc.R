
test_that(
  "vector behavior is correct",
  {

    counts <- c(1:10)
    denoms <- rep(20, 10)

    counts[2] <- NA
    denoms[2] <- NA

    counts[3] <- NA
    denoms[4] <- NA

    # Construction ----

    p <- numPer(counts, denoms, brac_left = '[[', brac_right = ']]')

    q <- numPer(counts, denoms, brac_left = '[', brac_right = ']')

    expect_error(numPer(-1, 1), 'must be positive')
    expect_error(numPer(1,0), 'zero values')

    expect_is(p, 'tblStrings_numPer')
    expect_true(is_numPer(p))


    # Formatting ----

    expect_equal(
      as.character(p),
      c(
        "1 [[5.00%]]",
        "NA [[NA%]]",
        "NA [[NA%]]",
        "4 [[NA%]]",
        "5 [[25.0%]]",
        "6 [[30.0%]]",
        "7 [[35.0%]]",
        "8 [[40.0%]]",
        "9 [[45.0%]]",
        "10 [[50.0%]]"
      )
    )

    # Casting ----

    expect_is(as_numPer(c(1,3)), 'tblStrings_numPer')

    mat = matrix(c(counts, denoms), ncol=2)
    dat = as.data.frame(mat)
    lst = list(counts, denoms)

    m = as_numPer(mat)
    d = as_numPer(dat)
    l = as_numPer(lst)

    # Coercion ----

    r <- c(m, d)
    expect_equal(r[1], r[11])
    expect_is(r, 'tblStrings_numPer')
    expect_is(c(p, numPer(0,1)), 'tblStrings_numPer')
    expect_error(c(p, '1'))
    expect_error(c(p, 1))
    expect_error(c(p, Sys.time()))

    # Comparisons and math ----

    csum = sum(counts, na.rm=TRUE)
    dsum = sum(denoms, na.rm=TRUE)

    chr_to_dbl(vctrs::vec_data(m))

    answer <- numPer(csum, dsum)

    expect_equal( sum(m, na.rm=TRUE), answer )
    expect_equal( sum(d, na.rm=TRUE), answer )
    expect_equal( sum(l, na.rm=TRUE), answer )

    expect_true(m[1] < m[5])

    # Front-end ----

    # 20 out of 100 - 20%
    np <- numPer(20, 100)

    expect_false(np_perc_gt(np, 1/5))
    expect_false(np_perc_lt(np, 1/5))
    expect_false(np_nums_gt(np, 20))
    expect_false(np_nums_lt(np, 20))

    expect_true(np_perc_gteq(np, 1/5))
    expect_true(np_perc_lteq(np, 1/5))
    expect_true(np_nums_gteq(np, 20))
    expect_true(np_nums_lteq(np, 20))

  }
)

