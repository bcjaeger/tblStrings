
test_that(
  "vector behavior is correct",
  {

    points <- c(1:10)
    errors <- rep(20, 10)

    points[2] <- NA
    errors[2] <- NA

    points[3] <- NA
    errors[4] <- NA

    # Construction ----

    p <- pointErr(points, errors, style = 'brac',
      brac_left = '[[', brac_right = ']]')

    q <- pointErr(points, errors)

    expect_error(pointErr(1, -1), 'must be >0')


    expect_is(p, 'tblStrings_pointErr')
    expect_true(is_pointErr(p))


    # Formatting ----

    expect_equal(
      as.character(p),
      c(
        "1.00 [[20.0]]",
        "NA [[NA]]",
        "NA [[20.0]]",
        "4.00 [[NA]]",
        "5.00 [[20.0]]",
        "6.00 [[20.0]]",
        "7.00 [[20.0]]",
        "8.00 [[20.0]]",
        "9.00 [[20.0]]",
        "10.0 [[20.0]]"
      )
    )

    # Casting ----

    expect_is(as_pointErr(c(1,3)), 'tblStrings_pointErr')

    mat = matrix(c(points, errors), ncol=2)
    dat = as.data.frame(mat)
    lst = list(points, errors)

    m = as_pointErr(mat)
    d = as_pointErr(dat)
    l = as_pointErr(lst)

    # Coercion ----

    r <- c(m, d)
    expect_equal(r[1], r[11])
    expect_is(r, 'tblStrings_pointErr')
    expect_is(c(p, pointErr(0,1)), 'tblStrings_pointErr')
    expect_error(c(p, '1'))
    expect_error(c(p, 1))
    expect_error(c(p, Sys.time()))

    # Comparisons and math ----

    csum = sum(points, na.rm=TRUE)
    dsum = sum(errors, na.rm=TRUE)

    chr_to_dbl(vctrs::vec_data(m))

    answer <- pointErr(csum, dsum)

    expect_equal( sum(m, na.rm=TRUE), answer )
    expect_equal( sum(d, na.rm=TRUE), answer )
    expect_equal( sum(l, na.rm=TRUE), answer )

    expect_true(m[1] < m[5])

    # scaling

    expect_equal(100 * p, p * 100)

    # Front-end ----



  }
)

