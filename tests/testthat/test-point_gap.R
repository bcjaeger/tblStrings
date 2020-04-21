

# test_that(
#   "data frame behavior is correct",
#   {
#
#     df <- data.frame(
#       x = 1:10/10,
#       y = 0:9 /10,
#       z = 2:11/10
#     )
#
#     df$pg = with(df, pointGap(x, y, z))
#
#   }
# )

test_that(
  "vector behavior is correct",
  {

    x = 1:10/10
    y = 0:9 /10
    z = 2:11/10

    x[2] <- NA
    y[2] <- NA
    z[2] <- NA

    x[3] <- NA
    y[4] <- NA
    z[5] <- NA

    # Construction ----

    p <- pointGap(
      point = x,
      lower = y,
      upper = z,
      brac_left = '[[',
      brac_right = ']]'
    )

    q <- pointGap(
      point = x,
      lower = y,
      upper = z,
      brac_left = '[',
      brac_right = ']',
      ref_value = 1
    )

    expect_error(pointGap(0, 1, 0), 'lower values')
    expect_error(pointGap(0, -1, -1/2), 'upper values')
    expect_is(p, 'tblStrings_pointGap')
    expect_true(is_pointGap(p))


    # Formatting ----

    expect_equal(
      as.character(p),
      c(
        "0.10 [[0.00, 0.20]]",
        "NA [[NA, NA]]",
        "NA [[0.20, 0.40]]",
        "0.40 [[NA, 0.50]]",
        "0.50 [[0.40, NA]]",
        "0.60 [[0.50, 0.70]]",
        "0.70 [[0.60, 0.80]]",
        "0.80 [[0.70, 0.90]]",
        "0.90 [[0.80, 1.00]]",
        "1.00 [[0.90, 1.10]]"
      )
    )

    expect_equal(as.character(pointGap(0, 0, 0)), '0 (ref)')
    expect_equal(as.character(exp(pointGap(0, 0, 0))), '1 (ref)')

    # Casting ----

    expect_is(as_pointGap(c(1,0,3)), 'tblStrings_pointGap')

    mat = matrix(c(x,y,z), ncol=3)
    dat = as.data.frame(mat)
    lst = list(x,y,z)

    m = as_pointGap(mat)
    d = as_pointGap(dat)
    l = as_pointGap(lst)

    expect_true( sum(m == p, na.rm=TRUE) == 8 )
    expect_true( sum(d == p, na.rm=TRUE) == 8 )
    expect_true( sum(l == p, na.rm=TRUE) == 8 )

    # Coercion ----

    r <- c(p, q)
    expect_equal(r[1], r[11])
    expect_is(r, 'tblStrings_pointGap')
    expect_is(c(p, pointGap(1,0,2)), 'tblStrings_pointGap')
    expect_error(c(p, '1'))
    expect_error(c(p, 1))
    expect_error(c(p, Sys.time()))

    # Comparisons ----

    expect_true(p[1] < p[4])
    expect_true(is.na(p[1] < p[2]))

    # Arithmetic ----

    expect_error(p[1] + p[2], class = 'vctrs_error_incompatible_op')
    expect_equal(as_pointGap(exp(mat)), pointGap(exp(x), exp(y), exp(z)))

    # scaling
    expect_equal(100 * p, p *100)

    # Front-end ----

    # coverage

    expect_equal(
      pg_covers(p[1:4], value = 0, strict_coverage = FALSE),
      c(TRUE, NA, FALSE, NA)
    )

    expect_equal(
      pg_covers(p[1:4], value = 0, strict_coverage = TRUE),
      c(FALSE, NA, FALSE, NA)
    )

    # correct inverse

    expect_equal(
      pg_covers(p[1:5], value = 0),
      !pg_omits(p[1:5], value = 0)
    )

    # flipping

    expect_equal(
      as.character(pg_flip(p[1])),
      '-0.10 [[-0.20, 0.00]]'
    )

    expect_error(pg_covers(1, value = 1), 'has type')

    expect_error(pg_flip(1), 'has type')

  }
)

