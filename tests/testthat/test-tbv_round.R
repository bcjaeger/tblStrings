
test_that(
  "good inputs work",
  {

    # missing inputs get converted to characters
    expect_equal(tbv_round(x = NULL), "NA")
    expect_equal(tbv_round(x = NA_real_), "NA")

    # invalid types cause hard stop
    expect_error(tbv_round(x = NA_character_), "numeric")
    expect_error(tbv_round(x = factor(1)), "numeric")
    expect_error(tbv_round(x = Sys.Date()), "numeric")

    # integers get rounded like an integer
    expect_equal(tbv_round(1L), "1")

    # boundaries are handled corrctly
    expect_equal(tbv_round(x = 0.995), "1.00")
    expect_equal(tbv_round(x = 9.995), "10.0")
    expect_equal(tbv_round(x = 99.95), "100")

  }
)
