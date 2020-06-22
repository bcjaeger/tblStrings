
test_that(
  "good inputs work",
  {

    # missing inputs get converted to characters
    expect_equal(tbl_val(x = NULL), "--")
    expect_equal(tbl_val(x = NA_real_, .missing = '---'), "---")

    # invalid types cause hard stop
    expect_error(tbl_val(x = NA_character_), "numeric")
    expect_error(tbl_val(x = factor(1)), "numeric")
    expect_error(tbl_val(x = Sys.Date()), "numeric")

    # integers get rounded like an integer
    expect_equal(tbl_val(1L), "1")

    # boundaries are handled corrctly
    expect_equal(tbl_val(x = 0.995, decimals_1_to_10 = 2), "1.00")
    expect_equal(tbl_val(x = 9.995, decimals_10_to_100 = 1), "10.0")
    expect_equal(tbl_val(x = 99.95, decimals_100_plus = 0), "100")

  }
)
