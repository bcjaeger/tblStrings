
test_that(
  "good inputs work",
  {

    # missing inputs get converted to characters
    expect_error(tbl_val(x = NULL), "empty")
    expect_equal(tbl_val(x = NA_real_, miss_replace = '---'), "---")

    # invalid types cause hard stop
    expect_error(tbl_val(x = NA_character_), "numeric")
    expect_error(tbl_val(x = factor(1)), "numeric")
    expect_error(tbl_val(x = Sys.Date()), "numeric")

    # integers get rounded like an integer
    expect_equal(tbl_val(1L), "1")

    # boundaries are handled correctly
    expect_equal(
      tbl_val(x = 0.995, decimals = c(2,1), breaks = c(1,10)), "1.0"
    )

    expect_equal(
      tbl_val(x = 11.995, decimals = c(2, 1), breaks = c(12, Inf)), "12.0"
    )

    expect_equal(tbl_val(x = 99.95), "100")

  }
)
