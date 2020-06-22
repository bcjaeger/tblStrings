

test_that("basic input works", {

  x <- 0.25
  y <- 0.1

  expect_equal(
    tbl_string("{x} / {y} = {x/y}"),
    "0.25 / 0.10 = 2.5"
  )

  tbl_string("{x}", "({100 * y}%)", .sep = ' ')

  expect_equal(
    tbl_string("{x}", "({100 * y}%)", .sep = ' '),
    "0.25 (10%)"
  )

  df = data.frame(x = 1:10, y=1:10)

  expect_equal(
    tbl_string("{x} / {y} = {as.integer(x/y)}", .envir = df),
    c(
      "1 / 1 = 1",
      "2 / 2 = 1",
      "3 / 3 = 1",
      "4 / 4 = 1",
      "5 / 5 = 1",
      "6 / 6 = 1",
      "7 / 7 = 1",
      "8 / 8 = 1",
      "9 / 9 = 1",
      "10 / 10 = 1"
    )
  )

  expect_equal(
    tbl_string("{x} / {y} = {as.integer(x/y)}"),
    "0.25 / 0.10 = 2"
  )

  expect_equal(
    with(df, tbl_string("{x} / {y} = {as.integer(x/y)}")),
    c(
      "1 / 1 = 1",
      "2 / 2 = 1",
      "3 / 3 = 1",
      "4 / 4 = 1",
      "5 / 5 = 1",
      "6 / 6 = 1",
      "7 / 7 = 1",
      "8 / 8 = 1",
      "9 / 9 = 1",
      "10 / 10 = 1"
    )
  )

})
