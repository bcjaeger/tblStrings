


test_that("multiplication works", {

  df <- mtcars
  df$vs <- factor(df$vs, levels = c(0,1), labels = c("B","A"))
  df$gear <- factor(df$gear)

  tidy_m1 <- structure(
    list(
      term = c("(Intercept)", "vsA", "disp", "gear4","gear5"),
      estimate = c(
        27.3329144181748,
        1.84291811615615,
        -0.0355342671613221,
        0.0359609169903317,
        0.873480373418429
      ),
      std.error = c(
        3.36136228671206,
        1.85767534854888,
        0.00925757604667379,
        2.0245187081063,
        2.08202049470795
      ),
      statistic = c(
        8.13149910267802,
        0.992056075673153,
        -3.838398624237,
        0.0177626992758042,
        0.41953495445344
      ),
      p.value = c(
        9.81554416124451e-09,
        0.329978183173494,
        0.000677606381212004,
        0.985958774488068,
        0.678144644733879
      )
    ),
    row.names = c(NA, -5L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  tidy_m2 <-
    structure(
      list(
        term = c("(Intercept)", "mpg", "disp"),
        estimate = c(0.387913827646164,
          0.132317752336874, -0.0161086505494384),
        std.error = c(6.01360917480705,
          0.214480270899169, 0.010512853187549),
        statistic = c(0.0645059923866121,
          0.616922721060339, -1.53228150931631),
        p.value = c(0.948567335816632,
          0.537285699403462, 0.125452982013574)
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  tidy_m1_refd <- add_ref_rows(tidy_m1, data = df)
  tidy_m2_refd <- add_ref_rows(tidy_m2, data = df)

  expect_true(all(levels(df$vs) %in% tidy_m1_refd$level))
  expect_true(all(levels(df$gear) %in% tidy_m1_refd$level))

  expect_equal(names(tidy_m1_refd), names(tidy_m2_refd))


})
