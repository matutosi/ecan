df <- tibble::tibble(
  x     = rep(letters[1:6], each  = 1),
  x_grp = rep(letters[1:3], each  = 2),
  y     = rep(LETTERS[1:3], each  = 2),
  y_grp = rep(LETTERS[1:3], each  = 2),
  z     = rep(LETTERS[1:3], each  = 2),
  z_grp = rep(LETTERS[1:3], times = 2))

test_that("unique_length counts uniques of each col and of their pairs", {
  expect_equal(unique_length(df, "x", "x_grp"), list(x = 6L, y = 3L, xy = 6L))
  expect_equal(unique_length(df, "y", "y_grp"), list(x = 3L, y = 3L, xy = 3L))
  expect_equal(unique_length(df, "z", "z_grp"), list(x = 3L, y = 3L, xy = 6L))
})

test_that("one-to-multi is neither one-to-one nor multi-to-multi", {
  expect_true (is_one2multi  (df, "x", "x_grp"))
  expect_false(is_one2one    (df, "x", "x_grp"))
  expect_false(is_multi2multi(df, "x", "x_grp"))
})

test_that("identical columns are one-to-one", {
  expect_true (is_one2one    (df, "y", "y_grp"))
  expect_false(is_one2multi  (df, "y", "y_grp"))
  expect_false(is_multi2multi(df, "y", "y_grp"))
})

test_that("crossed columns are multi-to-multi", {
  expect_true (is_multi2multi(df, "z", "z_grp"))
  expect_false(is_one2one    (df, "z", "z_grp"))
  expect_false(is_one2multi  (df, "z", "z_grp"))
})

test_that("cols_one2multi keeps only the one-to-multi columns", {
  # x is one-to-multi to z; x_grp, y and y_grp are one-to-one,
  # and z_grp is multi-to-multi, so all four are dropped
  expect_equal(cols_one2multi(df, "z"), c("z", "x"))
  expect_equal(cols_one2multi(df, "z", include_self = FALSE), "x")
  # the misspelt name of the argument is still accepted
  expect_equal(cols_one2multi(df, "z", inculde_self = FALSE), "x")
  expect_equal(select_one2multi(df, "z", inculde_self = FALSE),
               select_one2multi(df, "z", include_self = FALSE))
})

test_that("select_one2multi keeps distinct rows of the selected cols", {
  res <- select_one2multi(df, "z")
  expect_equal(colnames(res), cols_one2multi(df, "z"))
  expect_equal(nrow(res), nrow(dplyr::distinct(res)))
})
