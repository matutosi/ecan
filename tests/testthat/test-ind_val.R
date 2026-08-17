data(dune,     package = "vegan")
data(dune.env, package = "vegan")

df <- suppressMessages(
  dplyr::left_join(
    table2df(dune, st = "stand", sp = "species", ab = "cover"),
    tibble::rownames_to_column(dune.env, "stand")))

test_that("ind_val needs a group", {
  expect_error(ind_val(df, abundance = "cover"), 'Needs "group" input')
})

test_that("ind_val returns one row per species", {
  set.seed(1)
  res <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), ncol(dune))
  expect_equal(colnames(res), c("Moisture", "species", "ind.val", "p.value"))
  expect_setequal(res$species, colnames(dune))
  expect_true(all(res$ind.val >= 0 & res$ind.val <= 1))
  expect_true(all(res$p.value >= 0 & res$p.value <= 1))
})

test_that("the same seed gives the same p values", {
  set.seed(1)
  res_1 <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  set.seed(1)
  res_2 <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  expect_equal(res_1, res_2)
})

test_that("ind.val does not depend on the seed, only p.value does", {
  set.seed(1)
  res_1 <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  set.seed(2)
  res_2 <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  expect_equal(res_1$ind.val, res_2$ind.val)
})

test_that("row_data returns the raw result of labdsv::indval", {
  set.seed(1)
  res <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture", row_data = TRUE))
  expect_s3_class(res, "indval")
  expect_true(all(c("maxcls", "indcls", "pval") %in% names(res)))
  expect_length(res$pval, ncol(dune))
})

test_that("columns can be given by position", {
  set.seed(1)
  res_named <- suppressMessages(
    ind_val(df, stand = "stand", species = "species", abundance = "cover",
            group = "Moisture"))
  set.seed(1)
  res_default <- suppressMessages(ind_val(df, group = "Moisture"))
  expect_equal(res_named, res_default)
})
