df <- tibble::tibble(
  stand     = paste0("st_", c(1, 1, 2, 2)),
  species   = paste0("sp_", c("a", "b", "a", "c")),
  abundance = c(1, 2, 3, 4))

test_that("df2table and table2df are inverse of each other", {
  back <- table2df(df2table(df))
  expect_equal(dplyr::arrange(back, stand, species),
               dplyr::arrange(df,   stand, species))
})

test_that("df2table fills absent species with zero", {
  tbl <- df2table(df)
  expect_equal(dim(tbl), c(2L, 3L))
  expect_equal(rownames(tbl), c("st_1", "st_2"))
  expect_equal(tbl["st_1", "sp_c"], 0)
  expect_equal(tbl["st_2", "sp_b"], 0)
})

test_that("df2table sums duplicated records", {
  expect_equal(df2table(dplyr::bind_rows(df, df)), df2table(df) * 2)
})

test_that("non-numeric abundance is replaced by 1 with a message", {
  chr <- dplyr::mutate(df, abundance = as.character(abundance))
  expect_message(tbl <- df2table(chr), "non-numeric")
  expect_true(all(as.matrix(tbl) %in% c(0, 1)))
})

test_that("table2df drops the zero cells", {
  tbl <- df2table(df)
  expect_equal(nrow(table2df(tbl)), nrow(df))
})

test_that("df2table and table2df accept other column names", {
  named <- dplyr::rename(df, st = stand, sp = species, ab = abundance)
  tbl <- df2table(named, st = "st", sp = "sp", ab = "ab")
  expect_equal(tbl, df2table(df))
  expect_equal(colnames(table2df(tbl, st = "st", sp = "sp", ab = "ab")),
               c("st", "sp", "ab"))
})

test_that("dist2df drops the zero diagonal and keeps both directions", {
  data(dune, package = "vegan")
  d   <- distance(dune, "bray")
  res <- dist2df(d)
  n   <- nrow(dune)
  expect_equal(colnames(res), c("plot_1", "plot_2", "dist"))
  expect_equal(nrow(res), n * (n - 1))
  expect_true(all(res$dist > 0))
  expect_true(all(res$plot_1 != res$plot_2))
})
