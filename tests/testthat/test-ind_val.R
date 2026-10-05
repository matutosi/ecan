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

test_that("the seed changes p.value but not ind.val", {
  set.seed(1)
  res_1 <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  set.seed(1)
  same <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  set.seed(2)
  res_2 <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  expect_equal(res_1, same)              # the same seed gives the same result
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

test_that("the species are ordered by ind.val within a group", {
  set.seed(1)
  res <- suppressMessages(
    ind_val(df, abundance = "cover", group = "Moisture"))
  # the species of a group are given in decreasing order of ind.val
  by_group <- split(res$ind.val, res$Moisture)
  for(v in by_group) expect_false(is.unsorted(rev(v)))
  # every group is kept together
  gr <- as.character(res$Moisture)
  expect_equal(length(rle(gr)$values), length(unique(gr)))
})

test_that("the groups are ordered by the levels of a factor", {
  rev_lev <- rev(levels(dune.env$Moisture))
  df_rev <- dplyr::mutate(df, Moisture = factor(Moisture, levels = rev_lev))
  set.seed(1)
  res <- suppressMessages(
    ind_val(df_rev, abundance = "cover", group = "Moisture"))
  expect_equal(as.character(unique(res$Moisture)), rev_lev)
})

test_that("the groups of a character are ordered by sort()", {
  # put the stands of the last group first, so that the order in which
  # the groups appear in df differs from the sorted one
  df_chr <- dplyr::mutate(df, Moisture = as.character(Moisture))
  df_chr <- dplyr::arrange(df_chr, dplyr::desc(Moisture))
  set.seed(1)
  res <- suppressMessages(
    ind_val(df_chr, abundance = "cover", group = "Moisture"))
  expect_equal(unique(res$Moisture), sort(unique(df_chr$Moisture)))
})

test_that("the order of the groups does not change ind.val of a species", {
  rev_lev <- rev(levels(dune.env$Moisture))
  df_rev <- dplyr::mutate(df, Moisture = factor(Moisture, levels = rev_lev))
  set.seed(1)
  res_1 <- suppressMessages(
    ind_val(df,     abundance = "cover", group = "Moisture"))
  set.seed(1)
  res_2 <- suppressMessages(
    ind_val(df_rev, abundance = "cover", group = "Moisture"))
  res_1 <- res_1[order(res_1$species), ]
  res_2 <- res_2[order(res_2$species), ]
  expect_equal(as.character(res_1$Moisture), as.character(res_2$Moisture))
  expect_equal(res_1$ind.val, res_2$ind.val)
})
