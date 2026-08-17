data(dune, package = "vegan")

test_that("pca results match labdsv::pca", {
  res_ord <- ordination(dune, o_method = "pca")
  res_pca <- labdsv::pca(dune, dim = 10)
  expect_equal(res_ord$st_scores, res_pca$scores)
  expect_equal(res_ord$sp_scores, res_pca$loadings)
  expect_equal(res_ord$eig_val,   res_pca$sdev)
})

test_that("methods without a distance report none, even when one is given", {
  for(o_method in c("pca", "ca", "dca")){
    res <- ordination(dune, o_method = o_method, d_method = "bray")
    expect_null(res$distance_method)
    expect_null(res$d_method)
  }
})

test_that("bray is the default distance method", {
  res_default <- ordination(dune, o_method = "pcoa")
  res_bray    <- ordination(dune, o_method = "pcoa", d_method = "bray")
  expect_equal(res_default$distance_method, "bray")
  expect_equal(res_default$st_scores, res_bray$st_scores)
})

test_that("pcoa returns coordinates, not eigen values", {
  res <- ordination(dune, o_method = "pcoa")
  pco_st <- labdsv::pco(vegan::vegdist(dune,    method = "bray"))
  pco_sp <- labdsv::pco(vegan::vegdist(t(dune), method = "bray"))
  expect_equal(res$st_scores, pco_st$points)
  expect_equal(res$sp_scores, pco_sp$points)
  expect_equal(res$eig_val,   pco_st$eig)
  expect_equal(nrow(res$st_scores), nrow(dune))
  expect_equal(nrow(res$sp_scores), ncol(dune))
})

test_that("ca and dca keep scores of both stands and species", {
  res_ca <- ordination(dune, o_method = "ca")
  expect_equal(nrow(res_ca$st_scores), nrow(dune))
  expect_equal(nrow(res_ca$sp_scores), ncol(dune))
  expect_equal(res_ca$ordination_method, "ca")

  res_dca <- ordination(dune, o_method = "dca")
  expect_equal(nrow(res_dca$st_scores), nrow(dune))
  expect_equal(nrow(res_dca$sp_scores), ncol(dune))
  expect_equal(res_dca$ordination_method, "dca")
})

test_that("nmds returns coordinates and keeps both raw results", {
  invisible(utils::capture.output(res <- ordination(dune, o_method = "nmds")))
  expect_equal(nrow(res$st_scores), nrow(dune))
  expect_equal(nrow(res$sp_scores), ncol(dune))
  expect_null(res$eig_val)
  expect_length(res$results_raw, 2)
})

test_that("ord_extract_score turns scores into a data.frame", {
  res <- ordination(dune, o_method = "pca")
  df <- ord_extract_score(res, score = "st_scores")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), nrow(dune))
  expect_equal(df$rowname, rownames(dune))
  expect_equal(colnames(df)[ncol(df)], "rowname")

  df_named <- ord_extract_score(res, score = "st_scores", row_name = "stand")
  expect_equal(df_named$stand, rownames(dune))
})

test_that("ord_add_group joins group columns onto scores", {
  sp_group <-
    tibble::tibble(species = colnames(dune),
                   initial = stringr::str_sub(colnames(dune), 1, 1))
  res <- ordination(dune, o_method = "pca")
  df <-
    suppressMessages(
      ord_add_group(res, score = "sp_scores", sp_group,
                    indiv = "species", group = "initial"))
  expect_equal(nrow(df), ncol(dune))
  expect_true(all(c("species", "initial") %in% colnames(df)))
  expect_equal(df$initial, stringr::str_sub(df$species, 1, 1))
})

test_that("ord_plot returns a ggplot", {
  res <- ordination(dune, o_method = "pca")
  expect_s3_class(ord_plot(res), "ggplot")
  expect_s3_class(ord_plot(res, score = "sp_scores"), "ggplot")
})
