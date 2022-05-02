test_that("pca results match", {
  res_ord <- ordination(dune, o_method = "pca")
  res_pca <- labdsv::pca(dune, dim = 10)
  expect_equal(res_ord$st_scores, res_pca$scores)
  expect_equal(res_ord$sp_scores, res_pca$loadings)
  expect_equal(res_ord$sdev,   res_pca$eig_val)
})
