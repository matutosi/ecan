test_that("shdi matches vegan's results", {
  library(vegan)
  data(dune)
  df <- table2df(dune)
  res <- 
    shdi(df) %>%
    dplyr::mutate(stand = as.numeric(stand)) %>%
    dplyr::arrange(stand)
  expect_equal(res$s, unname(specnumber(dune)))
  expect_equal(res$h, unname(diversity(dune, index="shannon")))
  expect_equal(res$d, unname(diversity(dune, index="simpson")))
  expect_equal(res$i, unname(diversity(dune, index="invsimpson")))
})
