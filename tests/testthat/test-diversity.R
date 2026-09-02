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

test_that("shdi counts a zero abundance as an absent species", {
  df <- tibble::tibble(
    stand     = c("a", "a", "b", "b"),
    species   = c("s1", "s2", "s1", "s2"),
    abundance = c(3, 0, 2, 5))
  res <- suppressMessages(shdi(df))
  # the stand "a" holds one species only
  expect_equal(res$s, c(1L, 2L))
  expect_equal(res$h[1], 0)
  expect_false(any(is.na(res$h)))
  # a zero must not change the indices
  nz <- suppressMessages(shdi(dplyr::filter(df, abundance > 0)))
  expect_equal(res, nz)
})

test_that("shdi accepts columns given by name and by position", {
  df <- tibble::tibble(
    st = c("a", "a", "b"), sp = c("s1", "s2", "s1"), ab = c(1, 2, 3))
  named <- suppressMessages(
    shdi(df, stand = "st", species = "sp", abundance = "ab"))
  expect_equal(colnames(named), c("st", "s", "h", "d", "i"))
  expect_equal(named, suppressMessages(shdi(df)))
  expect_error(shdi(dplyr::mutate(df, ab = as.character(ab))))
})
