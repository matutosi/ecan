test_that("mid point and bin width can be caluculated", {
  height    <- c(2, 4, 8, 20)
  mid_point <- c(1, 3, 6, 14)
  bin_width <- c(2, 2, 4, 12)
  expect_equal(mid_point(height), mid_point)
  expect_equal(bin_width(height), bin_width)
})

test_that("add_mid_p_bin_w attaches mid_point and bin_width to df", {
  df <- tibble::tibble(stand = "A", height = c(2, 4, 8, 20), cover = c(1, 2, 3, 4))
  out <- add_mid_p_bin_w(df, "height")
  expect_true(all(c("mid_point", "bin_width") %in% names(out)))
  expect_equal(out$mid_point, mid_point(sort(unique(df$height))))
  expect_equal(out$bin_width, bin_width(sort(unique(df$height))))
  expect_equal(nrow(out), nrow(df))
})

test_that("draw_layer_construction returns a ggplot, with or without a group", {
  df <- tibble::tibble(stand = "A", height = c(2, 4, 8, 20),
                        cover = c(1, 2, 3, 4), sp_group = c("g1", "g1", "g2", "g2"))
  expect_s3_class(draw_layer_construction(df), "ggplot")
  expect_s3_class(draw_layer_construction(df, group = "sp_group"), "ggplot")
})
