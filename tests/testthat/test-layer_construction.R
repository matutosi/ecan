test_that("mid point and bin width can be caluculated", {
  height    <- c(2, 4, 8, 20)
  mid_point <- c(1, 3, 6, 14)
  bin_width <- c(2, 2, 4, 12)
  expect_equal(mid_point(height), mid_point)
  expect_equal(bin_width(height), bin_width)
})
