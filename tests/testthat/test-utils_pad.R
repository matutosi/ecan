test_that("pad2longest pads to the width of the longest string", {
  x <- c("a", "ab", "abc")
  expect_equal(pad2longest(x, side = "right"), c("a  ", "ab ", "abc"))
  expect_equal(pad2longest(x, side = "left"),  c("  a", " ab", "abc"))
  expect_equal(pad2longest(x, side = "both"),  c(" a ", "ab ", "abc"))
})

test_that("pad2longest uses the given pad character", {
  x <- c("a", "abc")
  expect_equal(pad2longest(x, side = "right", pad = "-"), c("a--", "abc"))
})

test_that("pad2longest leaves an already-uniform vector unchanged", {
  x <- c("aa", "bb", "cc")
  expect_equal(pad2longest(x), x)
})
