biss_json <- jsonlite::toJSON(list(
  plot = data.frame(stand = c("A", "B"), site = c("s1", "s2")),
  occ  = data.frame(stand = c("A", "A", "B"),
                     species = c("sp1", "sp2", "sp1"),
                     cover = c(1, 2, 3))
))

test_that("read_biss joins plot and occ by default", {
  df <- read_biss(biss_json, join = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3L)
  expect_true(all(c("stand", "site", "species", "cover") %in% names(df)))
})

test_that("read_biss returns separate tables when join is FALSE", {
  out <- read_biss(biss_json, join = FALSE)
  expect_named(out, c("plot", "occ"))
  expect_equal(nrow(out$plot), 2L)
  expect_equal(nrow(out$occ),  3L)
})
