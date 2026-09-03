st_list  <- LETTERS[1:3]
sp_list  <- letters[1:3]
ly_list  <- c("B", "S")
cover_list <- 2^(0:2)

test_that("gen_example returns layer and height columns by default", {
  set.seed(1)
  df <- gen_example(n = 30, use_layer = TRUE,
                     height_max = 10, ly_list = ly_list,
                     st_list = st_list, sp_list = sp_list,
                     cover_list = cover_list)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("stand", "layer", "species", "cover", "height") %in% names(df)))
  expect_true(all(df$stand   %in% st_list))
  expect_true(all(df$layer   %in% ly_list))
  expect_true(all(df$species %in% sp_list))
  expect_true(all(df$height >= 0 & df$height <= 10))
})

test_that("gen_example drops layer and height when use_layer is FALSE", {
  set.seed(1)
  df <- gen_example(n = 30, use_layer = FALSE,
                     ly_list = ly_list,
                     st_list = st_list, sp_list = sp_list,
                     cover_list = cover_list)
  expect_false(any(c("layer", "height") %in% names(df)))
  expect_true(all(c("stand", "species", "cover") %in% names(df)))
})

test_that("gen_example attaches optional group columns", {
  set.seed(1)
  st_group <- rep(c("g1", "g2"), length.out = length(st_list))
  sp_group <- rep(c("h1", "h2"), length.out = length(sp_list))
  df <- gen_example(n = 30, use_layer = TRUE,
                     height_max = 10, ly_list = ly_list,
                     st_list = st_list, sp_list = sp_list,
                     st_group = st_group, sp_group = sp_group,
                     cover_list = cover_list)
  expect_true(all(c("st_group", "sp_group") %in% names(df)))
  expect_true(all(df$st_group %in% st_group))
  expect_true(all(df$sp_group %in% sp_group))
})
