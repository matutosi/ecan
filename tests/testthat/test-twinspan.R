test_that("pseudospecies() expands species by cut levels", {
  x <- matrix(c(0, 1, 3, 25), nrow = 2,
              dimnames = list(c("st1", "st2"), c("sp_a", "sp_b")))
  psp <- pseudospecies(x, cut_levels = c(0, 2, 5, 10, 20))
  # sp_a: 0 and 1 -> only level 1 of st2; sp_b: 3 and 25
  expect_equal(nrow(psp), 2)
  expect_true(all(psp %in% c(0L, 1L)))
  expect_equal(unname(psp[, "sp_a_1"]), c(0L, 1L))
  expect_equal(unname(psp[, "sp_b_1"]), c(1L, 1L))
  expect_equal(unname(psp[, "sp_b_2"]), c(1L, 1L))
  expect_equal(unname(psp[, "sp_b_5"]), c(0L, 1L))
  # pseudospecies that occur in no stand are dropped
  expect_false("sp_a_2" %in% colnames(psp))
  expect_equal(length(attr(psp, "species")), ncol(psp))
  expect_equal(length(attr(psp, "level")),   ncol(psp))
  expect_error(pseudospecies(matrix(-1, 2, 2)))
})

test_that("tw_ra() reproduces the first correspondence analysis axis", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  psp <- pseudospecies(dune)
  ra  <- tw_ra(psp)
  ca  <- vegan::cca(psp)
  expect_true(ra$converged)
  expect_equal(ra$eig, unname(ca$CA$eig[1]), tolerance = 1e-6)
  ca1 <- as.vector(vegan::scores(ca, display = "sites", choices = 1))
  expect_equal(abs(stats::cor(ra$sample, ca1)), 1, tolerance = 1e-6)
  # weighted mean of the stand scores is zero
  expect_equal(sum(rowSums(psp) * ra$sample), 0, tolerance = 1e-8)
})

test_that("tw_ra() and tw_inertia() are safe for degenerate data", {
  y <- matrix(0L, 3, 3, dimnames = list(letters[1:3], paste0("s", 1:3)))
  expect_equal(tw_ra(y)$eig, 0)
  expect_equal(tw_inertia(y), 0)
  y1 <- matrix(1L, 3, 3, dimnames = list(letters[1:3], paste0("s", 1:3)))
  expect_equal(tw_inertia(y1), 0, tolerance = 1e-12)
})

test_that("tw_downweight() reproduces the downweighting of vegan", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  psp <- pseudospecies(dune)
  w   <- tw_downweight(psp)
  expect_equal(length(w), ncol(psp))
  expect_true(all(w > 0 & w <= 1))
  expect_equal(as.vector(sweep(psp, 2, w, "*")),
               as.vector(as.matrix(vegan::downweight(psp))),
               tolerance = 1e-8)
  # the most frequent pseudospecies keeps the weight of 1
  expect_equal(unname(w[which.max(colSums(psp))]), 1)
  # no downweighting when every pseudospecies is equally frequent
  y <- matrix(1L, 4, 3, dimnames = list(letters[1:4], paste0("s", 1:3)))
  expect_equal(unname(tw_downweight(y)), rep(1, 3))
  expect_equal(unname(tw_downweight(matrix(0L, 2, 2))), rep(1, 2))
})

test_that("downweighting changes the ordination but not the preference", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  psp <- pseudospecies(dune)
  w   <- tw_downweight(psp)
  expect_lt(tw_ra(psp, w = w)$eig, tw_ra(psp)$eig)
  expect_lt(tw_inertia(psp, w = w), tw_inertia(psp))
  # twinspan() runs both ways and keeps every stand
  for(dw in c(TRUE, FALSE)){
    tw <- twinspan(dune, downweight = dw)
    expect_setequal(tw$classification$stand, rownames(dune))
  }
})

test_that("tw_preference() ranges from -1 to 1", {
  y <- matrix(c(1, 1, 0, 0,
                0, 0, 1, 1,
                1, 0, 1, 0), nrow = 4,
              dimnames = list(paste0("st", 1:4), c("a", "b", "c")))
  pos <- c(FALSE, FALSE, TRUE, TRUE)
  d <- tw_preference(y, pos)
  expect_equal(unname(d[["a"]]), -1)
  expect_equal(unname(d[["b"]]),  1)
  expect_equal(unname(d[["c"]]),  0)
  expect_true(all(d >= -1 & d <= 1))
})

test_that("twinspan() classifies every stand exactly once", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  tw <- twinspan(dune)
  expect_s3_class(tw, "twinspan")
  expect_setequal(tw$classification$stand, rownames(dune))
  expect_equal(nrow(tw$classification), nrow(dune))
  expect_true(tw$n_division >= 1)
  expect_equal(max(tw$classification$group), length(tw$leaves))
  # groups are contiguous in the table order
  expect_false(is.unsorted(tw$classification$group))
  # a stand of a group shares its path
  paths <- unique(tw$classification[, c("group", "path")])
  expect_equal(nrow(paths), max(tw$classification$group))
})

test_that("modified twinspan() stops at n_clusters", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  for(k in 2:5){
    tw <- twinspan(dune, modified = TRUE, n_clusters = k)
    expect_equal(max(tw$classification$group), k)
  }
})

test_that("twinspan() respects min_size and max_depth", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  tw <- twinspan(dune, max_depth = 1)
  expect_equal(max(tw$classification$depth), 1)
  tw <- twinspan(dune, min_size = nrow(dune) + 1)
  expect_equal(max(tw$classification$group), 1)
})

test_that("twinspan() does not divide degenerate data", {
  x <- matrix(1, 4, 3, dimnames = list(letters[1:4], paste0("s", 1:3)))
  expect_equal(max(twinspan(x, min_size = 2)$classification$group), 1)
  x0 <- matrix(0, 4, 3, dimnames = list(letters[1:4], paste0("s", 1:3)))
  expect_equal(max(twinspan(x0, min_size = 2)$classification$group), 1)
})

test_that("as.hclust() gives a valid dendrogram", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  tw  <- twinspan(dune)
  cls <- stats::as.hclust(tw)
  expect_s3_class(cls, "hclust")
  expect_equal(nrow(cls$merge), nrow(dune) - 1)
  expect_setequal(cls$order, seq_len(nrow(dune)))
  expect_false(is.unsorted(cls$height))
  expect_silent(dend <- stats::as.dendrogram(cls))
  # cutting at the number of groups recovers the classification
  k  <- max(tw$classification$group)
  ct <- stats::cutree(cls, k = k)
  g  <- tw$classification$group[match(names(ct), tw$classification$stand)]
  expect_equal(length(unique(paste(ct, g))), k)
})

test_that("tw_two_way() arranges stands and species", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  tw  <- twinspan(dune)
  tab <- tw_two_way(tw)
  expect_s3_class(tab, "tw_two_way")
  expect_equal(dim(unclass(tab)), c(ncol(dune), nrow(dune)))
  expect_setequal(colnames(tab), rownames(dune))
  expect_setequal(rownames(tab), colnames(dune))
  expect_equal(colnames(tab), tw$classification$stand)
  expect_false(is.unsorted(attr(tab, "stand_path")))
  expect_error(tw_two_way(twinspan(dune, species = FALSE)))
})
