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

test_that("tw_downweight() gives both ways of downweighting", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  psp <- pseudospecies(dune)
  # "decorana" is the downweighting of vegan
  w <- tw_downweight(psp, method = "decorana")
  expect_equal(length(w), ncol(psp))
  expect_true(all(w > 0 & w <= 1))
  expect_equal(as.vector(sweep(psp, 2, w, "*")),
               as.vector(as.matrix(vegan::downweight(psp))),
               tolerance = 1e-8)
  expect_equal(unname(w[which.max(colSums(psp))]), 1)
  # "hill" is the WEIGHT subroutine: frq_lim of the stands, floor at w_min
  h <- tw_downweight(psp, method = "hill")
  f <- colSums(psp) / nrow(psp)
  expect_equal(unname(h), unname(pmin(f, 0.2) / 0.2 * 0.99 + 0.01))
  expect_true(all(h >= 0.01 & h <= 1))
  expect_equal(unname(h[f >= 0.2]), rep(1, sum(f >= 0.2)))
  # the default is "hill"
  expect_equal(tw_downweight(psp), h)
  # degenerate data
  y <- matrix(1L, 4, 3, dimnames = list(letters[1:4], paste0("s", 1:3)))
  expect_equal(unname(tw_downweight(y, method = "decorana")), rep(1, 3))
  expect_equal(unname(tw_downweight(matrix(0L, 2, 2))), rep(1, 2))
})

test_that("polish = 'hill' reproduces the original TWINSPAN", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  tw <- twinspan(dune)
  expect_equal(tw$polish, "hill")
  # the group of every stand in Hill's program, with his numbering
  hill <- c(22, 22, 22, 22, 21, 21, 21, 6, 23, 21,
            4, 6, 6, 7, 7, 7, 4, 20, 4, 7)
  names(hill) <- rownames(dune)
  id <- stats::setNames(strtoi(paste0("1", tw$classification$path), base = 2L),
                        tw$classification$stand)
  expect_equal(unname(id[names(hill)]), unname(hill))
  # the eigenvalue of the first division of the original
  expect_equal(tw$nodes[[1]]$division$eig, 0.5106, tolerance = 1e-3)
  # the original uses few indicators, chosen to misclassify fewest stands
  expect_lte(length(tw$nodes[[1]]$division$indicator$indicators), 7)
})

test_that("polish = 'ecan' keeps the earlier way", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  tw <- twinspan(dune, polish = "ecan")
  expect_equal(tw$polish, "ecan")
  expect_setequal(tw$classification$stand, rownames(dune))
  expect_true(tw$n_division >= 1)
  expect_error(twinspan(dune, polish = "no_such_way"))
})

test_that("the species are classified on their fidelity to the groups", {
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  tw <- twinspan(dune)
  sd <- tw_species_data(tw)
  # three pseudo-quadrats for every group of stands
  expect_equal(nrow(sd$y), ncol(dune))
  expect_equal(ncol(sd$y) %% 3, 0)
  expect_true(all(sd$y %in% c(0L, 1L)))
  # a species present at the lower cut level is present at the upper ones
  i <- seq(1, ncol(sd$y), by = 3)
  expect_true(all(sd$y[, i] >= sd$y[, i + 1]))
  expect_true(all(sd$y[, i + 1] >= sd$y[, i + 2]))
  # weights: a species weighs as much as it occurs
  expect_equal(sd$rw, unname(colSums(dune > 0)))
  # the two upper cut levels of a group weigh double
  expect_equal(sd$cw[2], 2 * sd$cw[1])
  expect_equal(sd$cw[3], 2 * sd$cw[1])
  # every species is classified once
  sc <- tw$species_classification
  expect_setequal(sc$species, colnames(dune))
  expect_equal(nrow(sc), ncol(dune))
  expect_false(is.unsorted(sc$group))
  # the species groups of Hill's program for the dune data, with his numbering
  hill <- c(37, 13, 36, 13, 37, 17, 17, 15, 16, 29, 29, 16, 36, 36, 29,
            13, 19, 37, 19, 5, 28, 19, 12, 12, 5, 17, 5, 36, 5, 29)
  names(hill) <- colnames(dune)
  id <- stats::setNames(strtoi(paste0("1", sc$path), base = 2L), sc$species)
  expect_equal(unname(id[names(hill)]), unname(hill))
  # tw_two_way() works with it
  expect_silent(tab <- tw_two_way(tw))
  expect_setequal(rownames(tab), colnames(dune))
})

test_that("tw_hill_const() gives the constants of the original", {
  cn <- tw_hill_const()
  expect_equal(cn$rat_lim, 3)
  expect_equal(cn$frq_lim, 0.2)
  expect_equal(cn$feeble, 0.1)
  expect_equal(cn$ipr_exp, 4)
  expect_equal(cn$polish_iter, 2)
  expect_equal(cn$mz_crit, 8)
  expect_equal(cn$mz_out, 4)
  expect_equal(cn$mz_ind, 4)
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
