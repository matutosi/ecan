data(dune,     package = "vegan")
data(dune.env, package = "vegan")

env <- tibble::rownames_to_column(dune.env, "stand")

## the "call" attribute records the call site, so it always differs
test_that("distance matches vegan::vegdist", {
  expect_equal(distance(dune, "bray"),
               vegan::vegdist(dune, method = "bray", diag = TRUE),
               ignore_attr = "call")
  expect_equal(distance(dune, "euclidean"),
               vegan::vegdist(dune, method = "euclidean", diag = TRUE),
               ignore_attr = "call")
})

test_that("correlation distance is scaled into 0 to 1", {
  d <- distance(dune, "correlation")
  expect_equal(d, stats::as.dist((1 - stats::cor(t(dune))) / 2, diag = TRUE),
               ignore_attr = "call")
  expect_true(all(d >= 0 & d <= 1))
})

test_that("cluster matches stats::hclust and records its methods", {
  cls <- cluster(dune, c_method = "average", d_method = "bray")
  ref <- stats::hclust(distance(dune, "bray"), method = "average")
  expect_equal(cls$merge,  ref$merge)
  expect_equal(cls$height, ref$height)
  expect_equal(cls$labels, ref$labels)
  expect_equal(cls$clustering_method, "average")
  expect_equal(cls$distance_method,   "bray")
})

test_that("diana is dispatched to cluster::diana", {
  cls <- cluster(dune, c_method = "diana", d_method = "bray")
  ref <- stats::as.hclust(cluster::diana(distance(dune, "bray"), diss = TRUE))
  expect_equal(cls$merge,  ref$merge)
  expect_equal(cls$height, ref$height)
  expect_equal(cls$clustering_method, "diana")
})

test_that("cls_add_group prefixes group names to the labels", {
  cls <- cluster(dune, c_method = "average", d_method = "bray")
  res <- suppressMessages(
    cls_add_group(cls, env, indiv = "stand", group = "Use"))
  expect_length(res$labels, length(cls$labels))
  expect_true(all(stringr::str_detect(res$labels, "-")))
  # padding makes every label the same width
  expect_length(unique(stringr::str_length(res$labels)), 1)

  group <- env$Use[match(cls$labels, env$stand)]
  expect_equal(stringr::str_remove(res$labels, "-.*"),
               pad2longest(as.character(group), side = "left", pad = "_"))
})

test_that("cls_add_group can skip the padding", {
  cls <- cluster(dune, c_method = "average", d_method = "bray")
  res <- suppressMessages(
    cls_add_group(cls, env, indiv = "stand", group = "Use", pad = FALSE))
  group <- env$Use[match(cls$labels, env$stand)]
  expect_equal(res$labels,
               stringr::str_c(as.character(group), "-", cls$labels))
})

test_that("cls_color gives one color per label, shared within a group", {
  cls <- cluster(dune, c_method = "average", d_method = "bray")
  col <- suppressMessages(cls_color(cls, env, indiv = "stand", group = "Use"))
  group <- env$Use[match(cls$labels, env$stand)]
  expect_length(col, length(cls$labels))
  expect_length(unique(col), length(unique(group)))
  expect_true(all(tapply(col, group, function(x) length(unique(x))) == 1))
})

test_that("cls_color colors the labels of a dendrogram", {
  cls  <- cluster(dune, c_method = "average", d_method = "bray")
  dend <- stats::as.dendrogram(cls)
  res  <- suppressMessages(
    cls_color(dend, env, indiv = "stand", group = "Use"))
  expect_s3_class(res, "dendrogram")
  expect_length(dendextend::labels_colors(res), length(labels(dend)))
})
