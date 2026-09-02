#' Two-way indicator species analysis (TWINSPAN)
#'
#' A native R implementation of TWINSPAN (Hill 1979) and
#' modified TWINSPAN (Roleček et al. 2009).
#' The algorithm divides stands (samples) hierarchically by
#' the first axis of a correspondence analysis (reciprocal averaging)
#' of pseudospecies, refines the division with differential species,
#' and summarises it with a small set of indicator pseudospecies.
#'
#' The package is written in plain R and needs no compiler.
#' It is not a port of Hill's FORTRAN program, but `polish = "hill"`
#' (the default) follows the steps of that program:
#' the rare pseudospecies are downweighted as `WEIGHT` does, the axis is
#' polished twice as `POLISH` does, the stands are divided at the middle
#' of the range of the polished axis, and the stands of the critical zone
#' around that point are placed by the indicator pseudospecies, whose
#' number and threshold are the ones that misclassify fewest stands.
#' The constants of the original are in `tw_hill_const()`.
#'
#' The two halves of a division are put in the order of the original as
#' well: the half that resembles the group next to the one being divided
#' comes first, so that neighbouring groups stay together.
#'
#' On the `dune`, `sipoo`, `varespec`, `mite`, `BCI` and `pyrifos` data
#' of `vegan`
#' this reproduces the original program exactly: the same groups with the
#' same numbers, the same divisions and the same eigenvalues.
#' It does so on twenty randomly generated data sets as well.
#' The species are classified in the way of the original as well, that is
#' on how faithful each of them is to the groups of stands rather than on
#' the pseudospecies table itself, and without indicators:
#' see `tw_species_data()`.
#' The species groups are those of the original too.
#'
#' `polish = "ecan"` keeps the earlier way of this package, which was
#' written from the published description alone: the division is refined
#' with the pseudospecies whose preference reaches `diff_threshold`, and
#' the stands are divided at the centroid of the axis.
#' It is kept because it needs no zone or indicator to place a stand,
#' but it does not follow the original as closely.
#'
#' If the results of the original program are needed,
#' the `twinspan` package of Oksanen
#' (<https://github.com/jarioksa/twinspan>, MIT licensed)
#' calls Hill's FORTRAN code itself.
#'

#' @param x           A community data matrix or data.frame.
#'                    rownames: stands, colnames: species.
#' @param cut_levels  A numeric vector of pseudospecies cut levels.
#' @param min_size    An integer. Groups smaller than this are not divided.
#' @param max_depth   An integer of the maximum number of division levels.
#'                    The default (6) is the same as the original TWINSPAN
#'                    (its `levmax`).
#' @param max_indicators
#'                    An integer of the maximum number of indicator
#'                    pseudospecies used to summarise a division.
#' @param diff_threshold
#'                    A numeric in (0, 1].
#'                    A pseudospecies is a differential species when the
#'                    absolute value of its preference is not less than this.
#'                    The default (1/3) corresponds to a 2:1 frequency ratio.
#' @param refine_iter An integer of the maximum number of refinement steps.
#' @param modified    A logical.
#'                    TRUE: modified TWINSPAN.
#'                    The most heterogeneous group is divided first.
#' @param n_clusters  An integer of the number of groups to stop at,
#'                    or NULL for no limit.
#' @param use_indicator
#'                    A logical.
#'                    TRUE: use the indicator ordination for the final
#'                    division (as in the original TWINSPAN).
#'                    FALSE: use the refined ordination.
#' @param downweight  A logical.
#'                    TRUE (the default, as in the original TWINSPAN):
#'                    downweight the rare pseudospecies in the
#'                    ordination, in the way of `decorana()`.
#'                    See `tw_downweight()`.
#' @param polish      A string.
#'                    "hill" (the default): divide in the way of the
#'                    original TWINSPAN.
#'                    "ecan": the earlier way of this package.
#'                    `diff_threshold`, `refine_iter` and `use_indicator`
#'                    are used only by "ecan".
#' @param species     A logical.
#'                    TRUE: classify pseudospecies as well as stands,
#'                    which is needed for tw_two_way().
#'
#' @return  twinspan() returns a list with class "twinspan".
#'          $classification: a tibble of stand, group, path and depth.
#'          $species_classification:
#'                           a tibble of species, group, path and depth
#'                           (of pseudospecies when `polish = "ecan"`).
#'          $nodes:          a list of the nodes of the division tree.
#'          $pseudospecies:  the pseudospecies matrix.
#'          $call, and the parameters above.
#'
#' @references
#' Hill, M.O. (1979) TWINSPAN: a FORTRAN program for arranging multivariate
#' data in an ordered two-way table by classification of the individuals and
#' attributes. Cornell University, Ithaca.
#'
#' Roleček, J., Tichý, L., Zelený, D. and Chytrý, M. (2009)
#' Modified TWINSPAN classification in which the hierarchy respects cluster
#' heterogeneity. Journal of Vegetation Science 20: 596-602.
#'
#' @examples
#' \donttest{
#' data(dune, package = "vegan")
#' tw <- twinspan(dune)
#' tw
#' tw$classification
#'
#' # modified TWINSPAN with a fixed number of groups
#' tw_mod <- twinspan(dune, modified = TRUE, n_clusters = 4)
#' table(tw_mod$classification$group)
#'
#' # use with the clustering helpers of ecan
#' library(ggdendro)
#' ggdendro::ggdendrogram(stats::as.hclust(tw))
#' }
#'
#' @export
twinspan <- function(x,
                     cut_levels     = c(0, 2, 5, 10, 20),
                     min_size       = 5,
                     max_depth      = 6,
                     max_indicators = 7,
                     diff_threshold = 1/3,
                     refine_iter    = 5,
                     modified       = FALSE,
                     n_clusters     = NULL,
                     use_indicator  = FALSE,
                     downweight     = TRUE,
                     polish         = c("hill", "ecan"),
                     species        = TRUE){
  polish <- match.arg(polish)
  cl <- match.call()
  x <- as.matrix(x)
  if(!is.numeric(x)) stop("x needs to be numeric.")
  if(is.null(rownames(x))) rownames(x) <- paste0("stand_", seq_len(nrow(x)))
  if(is.null(colnames(x))) colnames(x) <- paste0("species_", seq_len(ncol(x)))
  stopifnot(nrow(x) >= 2, ncol(x) >= 1)
  stopifnot(diff_threshold > 0, diff_threshold <= 1)
  if(!is.null(n_clusters)) stopifnot(n_clusters >= 1)

  psp <- pseudospecies(x, cut_levels = cut_levels)
  opt <- list(min_size       = min_size,
              max_depth      = max_depth,
              max_indicators = max_indicators,
              diff_threshold = diff_threshold,
              refine_iter    = refine_iter,
              use_indicator  = use_indicator,
              downweight     = downweight,
              polish         = polish)
  if(polish == "hill") opt <- c(opt, tw_hill_const())

  tree <- tw_tree(psp, opt, modified = modified, n_clusters = n_clusters,
                  sp_map = attr(psp, "species"))
  cls  <- tw_leaf_table(tree, rownames(x), unit = "stand")

  sp_tree <- NULL
  sp_cls  <- NULL
  if(species && ncol(psp) >= 2){
    if(polish == "hill"){
      sd <- tw_species_data(list(nodes = tree$nodes), psp = psp,
                            sp_map = attr(psp, "species"), levmax = max_depth)
      if(ncol(sd$y) >= 2 && nrow(sd$y) >= 2){
        # the original classifies the species without indicators (MIND = 0)
        sp_opt <- opt
        sp_opt$max_indicators <- 0L
        sp_tree <- tw_tree(sd$y, sp_opt, modified = modified, n_clusters = NULL,
                           rw = sd$rw, cw = sd$cw)
        sp_cls  <- tw_leaf_table(sp_tree, rownames(sd$y), unit = "species")
      }
    } else {
      sp_tree <- tw_tree(t(psp), opt, modified = modified, n_clusters = NULL)
      sp_cls  <- tw_leaf_table(sp_tree, colnames(psp), unit = "pseudospecies")
      i <- match(sp_cls$pseudospecies, colnames(psp))
      sp_cls <- dplyr::mutate(sp_cls,
                              species = attr(psp, "species")[i],
                              level   = attr(psp, "level")[i],
                              .after  = "pseudospecies")
    }
  }

  res <- list(classification = cls,
              species_classification = sp_cls,
              nodes          = tree$nodes,
              leaves         = tree$leaves,
              n_division     = tree$n_division,
              species_tree   = sp_tree,
              pseudospecies  = psp,
              data           = x,
              labels         = rownames(x),
              cut_levels     = sort(unique(cut_levels)),
              modified       = modified,
              polish         = polish,
              call           = cl)
  res <- c(res, opt)
  class(res) <- "twinspan"
  return(res)
}

#' @rdname twinspan
#' @param ... Ignored.
#' @importFrom stats as.hclust
#' @return  as.hclust() returns an "hclust" object
#'          so that cls_color(), cls_add_group() and
#'          ggdendro::ggdendrogram() can be used.
#' @export
as.hclust.twinspan <- function(x, ...){
  object <- x
  nodes  <- object$nodes
  n_div  <- object$n_division
  merges <- list()
  heis   <- numeric(0)
  add_merge <- function(a, b, h){
    merges[[length(merges) + 1L]] <<- c(a, b)
    heis <<- c(heis, h)
    return(length(merges))
  }
  rec <- function(id){
    nd <- nodes[[id]]
    if(all(is.na(nd$children))){
      m <- nd$members
      if(length(m) == 1) return(-m[1])
      cur <- add_merge(-m[1], -m[2], 0)
      for(k in seq_along(m)[-(1:2)]) cur <- add_merge(cur, -m[k], 0)
      return(cur)
    }
    a <- rec(nd$children[1])
    b <- rec(nd$children[2])
    return(add_merge(a, b, n_div - nd$order + 1))
  }
  rec_order <- function(id){
    nd <- nodes[[id]]
    if(all(is.na(nd$children))) return(nd$members)
    c(rec_order(nd$children[1]), rec_order(nd$children[2]))
  }
  rec(1L)
  ord <- order(heis)           # stable: children are created before parents
  pos <- integer(length(ord))
  pos[ord] <- seq_along(ord)
  mg <- t(vapply(merges[ord], function(v) {
           ifelse(v > 0, pos[abs(v)], v)
         }, numeric(2)))
  cls <- list(merge  = matrix(as.integer(mg), ncol = 2),
              height = heis[ord],
              order  = rec_order(1L),
              labels = object$labels,
              method = if(object$modified) "twinspan (modified)" else "twinspan",
              call   = object$call,
              dist.method = "pseudospecies")
  cls$clustering_method <- cls$method
  cls$distance_method   <- cls$dist.method
  class(cls) <- "hclust"
  return(cls)
}

#' @rdname twinspan
#' @return  print() returns the object invisibly.
#' @export
print.twinspan <- function(x, ...){
  cat(if(x$modified) "Modified TWINSPAN\n" else "TWINSPAN\n")
  cat("  stands:       ", length(x$labels), "\n", sep = "")
  cat("  pseudospecies:", ncol(x$pseudospecies), "\n", sep = " ")
  cat("  cut levels:   ", paste(x$cut_levels, collapse = " "), "\n", sep = "")
  cat("  divisions:    ", x$n_division, "\n", sep = "")
  cat("  groups:       ", length(x$leaves), "\n\n", sep = "")
  for(id in seq_along(x$nodes)){
    nd <- x$nodes[[id]]
    if(is.null(nd$division)) next
    ind <- nd$division$indicator
    lab <- if(is.null(ind)) "(none)" else
             paste0(colnames(x$pseudospecies)[ind$indicators],
                    ifelse(ind$sign > 0, "(+)", "(-)"), collapse = " ")
    cat("division ", nd$order,
        " at level ", nd$depth,
        " (n = ", length(nd$members),
        ", eig = ", round(nd$division$eig, 3), ")\n", sep = "")
    cat("  indicators: ", lab, "\n", sep = "")
  }
  invisible(x)
}

#' Pseudospecies transformation
#'
#' Expands each species into binary pseudospecies by cut levels.
#' A pseudospecies of a cut level is present when the abundance is
#' larger than zero and not less than the cut level.
#' Pseudospecies that occur in no stand are dropped.
#'
#' @inheritParams twinspan
#' @return  A binary matrix of stands by pseudospecies with
#'          "species", "level" and "cut_levels" attributes.
#' @examples
#' \donttest{
#' data(dune, package = "vegan")
#' psp <- pseudospecies(dune)
#' dim(psp)
#' head(colnames(psp))
#' }
#' @export
pseudospecies <- function(x, cut_levels = c(0, 2, 5, 10, 20)){
  x <- as.matrix(x)
  if(!is.numeric(x)) stop("x needs to be numeric.")
  if(any(x < 0, na.rm = TRUE)) stop("x needs to be non-negative.")
  x[is.na(x)] <- 0
  cut_levels <- sort(unique(cut_levels))
  stopifnot(length(cut_levels) >= 1)
  y  <- lapply(cut_levels, function(cl) (x > 0) & (x >= cl))
  y  <- do.call(cbind, y)
  y  <- y * 1L
  sp <- rep(colnames(x), times = length(cut_levels))
  lv <- rep(seq_along(cut_levels), each = ncol(x))
  colnames(y) <- paste0(sp, "_", lv)
  rownames(y) <- rownames(x)
  keep <- colSums(y) > 0
  y  <- y[, keep, drop = FALSE]
  attr(y, "species")    <- sp[keep]
  attr(y, "level")      <- lv[keep]
  attr(y, "cut_levels") <- cut_levels
  return(y)
}

#' Reciprocal averaging (first correspondence analysis axis)
#'
#' @param y        A binary matrix of stands by pseudospecies.
#' @param w        A numeric vector of pseudospecies weights, or NULL.
#' @param rw       A numeric vector of stand weights, or NULL.
#' @param max_iter An integer of the maximum number of iterations.
#' @param tol      A numeric of the convergence tolerance.
#' @return  A list of stand scores ($sample), pseudospecies scores
#'          ($species), the eigenvalue ($eig) and $converged.
#' @examples
#' \donttest{
#' data(dune, package = "vegan")
#' ra <- tw_ra(pseudospecies(dune))
#' ra$eig
#' }
#' @export
tw_ra <- function(y, w = NULL, rw = NULL, max_iter = 999, tol = 1e-10){
  y <- as.matrix(y) * 1
  n <- nrow(y)
  p <- ncol(y)
  if(is.null(w)) w <- rep(1, p)
  ym <- sweep(y, 2, w, "*")
  if(!is.null(rw)) ym <- sweep(ym, 1, rw, "*")
  r  <- rowSums(ym)
  cs <- colSums(ym)
  smp <- stats::setNames(rep(0, n), rownames(y))
  spc <- stats::setNames(rep(0, p), colnames(y))
  ok_r <- r  > 0
  ok_c <- cs > 0
  if(sum(ok_r) < 2 || sum(ok_c) < 1 || sum(ym) <= 0)
    return(list(sample = smp, species = spc, eig = 0, converged = FALSE))
  yy <- ym[ok_r, ok_c, drop = FALSE]
  rr <- r[ok_r]
  cc <- cs[ok_c]
  xi <- seq_len(nrow(yy)) - (nrow(yy) + 1) / 2
  xi <- xi - sum(rr * xi) / sum(rr)
  s  <- sqrt(sum(rr * xi^2) / sum(rr))
  if(s < 1e-12) return(list(sample = smp, species = spc, eig = 0, converged = FALSE))
  xi <- xi / s
  eig  <- 0
  conv <- FALSE
  u    <- rep(0, length(cc))
  for(i in seq_len(max_iter)){
    u  <- as.vector(crossprod(yy, xi) / cc)
    xn <- as.vector(yy %*% u / rr)
    xn <- xn - sum(rr * xn) / sum(rr)
    s  <- sqrt(sum(rr * xn^2) / sum(rr))
    if(!is.finite(s) || s < 1e-12){
      xi <- rep(0, length(xi))
      eig  <- 0
      conv <- TRUE
      break
    }
    xn  <- xn / s
    eig <- s
    if(max(abs(xn - xi)) < tol || max(abs(xn + xi)) < tol){
      xi <- xn
      conv <- TRUE
      break
    }
    xi <- xn
  }
  k <- which.max(abs(xi))
  if(length(k) == 1 && xi[k] < 0){
    xi <- -xi
    u  <- -u
  }
  smp[ok_r] <- xi
  spc[ok_c] <- u
  return(list(sample = smp, species = spc, eig = eig, converged = conv))
}

#' Downweighting of rare pseudospecies
#'
#' Gives a weight to each pseudospecies, so that the rare ones weigh less
#' in the ordination.
#' The original TWINSPAN downweights them before the correspondence
#' analysis, and `twinspan()` does the same by default.
#' The weights are used only in the ordination:
#' the preference of the pseudospecies is counted on the raw occurrences.
#'
#' Two ways are available.
#' "hill" is the `WEIGHT` subroutine of the original TWINSPAN:
#' a pseudospecies occurring in a smaller proportion of the stands than
#' `frq_lim` is weighted in proportion to that shortfall, and no weight
#' falls below `w_min`.
#' "decorana" is the downweighting of `decorana()` and of
#' `vegan::downweight()`, where the frequencies are compared with the
#' most frequent pseudospecies instead of a fixed proportion.
#'
#' @inheritParams tw_ra
#' @param method   A string, "hill" or "decorana".
#' @param fraction A numeric of the downweighting fraction of "decorana".
#' @param frq_lim  A numeric of the frequency above which "hill" does not
#'                 downweight.
#' @param w_min    A numeric of the smallest weight of "hill".
#' @return  A numeric vector of the weight of each pseudospecies.
#' @examples
#' \donttest{
#' data(dune, package = "vegan")
#' psp <- pseudospecies(dune)
#' summary(tw_downweight(psp))
#' summary(tw_downweight(psp, method = "decorana"))
#' }
#' @export
tw_downweight <- function(y,
                          method   = c("hill", "decorana"),
                          fraction = 5,
                          frq_lim  = 0.2,
                          w_min    = 0.01,
                          rw       = NULL){
  method <- match.arg(method)
  y   <- as.matrix(y) * 1
  tot <- colSums(y)
  one <- stats::setNames(rep(1, ncol(y)), colnames(y))
  if(!nrow(y) || !any(tot > 0)) return(one)
  if(method == "hill"){
    if(is.null(rw)){
      f <- tot / nrow(y)
    } else {
      f <- colSums(sweep(y, 1, rw, "*")) / sum(rw)
    }
    f <- pmin(f, frq_lim)
    w <- (f / frq_lim) * (1 - w_min) + w_min
  } else {
    lim <- max(tot) / fraction
    w   <- ifelse(tot < lim, tot / lim, 1)
  }
  return(stats::setNames(as.vector(w), colnames(y)))
}

#' Total inertia of a pseudospecies matrix
#'
#' Used as the heterogeneity of a group in modified TWINSPAN.
#'
#' @inheritParams tw_ra
#' @return  A numeric of the total inertia (0 when it cannot be computed).
#' @examples
#' \donttest{
#' data(dune, package = "vegan")
#' tw_inertia(pseudospecies(dune))
#' }
#' @export
tw_inertia <- function(y, w = NULL){
  y <- as.matrix(y) * 1
  if(!is.null(w)) y <- sweep(y, 2, w, "*")
  y <- y[rowSums(y) > 0, , drop = FALSE]
  y <- y[, colSums(y) > 0, drop = FALSE]
  if(nrow(y) < 2 || ncol(y) < 2) return(0)
  tot <- sum(y)
  r   <- rowSums(y) / tot
  cs  <- colSums(y) / tot
  e   <- outer(r, cs)
  return(sum((y / tot - e)^2 / e))
}

#' Preference of pseudospecies for one side of a division
#'
#' The preference is (f2 - f1) / (f2 + f1),
#' where f1 and f2 are the relative frequencies of the pseudospecies
#' in the negative and the positive group.
#' It ranges from -1 (only in the negative group) to 1.
#'
#' @inheritParams tw_ra
#' @param positive A logical vector. TRUE: the stand is in the positive group.
#' @return  A numeric vector of the preference of each pseudospecies.
#' @examples
#' \donttest{
#' data(dune, package = "vegan")
#' psp <- pseudospecies(dune)
#' pos <- tw_ra(psp)$sample > 0
#' summary(tw_preference(psp, pos))
#' }
#' @export
tw_preference <- function(y, positive){
  y  <- as.matrix(y) * 1
  N1 <- sum(!positive)
  N2 <- sum(positive)
  f1 <- if(N1 > 0) colSums(y[!positive, , drop = FALSE]) / N1 else rep(0, ncol(y))
  f2 <- if(N2 > 0) colSums(y[ positive, , drop = FALSE]) / N2 else rep(0, ncol(y))
  d  <- ifelse(f1 + f2 > 0, (f2 - f1) / (f2 + f1), 0)
  return(stats::setNames(as.vector(d), colnames(y)))
}

# Heterogeneity of a group, downweighted in the same way as the ordination
tw_het <- function(y, opt){
  w <- if(!isTRUE(opt$downweight)) NULL
       else if(identical(opt$polish, "hill"))
         tw_downweight(y, method = "hill", frq_lim = opt$frq_lim,
                       w_min = opt$cwt_min)
       else tw_downweight(y, method = "decorana")
  tw_inertia(y, w = w)
}

# Build the division tree of a binary matrix (rows are the units to divide)
tw_tree <- function(y, opt, modified = FALSE, n_clusters = NULL, sp_map = NULL,
                    rw = NULL, cw = NULL){
  lv <- if(identical(opt$polish, "hill")) tw_species_counts(y, sp_map) else NULL
  sp_id <- if(is.null(sp_map)) seq_len(ncol(y)) else
             as.integer(factor(sp_map, levels = unique(sp_map)))
  nodes <- list(list(id       = 1L,
                     parent   = NA_integer_,
                     members  = seq_len(nrow(y)),
                     depth    = 0L,
                     path     = "",
                     children = c(NA_integer_, NA_integer_),
                     order    = NA_integer_,
                     terminal = FALSE,
                     tried    = FALSE,
                     heterogeneity = tw_het(y, opt),
                     division = NULL))
  n_div <- 0L
  repeat{
    leaves <- which(vapply(nodes, function(nd) all(is.na(nd$children)), logical(1)))
    if(!is.null(n_clusters) && length(leaves) >= n_clusters) break
    cand <- leaves[vapply(nodes[leaves], tw_is_divisible, logical(1), opt = opt)]
    if(!length(cand)) break
    if(modified){
      het    <- vapply(nodes[cand], function(nd) nd$heterogeneity, numeric(1))
      target <- cand[which.max(het)]
    } else {
      dep    <- vapply(nodes[cand], function(nd) nd$depth, integer(1))
      target <- cand[order(dep, cand)][1]
    }
    nd  <- nodes[[target]]
    ctx <- NULL
    if(!is.null(lv))
      ctx <- list(lv      = lv,
                  rw      = rw,
                  cw      = cw,
                  sp      = sp_id,
                  path    = nd$path,
                  members = nd$members,
                  sibling = tw_members_of(nodes, tw_sib_path(nd$path)),
                  uncle   = tw_members_of(nodes,
                              tw_sib_path(substr(nd$path, 1, nchar(nd$path) - 1))))
    div <- tw_divide(y[nd$members, , drop = FALSE], opt, ctx)
    nodes[[target]]$tried <- TRUE
    if(is.null(div)){
      nodes[[target]]$terminal <- TRUE
      next
    }
    n_div <- n_div + 1L
    nodes[[target]]$order    <- n_div
    nodes[[target]]$division <- div
    kids <- integer(2)
    for(k in 1:2){
      idx <- if(k == 1) nd$members[!div$positive] else nd$members[div$positive]
      id  <- length(nodes) + 1L
      nodes[[id]] <- list(id       = id,
                          parent   = target,
                          members  = idx,
                          depth    = nd$depth + 1L,
                          path     = paste0(nd$path, k - 1L),
                          children = c(NA_integer_, NA_integer_),
                          order    = NA_integer_,
                          terminal = FALSE,
                          tried    = FALSE,
                          heterogeneity = tw_het(y[idx, , drop = FALSE], opt),
                          division = NULL)
      kids[k] <- id
    }
    nodes[[target]]$children <- kids
  }
  leaves <- which(vapply(nodes, function(nd) all(is.na(nd$children)), logical(1)))
  leaves <- leaves[order(vapply(nodes[leaves], function(nd) nd$path, character(1)))]
  return(list(nodes = nodes, leaves = leaves, n_division = n_div))
}

# Leaves of a division tree as a tibble
tw_leaf_table <- function(tree, labels, unit = "stand"){
  res <- tibble::tibble(unit = character(0), group = integer(0),
                        path = character(0), depth = integer(0))
  for(i in seq_along(tree$leaves)){
    nd  <- tree$nodes[[tree$leaves[i]]]
    res <- dplyr::bind_rows(res,
             tibble::tibble(unit  = labels[nd$members],
                            group = i,
                            path  = nd$path,
                            depth = nd$depth))
  }
  colnames(res)[1] <- unit
  return(res)
}

# Is a node worth trying to divide?
tw_is_divisible <- function(node, opt){
  if(node$terminal) return(FALSE)
  if(node$tried)    return(FALSE)
  if(node$depth >= opt$max_depth)       return(FALSE)
  if(length(node$members) < opt$min_size) return(FALSE)
  return(TRUE)
}

# Refined ordination: iterate preference weighting until the division is stable
tw_refine <- function(y, positive, diff_threshold, refine_iter){
  sc <- rep(0, nrow(y))
  d  <- rep(0, ncol(y))
  for(i in seq_len(refine_iter)){
    d    <- tw_preference(y, positive)
    keep <- abs(d) >= diff_threshold
    if(!any(keep)) break
    ys  <- y[, keep, drop = FALSE]
    ds  <- d[keep]
    den <- rowSums(ys)
    sc  <- ifelse(den > 0, as.vector(ys %*% ds) / den, 0)
    new <- sc > 0
    new[den == 0] <- positive[den == 0]   # borderline stands keep their side
    if(all(new) || !any(new)) break       # degenerate: keep the previous division
    stable <- all(new == positive)
    positive <- new
    if(stable) break
  }
  return(list(positive = positive, score = sc, preference = d))
}

# Indicator ordination: summarise the division with a few pseudospecies
tw_indicator <- function(y, positive, d, max_indicators, diff_threshold){
  cand <- which(abs(d) >= diff_threshold)
  if(!length(cand)) return(NULL)
  freq <- colSums(y)
  ord  <- cand[order(-abs(d[cand]), -freq[cand], colnames(y)[cand])]
  sel  <- ord[seq_len(min(max_indicators, length(ord)))]
  sgn  <- sign(d[sel])
  score <- as.vector(y[, sel, drop = FALSE] %*% sgn)
  cuts  <- c(sort(unique(score)), max(score) + 1)
  mis   <- vapply(cuts, function(t) sum((score >= t) != positive), numeric(1))
  best  <- which(mis == min(mis))
  bal   <- vapply(cuts[best], function(t) abs(sum(score >= t) - length(score) / 2), numeric(1))
  thr   <- cuts[best][which.min(bal)]
  return(list(indicators    = sel,
              sign          = sgn,
              score         = score,
              threshold     = thr,
              misclassified = min(mis),
              positive      = score >= thr))
}

#' Ordered two-way table of a TWINSPAN result
#'
#' Arranges the community data with the stands and the species in the
#' order of their division paths, as in the printed output of TWINSPAN.
#' The dichotomy of each stand is shown by the digits below the table.
#'
#' @param object A "twinspan" object made with species = TRUE.
#' @param cells  A string.
#'               "level": the pseudospecies cut level of each cell.
#'               "abundance": the original values.
#' @param ...    Ignored.
#' @return  tw_two_way() returns a character matrix with class
#'          "tw_two_way", "stand_path" and "species_path" attributes.
#' @examples
#' \donttest{
#' data(dune, package = "vegan")
#' tw_two_way(twinspan(dune))
#' }
#' @export
tw_two_way <- function(object, cells = c("level", "abundance")){
  stopifnot(inherits(object, "twinspan"))
  cells <- match.arg(cells)
  if(is.null(object$species_classification))
    stop('Needs the species classification: twinspan(x, species = TRUE).')
  psp <- object$pseudospecies
  x   <- object$data
  spm <- attr(psp, "species")

  st       <- object$classification
  st_order <- match(st$stand, rownames(x))

  sc <- object$species_classification
  if("level" %in% colnames(sc)) sc <- sc[order(sc$level), ]
  first <- sc[!duplicated(sc$species), ]
  first <- first[order(first$path, first$species), ]
  sp_order <- match(first$species, colnames(x))

  lv <- vapply(colnames(x),
               function(s) rowSums(psp[, spm == s, drop = FALSE]),
               numeric(nrow(x)))
  lv <- matrix(lv, nrow = nrow(x), dimnames = list(rownames(x), colnames(x)))
  v  <- if(cells == "level") lv else as.matrix(x)
  v  <- t(v)[sp_order, st_order, drop = FALSE]
  tab <- ifelse(v == 0, "-", as.character(v))
  dim(tab)      <- dim(v)
  dimnames(tab) <- dimnames(v)
  attr(tab, "stand_path")   <- st$path
  attr(tab, "species_path") <- first$path
  attr(tab, "stand_group")  <- st$group
  attr(tab, "species_group") <- first$group
  class(tab) <- c("tw_two_way", class(tab))
  return(tab)
}

#' @rdname tw_two_way
#' @param x A "tw_two_way" object.
#' @return  print() returns the object invisibly.
#' @export
print.tw_two_way <- function(x, ...){
  tab <- unclass(x)
  st  <- attr(x, "stand_path")
  sp  <- attr(x, "species_path")
  w   <- max(nchar(tab), 1)
  lab <- max(nchar(rownames(tab)))
  pad <- function(s, n) formatC(s, width = n, flag = "-")
  nch <- max(nchar(colnames(tab)))
  for(i in seq_len(nch)){
    ch <- substr(colnames(tab), i, i)
    ch[is.na(ch)] <- " "
    cat(strrep(" ", lab + 1),
        paste0(formatC(ch, width = w), collapse = ""), "\n", sep = "")
  }
  for(i in seq_len(nrow(tab))){
    cat(pad(rownames(tab)[i], lab), " ",
        paste0(formatC(tab[i, ], width = w), collapse = ""),
        "  ", sp[i], "\n", sep = "")
  }
  nch <- max(nchar(st), 1)
  cat("\n")
  for(i in seq_len(nch)){
    ch <- substr(st, i, i)
    ch[ch == ""] <- " "
    cat(strrep(" ", lab + 1),
        paste0(formatC(ch, width = w), collapse = ""), "\n", sep = "")
  }
  invisible(x)
}

# One dichotomy: primary -> refined -> indicator ordination
tw_divide <- function(y, opt, ctx = NULL){
  if(identical(opt$polish, "hill")) return(tw_divide_hill(y, opt, ctx))
  w   <- if(isTRUE(opt$downweight)) tw_downweight(y, method = "decorana") else NULL
  ra  <- tw_ra(y, w = w)
  pos <- ra$sample > 0
  if(all(pos) || !any(pos)) return(NULL)
  rf  <- tw_refine(y, pos, opt$diff_threshold, opt$refine_iter)
  pos <- rf$positive
  if(all(pos) || !any(pos)) return(NULL)
  ind <- tw_indicator(y, pos, rf$preference, opt$max_indicators, opt$diff_threshold)
  if(opt$use_indicator && !is.null(ind) && any(ind$positive) && !all(ind$positive))
    pos <- ind$positive
  return(list(positive   = pos,
              eig        = ra$eig,
              primary    = ra$sample,
              refined    = rf$score,
              preference = rf$preference,
              indicator  = ind))
}
