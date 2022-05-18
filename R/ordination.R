#' Helper function for ordination methods
#' 
#' @param x        A community data matrix.
#'                 rownames: stands.
#'                 colnames: species.
#' @param o_method A string of ordination method. 
#'                 "pca", "ca", "dca", "pcoa", "fspa", or "nmds".
#' @param d_method A string of distance method.
#'                 "correlation", "manhattan", "euclidean", "canberra", 
#'                 "clark", "bray", "kulczynski", "jaccard", "gower", 
#'                 "altGower", "morisita", "horn", "mountford", "raup", 
#'                 "binomial", "chao", "cao", "mahalanobis", "chisq", 
#'                 "chord", "aitchison", or "robust.aitchison".
#' @param ...      other parameters for PCA.
#' @return  Result of ordination.
#'          $st_scores:         scores for stand. 
#'          $sp_scores:         scores for species. 
#'          $eig_val:           eigen value for stand. 
#'          $results_raw:       results of original ordination function. 
#'          $ordination_method: o_method. 
#'          $distance_method:   d_method. 
#' @examples
#' library(vegan)
#' data(dune)
#' ordination(dune, o_method = "dca")
#' 
#' @export
ordination <- function(x, o_method, d_method = NULL, ...){
  res <- list()
  if(is.null(d_method)) d_method <- "bray"
  switch(o_method,
    "pca" = {
      ord <- labdsv::pca(x, dim=10, ...)
      res$st_scores  <- ord$scores                                         # "pca", "scores", "loadings", "sdev", NULL
      res$sp_scores  <- ord$loadings
      res$eig_val <- ord$sdev
      res$d_method <- NULL
      res$results_raw <- ord
    },
    "ca" = {
      ord <- vegan::cca(x)                                                 # "ca", "CA$u", "CA$v", "eig", NULL
      res$st_scores  <- ord$CA$u
      res$sp_scores  <- ord$CA$v
      res$eig_val <- ord$CA$eig
      res$d_method <- NULL
      res$results_raw <- ord
    },
    "dca" = {
      ord <- vegan::decorana(x)
      res$st_scores  <- ord$rproj                                         # "rproj", "cproj", "evals", NULL
      res$sp_scores  <- ord$cproj
      res$eig_val <- ord$evals
      res$d_method <- NULL
      res$results_raw <- ord
    },
    "pcoa" = {
      x_st <- vegan::vegdist(x,    method = d_method) # st
      ord <- labdsv::pco(x_st)
      res$st_scores  <- ord$                                         # "points", "points", "eng", d_method
      res$eig_val <- ord$eig
      res$results_raw[[1]] <- ord
      x_sp <- vegan::vegdist(t(x), method = d_method) # sp
      ord <- labdsv::pco(x_sp)
      res$sp_scores  <- ord$points
      res$results_raw[[2]] <- ord
    },
    "nmds" = {
      x_st <- vegan::vegdist(x,    method = d_method) # st
      ord <- MASS::isoMDS(x_st)
      res$st_scores  <- ord$points
      res$eig_val <- NULL           # no eigen value
      res$results_raw[[1]] <- ord
      x_sp <- vegan::vegdist(t(x), method = d_method) # sp
      ord <- labdsv::pco(x_sp)
      res$sp_scores  <- ord$points
      res$results_raw[[2]] <- ord
    },
    "fspa" = {
      ord <- dave::fspa(x, method = d_method, d.rev=0.5, n.groups=3) # st
      res$st_scores  <- ord$newpoints
      res$eig_val <- ord$eig
      res$results_raw[[1]] <- ord
      ord <- dave::fspa(t(x), method = d_method, d.rev=0.5, n.groups=3) # sp
      res$sp_scores  <- ord$newpoints
      res$results_raw[[2]] <- ord
    }
  )
  res$ordination_method <- o_method
  res$distance_method <- d_method
  return(res)
}

#' Helper function for ordination methods
#' 
#' @param ord    A result of ordination().
#' @param score  A string to specify score for plot.
#'               "st_scores" means stands and "sp_scores" species. 
#' @param x,y    A column number for x and y axis. 
#' @examples
#' library(ggplot2)
#' library(vegan)
#' data(dune)
#' ordination(dune, o_method = "dca") %>%
#'   ord_plot()
#' 
#' @export
ord_plot <- function(ord, score = "st_scores", x = 1, y = 2, col = NULL){
  scores <- as.data.frame(ord[[score]])
  x <- colnames(scores)[x]
  y <- colnames(scores)[y]
  scores %>%
    ggplot2::ggplot(ggplot2::aes(.data[[x]], .data[[y]], 
      label = rownames(scores), 
      col = if(is.null(col)) col else .data[[col]]
    )) +
    ggplot2::geom_text() + 
    ggplot2::theme_bw()
}
