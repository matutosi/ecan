#' Helper function for ordination methods
#' 
#' @param x        A community data matrix.
#' @param o_method A string of ordination method. 
#'                 "pca", "ca", "dca", "pcoa", "fspa", or "nmds".
#' @param d_method A string of distance method.
#' @param ...      other parameters for PCA.
#' @return  Result of ordination.
#'          $scores
#'          $eig_val
#'          $ordination_method
#'          $distance_method
#' @example
#' library(vegan)
#' data(dune)
#' ordination(dune, o_method = "dca")
#' 
#' @export
ordination <- function(x, o_method, d_method = NULL, ...){
  res <- list()
  if(is.null(d_method)) d_method <- "bray"
  if(o_method %in% c("pcoa", "nmds")) # compute dist when PCOA or nMDS
    x <- vegan::vegdist(x, method = d_method)
  switch(o_method,
    "pca" = {
      ord <- labdsv::pca(x, dim=10, ...)
      res$scores  <- ord$scores
      res$eig_val <- ord$sdev
      res$d_method <- NULL
    },
    "ca" = {
      ord <- vegan::cca(x)
      res$scores  <- ord$CA$u
      res$eig_val <- ord$CA$eig
      res$d_method <- NULL
    },
    "dca" = {
      ord <- vegan::decorana(x)
      res$scores  <- ord$rproj
      res$eig_val <- ord$evals
      res$d_method <- NULL
    },
    "pcoa" = {
      ord <- labdsv::pco(x)
      res$scores  <- ord$points
      res$eig_val <- ord$eig
    },
    "fspa" = {
      ord <- dave::fspa(x, method = d_method, d.rev=0.5, n.groups=3)
      res$scores  <- ord$newpoints
      res$eig_val <- ord$eig
    },
    "nmds" = {
      ord <- MASS::isoMDS(x)
      res$scores  <- ord$points
      res$eig_val <- NULL  # no eigen value
    }
  )
  res$ordination_method <- o_method
  res$distance_method <- d_method
  return(res)
}
