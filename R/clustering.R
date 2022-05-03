#' Helper function for clustering methods
#' 
#' @param x        A community data matrix. 
#'                 rownames: stands.
#'                 colnames: species.
#' @param c_method A string of clustering method. 
#'                 "ward.D", "ward.D2", "single", "complete", 
#'                 "average" (= UPGMA), "mcquitty" (= WPGMA), 
#'                 "median" (= WPGMC), "centroid" (= UPGMC), or
#'                 "diana".
#' @param d_method A string of distance method.
#'                 "correlation", "manhattan", "euclidean", "canberra", 
#'                 "clark", "bray", "kulczynski", "jaccard", "gower", 
#'                 "altGower", "morisita", "horn", "mountford", "raup", 
#'                 "binomial", "chao", "cao", "mahalanobis", "chisq", 
#'                 "chord", "aitchison", or "robust.aitchison".
#' @return  Result of clustering.
#'          $clustering_method: c_method
#'          $distance_method:   d_method
#' @examples
#' library(vegan)
#' data(dune)
#' clustering(dune, c_method = "average", d_method = "euclidean")
#' 
#' @export
clustering <- function(x, c_method, d_method){
  cls <- list()
    # distance
  if(d_method == "correlation"){
    d <- stats::as.dist( ( 1 - stats::cor( t(x) ) ) / 2, diag = TRUE)
  } else {
    d <- vegan::vegdist(x, method = d_method, diag = TRUE)
  }
    # clustering
  if(c_method == "diana"){
    cls <- stats::as.hclust(cluster::diana(d, diss = TRUE))
  } else {
    cls <- stats::hclust(d, method = c_method)
  }
    # methods
  cls$clustering_method <- c_method
  cls$distance_method <- d_method
    # result
  return(cls)
}

#' Transfer when true
#' 
#' @param x        A community data matrix.
#' @param cond     A logical.
#' 
#' @export
t_if_true <- function(x, cond){
  if(cond) t(x) else x
}
