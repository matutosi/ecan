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
#' library(ggdendro)
#' data(dune)
#' data(dune.env)
#' 
#' df <- 
#'   table2df(dune) %>%
#'   dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
#' sp_dammy <- 
#'  tibble::tibble("species" = colnames(dune), 
#'                 "dammy_1" = stringr::str_sub(colnames(dune), 1, 1),
#'                 "dammy_6" = stringr::str_sub(colnames(dune), 6, 6))
#' df <- 
#'   df %>%
#'   dplyr::left_join(sp_dammy)
#' 
#' cls <- cluster(dune, c_method = "average", d_method = "euclidean")
#' ggdendrogram(cls)
#' 
#' cls$labels <- 
#'   cls_add_group(cls, df, single = "stand", group = "Use")
#' ggdendrogram(cls)
#' 
#' 
#' 
#' 
#' @export
cluster <- function(x, c_method, d_method){
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


#' Transfer when true
#' 
#' @param cls      A result of cluster analysis.
#' @inheritParams  ordination
#' @rdname         cluster
#' @export
cls_add_group <- function(cls, df, single, group){
  single_group <- 
    tibble::tibble(`:=`({{single}}, cls$labels)) %>%
    dplyr::left_join(dplyr::distinct(df, .data[[single]], .data[[group]]))
  lab_1 <- 
    single_group[[group]] %>%
    pad2longest(side = "right", pad = "_")
  lab_2 <- pad2longest(cls$labels, side = "left", pad = "_")
  new_lab <- stringr::str_c(lab_1 , "-", lab_2)
  return(new_lab)
}
