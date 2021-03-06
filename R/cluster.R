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
#' @param indiv,group
#'                 A string to specify indiv, group, row_name column in df.
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
#' ggdendro::ggdendrogram(cls)
#' 
#' cls <- cls_add_group(cls, df, indiv = "stand", group = "Use")
#' ggdendro::ggdendrogram(cls)
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


#' Add group names to hclust labels.
#' 
#' @param cls      A result of cluster analysis (hclust).
#' @inheritParams  ordination
#' @rdname         cluster
#' @export
cls_add_group <- function(cls, df, indiv, group){
  labs_group <- 
    df %>%
    dplyr::distinct(!!indiv := as.character(.data[[indiv]]), .data[[group]]) 
  labs_group <- 
    tibble::tibble(!!indiv := cls$labels) %>%
    dplyr::left_join(labs_group)
  cls$labels <- stringr::str_c(labs_group[[group]] , "_", cls$labels)
  return(cls)
}

#' Add colors to dendrogram
#' 
#' @param cls      A result of cluster or dendrogram.
#' @inheritParams  ordination
#' @rdname         cluster
#' @return         Inputing cls return a color vector, 
#'                 inputing dend return a dend with color.
#' @export
cls_color <- function(cls, df, indiv, group){
  color <- "color"
  labs_group <- 
    df %>%
    dplyr::distinct(!!indiv := as.character(.data[[indiv]]), .data[[group]]) 
  col <- 
    tibble::tibble(!!group :=                    unique(labs_group[[group]]), 
           !!color := seq_along(unique(labs_group[[group]])) + 1)
  labs <- 
    if(dendextend::is.hclust(cls)){
      cls$labels
    } else if(dendextend::is.dendrogram(cls)) {
      labels(cls)
    }
  lab_col <- 
    tibble::tibble(!!indiv := labs) %>%
    dplyr::left_join(labs_group) %>%
    dplyr::left_join(col)
  col <- lab_col[[color]]
  if(dendextend::is.hclust(cls)){
    return(col)
  } else if(dendextend::is.dendrogram(cls)) {
    dendextend::labels_colors(cls) <- lab_col[[color]]
    return(cls)
  }
}
