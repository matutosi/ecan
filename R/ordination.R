#' Helper function for ordination methods
#' 
#' @param tbl      A community data matrix.
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
#' @param ...      Other parameters for PCA.
#' @param ord      A result of ordination().
#' @param score    A string to specify score for plot.
#'                 "st_scores" means stands and "sp_scores" species. 
#' @param x,y      A column number for x and y axis. 
#' @param df       A data.frame to be added into ord scores
#' @param indiv,group 
#'                 A string to specify indiv or group column in df.
#' @return  ordination() returns result of ordination.
#'          $st_scores:         scores for stand. 
#'          $sp_scores:         scores for species. 
#'          $eig_val:           eigen value for stand. 
#'          $results_raw:       results of original ordination function. 
#'          $ordination_method: o_method. 
#'          $distance_method:   d_method. 
#'          ord_plot() returns ggplot2 object. 
#'          ord_extract_score() extracts stand or species scores 
#'          from ordination result. 
#'          ord_add_group() adds group data.frame into ordination scores.
#' @examples
#' library(ggplot2)
#' library(vegan)
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
#' ord_dca <- ordination(dune, o_method = "dca")
#' ord_pca <- 
#'   df %>%
#'   df2table() %>%
#'   ordination(o_method = "pca")
#' 
#' ord_dca_st <- 
#'   ord_extract_score(ord_dca, score = "st_scores")
#' ord_pca_sp <- 
#'   ord_add_group(ord_pca, 
#'   score = "st_scores", df, indiv = "species", group = "dammy_1")
#' 
#' # ord_plot(ord)
#' 
#' @export
ordination <- function(tbl, o_method, d_method = NULL, ...){
  res <- list()
  if(is.null(d_method)) d_method <- "bray"
  switch(o_method,
    "pca" = {
      ord <- labdsv::pca(tbl, dim=10, ...)
      res$st_scores  <- ord$scores                                         # "pca", "scores", "loadings", "sdev", NULL
      res$sp_scores  <- ord$loadings
      res$eig_val <- ord$sdev
      res$d_method <- NULL
      res$results_raw <- ord
    },
    "ca" = {
      ord <- vegan::cca(tbl)                                                 # "ca", "CA$u", "CA$v", "eig", NULL
      res$st_scores  <- ord$CA$u
      res$sp_scores  <- ord$CA$v
      res$eig_val <- ord$CA$eig
      res$d_method <- NULL
      res$results_raw <- ord
    },
    "dca" = {
      ord <- vegan::decorana(tbl)
      res$st_scores  <- ord$rproj                                         # "rproj", "cproj", "evals", NULL
      res$sp_scores  <- ord$cproj
      res$eig_val <- ord$evals
      res$d_method <- NULL
      res$results_raw <- ord
    },
    "pcoa" = {
      x_st <- vegan::vegdist(tbl,    method = d_method) # st
      ord <- labdsv::pco(x_st)
      res$st_scores  <- ord$                                         # "points", "points", "eng", d_method
      res$eig_val <- ord$eig
      res$results_raw[[1]] <- ord
      x_sp <- vegan::vegdist(t(tbl), method = d_method) # sp
      ord <- labdsv::pco(x_sp)
      res$sp_scores  <- ord$points
      res$results_raw[[2]] <- ord
    },
    "nmds" = {
      x_st <- vegan::vegdist(tbl,    method = d_method) # st
      ord <- MASS::isoMDS(x_st)
      res$st_scores  <- ord$points
      res$eig_val <- NULL           # no eigen value
      res$results_raw[[1]] <- ord
      x_sp <- vegan::vegdist(t(tbl), method = d_method) # sp
      ord <- labdsv::pco(x_sp)
      res$sp_scores  <- ord$points
      res$results_raw[[2]] <- ord
    },
    "fspa" = {
      ord <- dave::fspa(tbl, method = d_method, d.rev=0.5, n.groups=3) # st
      res$st_scores  <- ord$newpoints
      res$eig_val <- ord$eig
      res$results_raw[[1]] <- ord
      ord <- dave::fspa(t(tbl), method = d_method, d.rev=0.5, n.groups=3) # sp
      res$sp_scores  <- ord$newpoints
      res$results_raw[[2]] <- ord
    }
  )
  res$ordination_method <- o_method
  res$distance_method <- d_method
  return(res)
}

#' @rdname ordination
#' @export
ord_plot <- function(ord, score = "st_scores", x = 1, y = 2){
  ord_scores <- ord_extract_score(ord, score)
  x <- colnames(ord_scores)[x]
  y <- colnames(ord_scores)[y]
  ord_scores %>%
    ggplot2::ggplot(ggplot2::aes(.data[[x]], .data[[y]], label = rownames(ord_scores))) +
    ggplot2::geom_text() + 
    ggplot2::theme_bw()
}

#' @rdname ordination
#' @export
ord_add_group <- function(ord, score = "st_scores", df, indiv, group){
  cols_add <- cols_one2multi(df, indiv)
  df_add <- 
    df %>%
    dplyr::select(dplyr::any_of(cols_add)) %>%
    dplyr::mutate({{indiv}} := as.character(.data[[indiv]])) %>%
    dplyr::distinct()
  df_grouped <- 
    ord_extract_score(ord, score) %>%
    tibble::rownames_to_column(indiv) %>%
    dplyr::left_join(df_add) %>%
    dplyr::relocate(dplyr::any_of(cols_add), .after = dplyr::last_col())
  rownames(df_grouped) <- df_grouped[[indiv]]
  df_grouped
}

#' @rdname ordination
#' @export
ord_extract_score <- function(ord, score = "st_scores"){
  ord[[score]] %>% # needs "[[" not "["
    as.data.frame()
}
