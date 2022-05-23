#' Helper function for Indicator Species Analysis
#' 
#' @inherit      shdi
#' @param     group    A text to specify group column.
#' @param     row_data A logical. 
#'                     TRUE: return row result data of labdsv::indval(). 
#' @return    A data.frame.
#' 
#' @examples
#' library(tidyverse)
#' library(magrittr)
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' df <- 
#'   dune %>%
#'   table2df(st = "stand", sp = "species", ab = "cover") %>%
#'   dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
#' ind_val(df, abundance = "cover", group = "Moisture", row_data = TRUE)
#' ind_val(df, abundance = "cover", group = "Management")
#' ind_val(df, abundance = "cover", group = "Use")
#' ind_val(df, abundance = "cover", group = "Manure")
#' 
#' @export
ind_val <- function(df, stand = NULL, species = NULL, abundance = NULL, group = NULL, row_data = FALSE){
  # check inputs
  stopifnot(is.data.frame(df))
  if(is.null(stand))     stand     <- colnames(df)[1]
  if(is.null(species))   species   <- colnames(df)[2]
  if(is.null(abundance)) abundance <- colnames(df)[3]
  stopifnot(is.numeric(df[[abundance]]))
  if(is.null(group))     stop('Needs "group" input')

  # column setting
  indval <- "ind.val"
  pvalue <-  "p.value"

  # table
  tbl <- df2table(df, st = stand, sp = species, ab = abundance)
  # group
  group_no <- paste0("numeric_", group)
  gr <- 
    tibble::tibble(`:=`({{stand}}, rownames(tbl))) %>%
    dplyr::left_join(
      dplyr::distinct(df, {{stand}} := as.character(.data[[stand]]), .data[[group]]), by = stand
    ) %>%
    dplyr::left_join(
      tibble::tibble({{group}} := unique(df[[group]]), {{group_no}} := seq_along(unique(df[[group]]))), by = group
    )
  res <- labdsv::indval(tbl, gr[[group_no]])  # Species Indicator Analysis
  if(!row_data){
    res <- 
      tibble::tibble(
        {{species}}  := names(res$maxcls), 
        {{group_no}} := res$maxcls, 
        {{indval}}   := res$indcls, 
        {{pvalue}}   := res$pval
      ) %>%
      dplyr::arrange(.data[[group_no]], dplyr::desc(res$ind.val), dplyr::desc(res$p.value))
      gr <- 
        gr %>%
        dplyr::select(!all_of(stand)) %>%
        dplyr::distinct()
      res <- 
        dplyr::left_join(res, gr) %>%
        dplyr::select(all_of(c(group, species, indval, pvalue)))
  }
  return(res)
}
