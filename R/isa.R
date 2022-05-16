#' Helper function for Indicator Species Analysis
#' 
#' @inherit      shdi
#' @param group  A text to specify group column.
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
#' isa(df, abundance = "cover", group = "Moisture", row_data = TRUE)
#' isa(df, abundance = "cover", group = "Management")
#' isa(df, abundance = "cover", group = "Use", row_data = TRUE)
#' isa(df, abundance = "cover", group = "Manure")
#' 
#' @export
isa <- function(df, stand = NULL, species = NULL, abundance = NULL, group = NULL, row_data = FALSE){
  # check inputs
  stopifnot(is.data.frame(df))
  if(is.null(stand))     stand     <- colnames(df)[1]
  if(is.null(species))   species   <- colnames(df)[2]
  if(is.null(abundance)) abundance <- colnames(df)[3]
  stopifnot(is.numeric(df[[abundance]]))
  if(is.null(group))     stop('Needs "group" input')
  # table
  tbl <- df2table(df, st = stand, sp = species, ab = abundance)
  # group
  group_no <- paste0("numeric_", group)
  gr <- 
    tibble::tibble(`:=`({{stand}}, rownames(tbl))) %>%
    dplyr::left_join(
      dplyr::distinct(df, .data[[stand]], .data[[group]]), by = stand
    ) %>%
    dplyr::left_join(
      tibble::tibble({{group}} := unique(df[[group]]), {{group_no}} := seq_along(unique(df[[group]]))), by = group
    )
  res <- labdsv::indval(tbl, gr[[group_no]])  # Species Indicator Analysis
  if(!row_data){
    res <- 
      tibble::tibble(
        {{species}} := names(res$maxcls), 
        {{group}}   := res$maxcls, 
        "ind.val"   := res$indcls, 
        "p.value"   := res$pval
      ) %>%
      dplyr::arrange(.data[[group]], desc(res$ind.val), desc(res$p.value))
  }
  return(res)
}
