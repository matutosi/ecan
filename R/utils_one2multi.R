#' Check cols one-to-one, or one-to-multi in data.frame
#' 
#' @param df                A data.frame
#' @param col,col_1,col_2   A string to specify a colname.
#' @param inculde_self      A logical. 
#'                          If TRUE, return value including input col.
#' 
#' @return is_one2multi() and is_one2one() return a logical.
#'         cols_one2multi() returns strings of colnames 
#'         that has one2multi relation to input col.
#' 
#' @examples
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
#' is_one2one(df, "stand", "Use")
#' is_one2multi(df, "stand", "Use")
#' cols_one2multi(df, "stand")
#' cols_one2multi(df, "species")
#' select_one2multi(df, "stand")
#' select_one2multi(df, "species")
#' 
#' 
#' @export
is_one2multi <- function(df, col_1, col_2){
  res <- 
  #   try(silent = TRUE, {
      df %>%
      dplyr::distinct(.data[[col_1]], .data[[col_2]]) %>%
      dplyr::group_by(.data[[col_1]]) %>%
      dplyr::tally() %>%
      dplyr::summarise(max(.data[["n"]])) %>%
      as.numeric()
  #   })
  return(res == 1)
}

#' @rdname is_one2multi
#' @export
is_one2one <- function(df, col_1, col_2){
  res_1 <- is_one2multi(df, col_1, col_2)
  res_2 <- is_one2multi(df, col_2, col_1)
  return(res_1 + res_2 == 2)
}

#' @rdname is_one2multi
#' @export
cols_one2multi <- function(df, col, inculde_self = TRUE){
  cols <- setdiff(colnames(df), col)
  vars <- tibble::tibble(col_1 = col, col_2 = cols)
  cols <- cols[purrr::pmap_lgl(vars, is_one2multi, df = df)]
  if(inculde_self) cols <- c(col, cols)
  return(cols)
}

#' @rdname is_one2multi
#' @export
select_one2multi <- function(df, col, inculde_self = TRUE){
  cols <- cols_one2multi(df, col, inculde_self)
  dplyr::select(df, all_of(cols))
}
