#' Check cols one-to-one, or one-to-multi in data.frame
#' 
#' @param df                A data.frame
#' @param col,col_1,col_2   A string to specify a colname.
#' 
#' @return is_one2multi() and is_one2one() return a logical.
#'         cols_one2multi() returns strings of colnames 
#'         that has one2multi relation to input col.
#' 
#' @examples
#' cols_one2multi(example, "stand")
#' cols_one2multi(example, "species")
#' df <- example; col <- "stand"
#' is_one2multi(example, "stand", "Use")
#' is_one2one(example, "stand", "Use")
#' is_one2one(DF, "A", "B")
#' 
#' 
#' 
#' @export
is_one2multi <- function(df, col_1, col_2){
  res <- 
    df %>%
    dplyr::distinct(.data[[col_1]], .data[[col_2]]) %>%
    dplyr::group_by(.data[[col_1]]) %>%
    dplyr::tally() %>%
    dplyr::summarise(max(.data[["n"]])) %>%
    as.numeric()
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
cols_one2multi <- function(df, col){
  cols <- setdiff(colnames(df), col)
  vars <- tibble::tibble(col_1 = col, col_2 = cols)
  cols[purrr::pmap_lgl(vars, is_one2multi, df = df)]
}
