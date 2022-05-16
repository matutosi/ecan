#' Convert data.frame and table to each other. 
#' 
#' @param df       A data.frame.
#' @param tbl      A table. community matrix.
#'                 rownames: stands.
#'                 colnames: species.
#' @param st,sp,ab A string.
#' @return  df2table() return table, table2df return data.frame.
#' @examples
#' library(vegan)
#' data(dune)
#' tibble::tibble(st=rep(1:2, 2), sp=rep(1:2, each=2), ab=runif(4)) %>%
#'   dplyr::bind_rows(., .) %>%
#'   df2table("st", "sp", "ab")
#' 
#' @export
df2table <- function(df, st = "stand", sp = "species", ab = "abundance"){
  stopifnot(is.numeric(df$ab))
  df %>%
    dplyr::select(dplyr::all_of(c(st, sp, ab))) %>%
    tidyr::pivot_wider(
      names_from = sp, values_from = ab, 
      values_fill = 0, values_fn = sum) %>%
    tibble::column_to_rownames(var = st)
}

#' @rdname df2table
#' @export
table2df <- function(tbl, st = "stand", sp = "species", ab = "abundance"){
  tbl %>%
    tibble::rownames_to_column(st) %>%
    tidyr::pivot_longer(-!!st, names_to = sp, values_to = ab) %>%
    dplyr::filter(.data[[ab]] != 0)
}
