#' Convert data.frame and table to each other. 
#' 
#' @param df       A data.frame.
#' @param tbl      A table. community matrix.
#'                 rownames: stands.
#'                 colnames: species.
#' @param dist     A distance table.
#' @param st,sp,ab A string.
#' @return
#'     df2table() return table, 
#'     table2df() return data.frame,
#'     dist2df() return data.frame.
#' @examples
#' tibble::tibble(
#'    st = paste0("st_", rep(1:2, times = 2)), 
#'    sp = paste0("sp_", rep(1:2, each = 2)), 
#'    ab = runif(4)) %>%
#'   dplyr::bind_rows(., .) %>%
#'   print() %>%
#'   df2table("st", "sp", "ab")
#' 
#' @export
df2table <- function(df, st = "stand", sp = "species", ab = "abundance"){
  if(!is.numeric(df[[ab]])){
    df[[ab]] <- 1
    message(paste0('non-numeric "', ab, '" was converted into 1 in df2table()'))
  }
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

#' @rdname df2table
#' @export
dist2df <- function(dist){
  dist_col <- "dist"
  tbl <- 
    as.matrix(dist) %>%
    tibble::as_tibble()
  tbl %>%
    dplyr::mutate("plot_1" := colnames(tbl)) %>%
    tidyr::pivot_longer(-dplyr::all_of("plot_1"), names_to = "plot_2", values_to = dist_col) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data[[dist_col]] != 0)
}
