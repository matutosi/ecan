#' Check cols one-to-one, or one-to-multi in data.frame
#' 
#' @param df                A data.frame
#' @param col,col_1,col_2   A string to specify a colname.
#' @param inculde_self      A logical. 
#'                          If TRUE, return value including input col.
#' 
#' @return is_one2multi(), is_one2one(), is_multi2multi()  return a logical.
#'         cols_one2multi() returns strings of colnames 
#'         that has one2multi relation to input col.
#'         unique_length() returns a list. 
#' 
#' @examples
#' df <- tibble::tibble(
#'   x     = rep(letters[1:6], each = 1),
#'   x_grp = rep(letters[1:3], each = 2),
#'   y     = rep(LETTERS[1:3], each = 2),
#'   y_grp = rep(LETTERS[1:3], each = 2),
#'   z      = rep(LETTERS[1:3], each = 2),
#'   z_grp  = rep(LETTERS[1:3], times = 2))
#' 
#' unique_length(df, "x", "x_grp")
#' 
#' is_one2one(df, "x", "x_grp")
#' is_one2one(df, "y", "y_grp")
#' is_one2one(df, "z", "z_grp")
#' 
#' @export
is_one2multi <- function(df, col_1, col_2){
  if(is_one2one(df, col_1, col_2)){ return(FALSE) }
  if(is_multi2multi(df, col_1, col_2)){ return(FALSE) }
  return(TRUE)
}

#' @rdname is_one2multi
#' @export
is_one2one <- function(df, col_1, col_2){
  len <- unique_length(df, col_1, col_2)
  if(len$x == len$xy){
    if(len$x == len$y){ return(TRUE) }
  }
  return(FALSE)
}

#' @rdname is_one2multi
#' @export
is_multi2multi <- function(df, col_1, col_2){
  len <- unique_length(df, col_1, col_2)
  if(len$x != len$xy){
    if(len$y != len$xy){ return(TRUE) }
  }
  return(FALSE)
}

#' @rdname is_one2multi
#' @export
cols_one2multi <- function(df, col, inculde_self = TRUE){
  cols <- try({
    cols <- setdiff(colnames(df), col)
    vars <- tibble::tibble(col_1 = col, col_2 = cols)
    cols <- cols[purrr::pmap_lgl(vars, is_one2multi, df = df)]
    if(inculde_self) cols <- c(col, cols)
    cols
  })
  if(inherits(cols, "try-error"))
    cols <- character(0)
  return(cols)
}

#' @rdname is_one2multi
#' @export
select_one2multi <- function(df, col, inculde_self = TRUE){
  cols <- cols_one2multi(df, col, inculde_self)
  df %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::distinct()
}


#' @rdname is_one2multi
#' @export
unique_length <- function(df, col_1, col_2){
  x <- df[[col_1]]
  y <- df[[col_2]]
  xy <- paste0(x, "-", y)
  x  <- length(unique(x))
  y  <- length(unique(y))
  xy <- length(unique(xy))
  return(list(x = x, y = y, xy = xy))
}
