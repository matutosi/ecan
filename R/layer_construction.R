#' Draw layer construction plot
#' 
#' @param df    A dataframe including columns: stand, layer height and cover.
#'              Optional column: stand group.
#' @param stand,height,cover,group
#'              A string to specify stand, height, cover, group column.
#' @param ...   Extra arguments for geom_bar().
#' @param x     A numeric vector.
#' @return  draw_layer_construction() returns gg object, 
#'          add_mid_p_bin_w() returns dataframe including mid_point and 
#'          bin_width columns. 
#'          mid_point() and bin_width() return a numeric vector.
#' 
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' n <- 100
#' height_max <- 20
#' ly_list    <- c("B1", "B2", "S1", "S2", "K")
#' st_list    <- LETTERS[1:3]
#' sp_list    <- letters[1:9]
#' st_group   <- NULL
#' sp_group   <- rep(letters[24:26], 3)
#' cover_list <- 2^(0:6)
#' df <- 
#'   gen_example(n = n, use_layer = TRUE,
#'               height_max = height_max, ly_list = ly_list, 
#'               st_list  = st_list,  sp_list  = sp_list,
#'               st_group = st_group, sp_group = sp_group,
#'               cover_list = cover_list)
#' 
#' # select stand and summarise by sp_group
#' df %>%
#'   dplyr::filter(stand == "A") %>%
#'   dplyr::group_by(height, sp_group) %>%
#'   dplyr::summarise(cover = sum(cover), .groups = "drop") %>%
#'   draw_layer_construction(group = "sp_group", colour = "white") + 
#'     ggplot2::theme_bw()
#' 
#' @export
draw_layer_construction <- function(df, 
                                    stand    = "stand", 
                                    height   = "height", 
                                    cover    = "cover",
                                    group    = "",
                                    ...){
  gg <- 
    add_mid_p_bin_w(df, height) %>%
    ggplot2::ggplot(ggplot2::aes(
                    x = .data[[cover]], y = .data[["mid_point"]], 
                    width = .data[["bin_width"]], 
                    group = if(group == "") "" else .data[[group]], 
                    fill  = if(group == "") "" else .data[[group]])) +
    ggplot2::geom_bar(stat = "identity", 
             position = "stack", 
             orientation = "y", ...)
  return(gg)
}

#' Add mid point and bin width of layer heights.
#' 
#' @rdname draw_layer_construction
#' @export
add_mid_p_bin_w <- function(df, height = "height"){
  h     <- sort(unique(df[[height]]))
  tibble::tibble(height = h, 
                 mid_point = mid_point(h), 
                 bin_width = bin_width(h)) %>%
    dplyr::left_join(df)
}

#' Compute mid point of layer heights.
#' 
#' @rdname draw_layer_construction
#' @export
mid_point <- function(x){
  purrr::map2(x, dplyr::lag(x, default = 0), c) %>%
  purrr::map_dbl(mean)
}

#' Compute bin width of layer heights.
#' 
#' @rdname draw_layer_construction
#' @export
bin_width <- function(x){
  purrr::map2_dbl(x, dplyr::lag(x, default = 0), function(.x, .y) .x - .y)
}
