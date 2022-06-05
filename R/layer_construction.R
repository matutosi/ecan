
draw_layer_construction <- function(df, 
                                    stand    = "stand", 
                                    height   = "height", 
                                    cover    = "cover",
                                    group    = "",
                                    ...){
  gg <- 
  add_mid_p_bin_w(df, height) %>%
  ggplot(aes(x = .data[[cover]], y = .data[["mid_point"]], 
         width = .data[["bin_width"]], 
         group = if(group == "") "" else .data[[group]], 
         fill  = if(group == "") "" else .data[[group]]
         )) +
    geom_bar(stat="identity", 
             position = "stack", 
             orientation = "y", ...)
  gg
}

  # select a stand
df %>%
  dplyr::filter(stand == "C") %>%
  dplyr::group_by(height, sp_group) %>%
  dplyr::summarise(cover = sum(cover), .groups = "drop") %>%
  draw_layer_construction()



#' Compute mid point and width from height of layers. 
#' height: c(2, 4, 8, 20)
#' mid_point: c(1, 3, 6, 14)
#' bin_width: c(2, 2, 4, 12)
add_mid_p_bin_w <- function(df, height = "height"){
  h     <- sort(unique(df[[height]]))
  tibble::tibble(height = h, 
                 mid_point = mid_point(h), 
                 bin_width = bin_width(h)) %>%
    dplyr::left_join(df)
}

mid_point <- function(x){
  purrr::map2(x, dplyr::lag(x, default = 0), c) %>%
  purrr::map_dbl(mean)
}

bin_width <- function(x){
  purrr::map2_dbl(x, dplyr::lag(x, default = 0), function(.x, .y) .x - .y)
}












df_c %>%
  dplyr::group_by(stand, height, sp_group) %>%
  dplyr::summarise(cover = sum(cover), .groups = "drop") %>%
  draw_layer_construction()





df_c %>%
  dplyr::group_by(stand, height, species_group) %>%
  dplyr::summarise(cover = sum(cover), .groups = "drop") %>%
  dplyr::distinct(height) %>%
  dplyr::mutate(
    min = dplyr::lag(height, default = 0),
    max = height,
    mid_point = (min + max) / 2, 
    width = max - min) %>%
  dplyr::select(height, mid_point, width) %>%
  left_join(df_c) %>%
  print(n = nrow(.)) %>%
  ggplot(aes(x = cover, y = mid_point, width = width, group = species_group, fill = species_group)) +
    geom_bar(stat="identity", position = "stack", orientation = "y", colour = "black")

