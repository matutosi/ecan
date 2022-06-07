#' Generate vegetation example
#' 
#' Stand, species, and cover are basic. 
#' Layer, height, st_group, are sp_group optional. 
#' 
#' @param n            A numeric to generate no of occurances.
#' @param use_layer    A logical. If FALSE, height_max and ly_list 
#'                     will be omitted.
#' @param height_max   A numeric. The highest layer of samples.
#' @param ly_list,st_list,sp_list,st_group,sp_group
#'                     A string vector. 
#'                     st_group and sp_group are optional (default is NULL).
#'                     Lenght of st_list and st_group should be the same.
#'                     Lenght of sp_list and sp_group should be the same.
#' @param cover_list   A numeric vector.
#' 
#' @return  A dataframe with columns: stand, layer, species, cover, 
#'          st_group and sp_group.
#' 
#' @examples
#' n <- 300
#' height_max <- 20
#' ly_list    <- c("B1", "B2", "S1", "S2", "K")
#' st_list    <- LETTERS[1:9]
#' sp_list    <- letters[1:9]
#' st_group   <- rep(LETTERS[24:26], 3)
#' sp_group   <- rep(letters[24:26], 3)
#' cover_list <- 2^(0:6)
#' gen_example_layer(n = n, use_layer = TRUE,
#'                   height_max = height_max, ly_list = ly_list, 
#'                   st_list  = st_list,  sp_list  = sp_list,
#'                   st_group = st_group, sp_group = sp_group,
#'                   cover_list = cover_list)
#' 
#' @export
gen_example_layer <- function(n = 300, 
                              use_layer  = TRUE,
                              height_max = 20,
                              ly_list    = "",
                              st_list    = LETTERS[1:9],
                              sp_list    = letters[1:9],
                              st_group   = NULL,
                              sp_group   = NULL,
                              cover_list = 2^(0:6)){
  # basic info
  comp <- 
    tibble::tibble(
      stand   = sample(st_list, n, replace = TRUE),
      layer   = sample(ly_list, n, replace = TRUE),
      species = sample(sp_list, n, replace = TRUE),
      cover   = sample(cover_list, size = n, replace = TRUE, 
                       prob = log(2^(length(cover_list):1)))) %>%
    dplyr::group_by(stand, layer, species) %>%
    dplyr::summarise(cover = mean(cover), .groups = "drop")
  # additional info
  species <- tibble::tibble(species = sp_list, sp_group)
  stand <- tibble::tibble(stand = st_list, st_group)
  height <- if( use_layer ){
    sample(1:(height_max * 10) / 10, length(ly_list) * length(st_list))
  } else {
    height_max  # dammy (remove layer and height at last)
  }
  layer <- 
    tibble::tibble(
    stand  = rep(st_list, times = length(ly_list)),
    layer  = rep(ly_list, each  = length(st_list)),
    height = sort(height, decreasing = TRUE))
  df <- 
    comp %>%
    dplyr::left_join(stand) %>%
    dplyr::left_join(layer)  %>%
    dplyr::left_join(species) %>%
    dplyr::arrange(stand, layer, desc(cover))
  if( !use_layer ) df <- dplyr::select(df, -c("layer", "height"))
  return(df)
}
