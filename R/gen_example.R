  # library(tidyverse)
  #   # generate example
  # n <- 300
  # ly_list    <- c("B1", "B2", "S1", "S2", "K")
  # st_list    <- LETTERS[1:9]
  # sp_list  <- letters[1:9]
  # st_group   <- rep(LETTERS[24:26], 3)
  # sp_group <- rep(letters[24:26], 3)
  # cover <- sample(2^(0:6), size = n, replace = TRUE, prob = log(2^(7:1)))
  # 
  # layer <- 
  #   tibble::tibble(
  #   stand  = rep(st_list, times = length(ly_list)),
  #   layer  = rep(ly_list, each  = length(st_list)),
  #   height = 
  #     sample(1:100 / 5, length(ly_list) * length(st_list)) %>%
  #     sort(decreasing = TRUE))
  # stand   <- 
  #   tibble::tibble(stand   = st_list,   st_group) %>%
  #   dplyr::left_join(layer)
  # species <- tibble::tibble(species = sp_list, sp_group)
  # comp <- 
  #   tibble::tibble(
  #   stand   = sample(st_list, n, replace = TRUE),
  #   layer   = sample(ly_list, n, replace = TRUE),
  #   species = sample(sp_list, n, replace = TRUE),
  #   cover   = sample(2^(0:6), size = n, replace = TRUE, prob = log(2^(7:1)))) %>%
  #   dplyr::group_by(stand, layer, species) %>%
  #   dplyr::summarise(cover = sum(cover), .groups = "drop") %>%
  #   dplyr::ungroup() %>%
  #   arrange(stand, layer, species)
  # df <- 
  #   comp %>%
  #   dplyr::left_join(stand) %>%
  #   dplyr::left_join(species)
