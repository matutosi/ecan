#' Read data from BISS (Biodiversity Investigaraion Support System) to data frame.
#' 
#' BISS data is a string based on JSON. 
#'   The string includes 4 parts as shown below. 
#'   Each part is JSON format.
#'   c_names: Column names of table, which will be used for making th.
#'   d_types: Data types of each column for judging the td and input types.
#'   selects: Select options for 'select-one' element. null for other types.
#'   t_data : Table data for making td values or innnerHTML.
#' 
#' @paramas path    A string to specify the data.
#' @paramas convert A logical.
#' @retrun  A data frame
#' 
#' @examples
#' library(tidyverse)
#' library(jsonlite)
#' # path <- "set file path"
#' path <- "D:/matu/work/ToDo/biodiv/R/input_occ_example_2022_07_07_20_04_11.txt"
#' read_bis(path)
#' 
#' @export
read_bis <- function(path, convert = TRUE){
  input_name <- c("c_names", "c_types", "selects", "data")
  occ <- 
    readr::read_tsv(path, col_names = "input", show_col_types = FALSE) %>%
    tidyr::separate(input, into = input_name, sep=";")
  occ <- 
    c("c_names", "c_types" , "selects", "data") %>%
    map(~`[`(occ, .)) %>%
    map(~`[[`(., 1)) %>%
    map(jsonlite::fromJSON)
  c_names <- occ[[1]][[1]]
  c_types <- occ[[2]][[1]]
  #   occ[[3]][[1]]  # not for use
  data    <- occ[[4]]
  df <- 
    data.frame(data) %>%
    tibble::tibble()
  if(convert){
    col_num  <- (c_types == "number") | (c_names %in% c("locLat", "locLon", "locAcc", "No"))
    col_date <- (c_names == "Date")
    df <- 
      df %>%
      dplyr::mutate_if(col_num, as.numeric) %>%
      dplyr::mutate_if(col_date, lubridate::ymd_hms)
  }
  df <- 
    df %>%
    dplyr::select(dplyr::all_of(c_names[c_types != "button"]))
  return(df)
}
