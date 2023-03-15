#' Read data from BiSS (Biodiversity Investigaraion Support System) to data frame.
#' 
#' BiSS data is formatted as JSON. 
#' 
#' @paramas path   A string to specify the data.
#' @paramas join   A logical. TRUE: join plot and occurrence, FALSE: do not join.
#' @retrun  A data frame
#' 
#' @examples
#' library(dplyr)
#' # path <- "set file path"
#' path <- "https://raw.githubusercontent.com/matutosi/biodiv/main/man/example.json"
#' read_bis(path)
#' 
#' @export
read_biss <- function(json, join = TRUE){
  biss <- jsonlite::fromJSON(json)
  plot <- data.frame(biss$plot)
  occ  <- data.frame(biss$occ)
  if(join){
    return(dplyr::left_join(plot, occ))
  }else{
    return(list(plot = plot, occ = occ))
  }
}
