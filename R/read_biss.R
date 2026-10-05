#' Read data from BiSS (Biodiversity Investigation Support System) to data frame.
#' 
#' BiSS data is formatted as JSON. 
#' 
#' @param txt    A JSON string, URL or file.
#' @param join   A logical. TRUE: join plot and occurrence, FALSE: do not join.
#' @return  A data frame.
#' 
#' @examples
#' path <- system.file("extdata", "biss_example.json", package = "ecan")
#' read_biss(path)
#' 
#' @export
read_biss <- function(txt, join = TRUE){
  biss <- jsonlite::fromJSON(txt)
  plot <- data.frame(biss$plot)
  occ  <- data.frame(biss$occ)
  if(join){
    return(dplyr::left_join(plot, occ, by = intersect(colnames(plot), colnames(occ))))
  }else{
    return(list(plot = plot, occ = occ))
  }
}
