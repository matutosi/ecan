#' Helper function for calculating diversity
#' 
#' Calculating diversity indices such as species richness (s), 
#' Shannon's H' (h), Simpson' D (d), Simpson's inverse D (i). 
#' @param df       A data.frame, which has three cols: stand, species, abundance.
#'                 Community matrix should be converted using table2df().
#' @param stand,species,abundance 
#'                 A text to specify each column.
#'                 If NULL, 1st, 2nd, 3rd column will be used.
#' @return    A data.frame.
#'            Including species richness (s), Shannon's H' (h), 
#'            Simpson' D (d), Simpson's inverse D (i). 
#' @examples
#' data(dune, package = "vegan")
#' df <- table2df(dune)
#' shdi(df)
#' 
#' @export
shdi <- function(df, stand = NULL, species = NULL, abundance = NULL){
  stopifnot(is.data.frame(df))
  if(is.null(stand))     stand     <- colnames(df)[1]
  if(is.null(species))   species   <- colnames(df)[2]
  if(is.null(abundance)) abundance <- colnames(df)[3]
  stopifnot(is.numeric(df[[abundance]]))
  df %>%
    dplyr::group_by(.data[[stand]], .data[[species]]) %>%
    dplyr::summarise(`:=`("abundance", sum(.data[[abundance]]))) %>%
    dplyr::summarise(
      s = sum(abundance > 0),
      h = h(abundance),
      d = 1 - d(abundance),
      i = 1 / d(abundance)
    )
}

#' Calculating diversity
#' 
#' @param x,base A numeric vector. 
#' @return A numeric vector.
d <- function(x){
  stopifnot(is.numeric(x))
  sum(x^2) / sum(x)^2 
}

#' @rdname d
h <- function(x, base = exp(1)){ 
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(base))
 -sum( x / sum(x) * log(x / sum(x), base = base)) 
}
