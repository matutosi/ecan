#' Pad a string to the longest width of the strings.
#' 
#' @param string   Strings.
#' @param side     Side on which padding character is added (left, right or both).
#' @param pad      Single padding character (default is a space).
#' 
#' @return         Strings.
#' @examples
#' x <- c("a", "ab", "abc")
#' pad2longest(x, side = "right", pad = " ")
#' 
#' @export
pad2longest <- function(string, side = "right", pad = " "){
  width <- max(stringr::str_length(string))
  stringr::str_pad(string, width = width, side = side, pad = pad)
}

#' Pad a string to the longest width of the strings.
#' 
#' @param x        A numeric vector.
#' @return         A factor vector.
#' @examples
#' x <- runif(100)
#' cut_conti(x)
#' 
#' @export
cut_conti <- function(x){
  brks <- graphics::hist(x, plot = FALSE)$breaks
  cut(x, brks)
}
