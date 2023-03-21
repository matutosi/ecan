#' Pad a string to the longest width of the strings.
#' 
#' @param string   Strings.
#' @param side     Side on which padding character is added (left, right or both).
#' @param pad      Single padding character (default is spaces).
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
