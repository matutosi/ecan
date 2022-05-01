#' Convert inputs into a list.
#' @param ... Vectors or a list.
#' @export
dots2list <- function(...){
  res <- list(...)
  if(length(res) == 0) return(NULL)
  res[sapply(res, is.null)] <- NULL # remove NULL
  if (length(res) == 1) res <- res[[1]]
  return(res)
}
