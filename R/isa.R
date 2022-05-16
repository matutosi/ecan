#' Helper function for Indicator Species Analysis
#' 
#' @inherit      shdi
#' @param group  A text to specify group column.
#' @return    A data.frame.
#' 
#' @examples
#' library(tidyverse)
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' df <- 
#'   table2df(dune) %>%
#'   dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
#' df
#'   
#'   
#' 
#' 
#' @export

df %>%
  prep.isa(community="Management")
isa <- function(df, stand = NULL, species = NULL, abundance = NULL, group = NULL){
  stopifnot(is.data.frame(df))
  if(is.null(stand))     stand     <- colnames(df)[1]
  if(is.null(species))   species   <- colnames(df)[2]
  if(is.null(abundance)) abundance <- colnames(df)[3]
  stopifnot(is.numeric(df[[abundance]]))
  if(is.null(group))     stop('Needs "group" input')
  df %>%
    dplyr::select(all_of(stand, species, abundance, group)) %>%
    distinct() %>%
  
  
}

source("d:/matu/work/stat/r/matu2.r")	# MATSUMURA Toshikazu が作成した一般的に利用できる
df$cover <- df$abundance

## Indicator Spcies Analysis を簡単に実行する関数
isa <- function(df, row.data=F){	# $stand, $species, $cover, $community
	table <- tapply(df$cover, list(df$stand, df$species), sum)	# make compositional table
	table[is.na(table)] <- 0
	table <- data.frame(table)	#  to be coerced to data.frame
	name.table <- colnames(table)	# original names of species
	colnames(table) <- as.numeric(factor(colnames(table)))	# numerical names of species
	names.table <- data.frame(species=name.table, numeric.sp=colnames(table))
	com <- unique(subset(df, select=c(stand, community)))$community	# unique stand and community
	com <- as.numeric(com)	# to be coerced to numeric
	res <- labdsv::indval(table, com)	# Species Indicator Analysis
	if(!row.data){
		res <- data.frame(numeric.sp=names(res$maxcls), community=res$maxcls, ind.val=round(res$indcls,3), p.value=round(res$pval,4))
		res <- merge(names.table, res)
		res <- res[order(res$community, res$ind.val, res$p.value, decreasing=T),][,-1]
	}
	return(res)
}
