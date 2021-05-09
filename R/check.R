#' check is a prelim function for Kostas
#'
#' This function takes as input a list of ko ids in the format of kegg genes and returns a list!
#'
#' Example: ids <- as.list(c("hsa:5053","cfr:102516611","lve:103079765"))
#' out <- as.data.frame(do.call(rbind,lapply(ids, check)))
#'
#' @param ids list of ko genes
#' @return list with kegg genes and respective kos
#' @export
check <- function(x){
  input <- paste0('http://rest.kegg.jp/link/ko/',x)
  df <- as.data.frame(do.call(rbind,strsplit(readLines(input),'\t',fixed=T)),stringsAsFactors=F)
  return(df)
}

