#'Functions returns kegg brite ids
#'
#'Just type "brite_ids()" to get brite ids and names of all hierachical files in kegg
#'
#' @param ()
#' @return kegg brite ids
#' @export
brite_ids <- function() {as.data.frame(do.call(rbind,strsplit(readLines('http://rest.kegg.jp/list/brite'),'\t',fixed=T)))}
