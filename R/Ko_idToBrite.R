#'Ko_idToBrite: retrieve brite inforamtion for ko ids
#'
#'This function takes as input a vector of ko ids in the format of ko:KXXXXX and returns a data frame
#'with associated kegg brite inforamtions. The brite information do not include the pathway mappings.
#'Pathway mappings can be retrieved with the function MapKo_idToPathway
#'The function takes a little bit to run, it needs to download and format different datasets
#'
#'Example: id <- id <- c("ko:K00031","ko:K00001","ko:K00002")
#'Ko_idToBrite(id=id)
#'
#' @param id vector of ko ids in the format ko:KXXXXX
#' @return dataframe with ko ids and associated kegg brite information
#' @export
Ko_idToBrite <- function (id) {
  df <- MakeKeggReferenceTableFromBrite(brite="br:ko00001")
  g <- c('09180 Brite Hierarchies','09190 Not Included in Pathway or Brite')
  df <- df[df$V1 %in% g,]
  g <- NULL
  df$ID <- paste('ko:',df$ID,sep='')
  df <- df[df$ID %in% id,]
  return(df)
}
