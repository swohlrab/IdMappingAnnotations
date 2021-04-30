#'MapKo_idToPathway: retrieve pathway inforamtion for ko ids
#'
#'This function takes as input a vector of ko ids in the format of ko:KXXXXX and returns a data frame
#'with associated kegg pathway inforamtions. If you also want to have the respective module information,
#'run afterwards the function 'ModulesToPath'
#'
#'Example: id <- id <- c("ko:K00031","ko:K00001","ko:K00002")
#'MapKo_idToPathway(id=id)
#'
#' @param id vector of ko ids in the format ko:KXXXXX
#' @return dataframe with ko ids and associated kegg pathway information
#' @export
MapKo_idToPathway <- function (id) {
  df <-  as.data.frame(do.call(rbind,strsplit(readLines('http://rest.kegg.jp/link/pathway/ko'),'\t',fixed=T)),stringsAsFactors=F)
  #remove all "map" entries
  df <- df[!grepl("map",df$V2),]
  df[] <- lapply(df, as.character)
  #subset
  df <- df[df$V1 %in% id,]
  colnames(df)[2] <- "ID"
  df2 <- MakeKeggReferenceTableFromBrite(brite="br:br08901")
  df2$ID <- paste("path:ko",df2$ID, sep="")
  df <- merge(df,df2,by="ID")
  df2 <- NULL
  #get gene names
  df2 <- as.data.frame(do.call(rbind,strsplit(readLines('http://rest.kegg.jp/list/ko'),'\t',fixed=T)),stringsAsFactors=F)
  df <- merge(df,df2,by.x="V1.x",by.y="V1",all.x = T)
  colnames(df) <- c('ko_id','path_id','path_level_1','path_level_2','path_level_3','gene_name')
  df2 <- NULL
  return(df)
}
