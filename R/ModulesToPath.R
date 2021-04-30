#' ModulesToPath: adds module inforamtion to the output of MapKo_idToPathway
#'
#' Example: id <- c("ko:K00031","ko:K00001","ko:K00002")
#' out <- MapKo_idToPathway(id=id)
#' ModulesToPath(out)
#'
#' @param path output data frame of the MapKo_idToPathway
#' @return dataframe with module inforamtion added to the output of MapKo_idToPathway
#' @export
ModulesToPath <- function(path) {
  df <- as.data.frame(do.call(rbind,strsplit(readLines('http://rest.kegg.jp/link/pathway/module'),'\t',fixed=T)),stringsAsFactors = F)
  df$V2 <- sub("map","ko",df$V2)
  df <- df[df$V2 %in% path$path_id,]
  df <- merge(out,df,by.x="path_id",by.y="V2",all=T)
  #add module names
  df2 <- as.data.frame(do.call(rbind,strsplit(readLines('http://rest.kegg.jp/list/module'),'\t',fixed=T)),stringsAsFactors = F)
  df2 <- df2[df2$V1 %in% df$V1,]
  df <- merge(df2,df,by.x="V1",by.y="V1",all.y = T)
  colnames(df)[1:2] <- c('module_id','module_name')
  df <- df[,c(4,3,5:7,1:2,8)]
  df2 <- NULL
  return(df)
}
