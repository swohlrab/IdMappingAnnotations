#' MakeKeggReferenceTable: creates a reference table for your Kegg hierachical files
#'
#' This function takes as input a brite ID and convert the hierachical files into a dataframe.
#' The Kegg brite IDs can be retrieved with the helper function brite_ids()
#' Example: MakeKeggReferenceTableFromBrite(brite="br:br08901") returns the pathway calssifications
#'
#' @param brite Kegg brite ID
#' @return A data frame with kegg hierachy files
#' @export
MakeKeggReferenceTableFromBrite <- function(brite) {
  htext <- readLines(paste("http://api.kegg.net/get/",brite,sep=""))
  # collect prefixes
  vb <- gsub("[^[:alnum:]]", "", unique(substring(htext, 1, 1)))
  vb <- vb[!vb==""]
  #make a list for each letter, remove leading NAs and make equal lengths
  x1 <- lapply(vb, function(x){grepl(paste("^",x,sep=""), htext)})
  x1 <- lapply(x1, function(x){ifelse(x==TRUE,htext,"")})
  x1 <- lapply(x1, function(x){ifelse(x=="",NA,x)})
  x1<- lapply(x1,function(x){x[min(which(!is.na(x))):length(x)]})
  x1 <- lapply(x1, `length<-`, max(lengths(x1)))
  # make data frame and fill empty cells
  x1 <- as.data.frame(do.call(cbind,x1))
  x1 <- x1 %>% fill(names(.))
  #remove prefixes
  x1 <- as.data.frame(apply(x1,2,function(x){sub("^.", "",x)}))
  #remove leading white spaces and double entries
  x1 <- as.data.frame(apply(x1,2,function(x){trimws(x)}))
  x1 <- unique(x1)
  x1$ID <- sub("\\s.*","",x1[,ncol(x1)])
  x1 <- as.data.frame(apply(x1,2,function(x){trimws(x)}))
  x1[] <- lapply(x1, as.character)
  return(x1)
}
