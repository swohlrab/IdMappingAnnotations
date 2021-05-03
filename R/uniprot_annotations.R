#' Retrieve IDs from the uniprot REST API
#'
#' This functions retrives IDs from the uniprot server. It takes as input swiss
#' prot identifers that generated in the trinnoate output table, or that are
#' retrieved form a blast search against the swiss prot database (it also works
#' with Trembl IDs)
#' You have to provide a vector ID names e.g ID = c("MSRA_PSEPW","NAT6_HUMAN")
#' and specifiy your mapping options. A complete list of mapping options is
#' available at https://www.uniprot.org/help/api_idmapping
#' Examples of mapping options (database options):
#' database=c('GENENAME','ACC','EGGNOG_ID','KEGG_ID')
#' This function connects to the uniprot REST API and returns the specified
#' options in the form of a data frame.
#'
#' @param ids vector of uniprot IDs
#' @param database vector of database abbreviations
#' @return A data frame with all requested IDs
#' @export
uniprot_annotations <- function(ids,database) {
  ifelse(length(ids) > 5000, ids <- split(ids, ceiling(seq_along(ids)/5000)),ids <- list(ids))
  parseIDs <- function(x) {
    as.data.frame(httr::content(httr::POST(url = "https://www.uniprot.org/uploadlists/",body = list(from = "ACC", to = "ACC", format = "tab", query = sprintf("%s", paste(noquote(x), collapse = " ")))), type = "text/tab-separated-values",  col_names = TRUE, col_types = NULL, encoding = "ISO-8859-1"))
  }
  dat1 <- do.call(rbind,lapply(ids, parseIDs))
  query <- dat1$To
  ifelse(length(query)>5000,query<-split(query, ceiling(seq_along(query)/5000)),query<-list(query))
  to <- c('ID',database)
  getIDs <- function(to,query){
    ls <- as.list(to)
    fx <- function(x){as.data.frame(httr::content(httr::POST(url='https://www.uniprot.org/uploadlists/',body=list(from="ID", to=x, format='tab',query=sprintf("%s", paste(noquote(query), collapse = " ")))),type = 'text/tab-separated-values', col_names = TRUE, col_types = NULL, encoding = "ISO-8859-1"))}
    total <- lapply(ls,fx)
    names(total) <- to
    return(total)
  }
  dat2 <- lapply(query, function(x) {getIDs(to = to, query = x)})
  dat2 <- unlist(dat2)
  query_match <- dat2[grepl(".From", names(dat2))]
  lto <- dat2[!grepl(".From", names(dat2))]
  ifelse(length(query_match) > 5000, names <- sub("\\..*", "", gsub("^.+?\\.(.*)","\\1", names(query_match))), names <- sub("\\..*", "",names(query_match)))
  dat2 <- as.data.frame(cbind(names, query_match, lto)) #rename Query to someting with a or b
  dat2 <- dat2 %>% spread(names, lto)
  dat2$input <- dat1$From[match(dat2$query_match,dat1$To)]
  dat2 <- dat2[,c(ncol(dat2),1:(ncol(dat2)-1))]
  dat1<-NULL
  query_match<-NULL
  lto<-NULL
  return(dat2)
}
