#' classify_ARG_genes
#'
#' Classifies ARGs from the \href{https://card.mcmaster.ca/home}{CARD database} using either accession number or gene name.
#' @usage classify_ARG_genes(genes, obo = NULL)
#' @param genes vector or dataframe column of ARG ARO accession numbers or gene names.
#' @param obo an object processed with \code{\link{processOBO}}.
#' @import stringr
#' @export

classify_ARG_genes <- function(genes, obo = NULL){
  if(!(is.null(obo))){CARD <- obo}
  genes <- data.frame(genes, stringsAsFactors = FALSE)
  classified_genes <- unlist(unname(apply(genes,1,FUN = function(gene){
    if(gene %in% CARD$ID){
      if(is.na(CARD$Resistance[gene])){return('Unclassified')}
      return(CARD$Resistance[gene])
    } else if(gene %in% CARD$Name){gene <- CARD$ID[which(CARD$Name %in% gene)]
      if(is.na(CARD$Resistance[gene])){return('Unclassified')}
      return(CARD$Resistance[gene])   
    } else {return('Unclassified')}
  })))
  return(classified_genes)
}

#' processOBO
#'
#' Converts the aro.obo file from the \href{https://card.mcmaster.ca/home}{CARD database} into a data.table.
#' @usage processOBO(OBO_filepath)
#' @param OBO_filepath filepath to the aro.obo file.
#' @import stringr
#' @export

processOBO <- function(OBO_filepath){
  con <- file(OBO_filepath, "r")
  OBO <- list()
  while(TRUE){
    line <- readLines(con, n = 1)
    if (length(line) == 0){break}
    if(length(grep('id: ARO', line)) > 0){
      ID <- str_split(line, ': ')[[1]][2]
      OBO$ID[ID] <- ID
      OBO$Resistance[ID] <- NA
      OBO$is_a[ID] <- NA
      OBO$part_of[ID] <- NA
      res <- NULL}
    if(length(grep('name: ', line)) > 0){
      name <- str_split(line, ': ')[[1]][2]
      OBO$Name[ID] <- name}
    
    if(length(grep('confers_resistance_to', line)) > 0){
      res <- c(res, str_split(line, '! ')[[1]][2])
      OBO$Resistance[ID] <- gsub(' Antibiotic', '', str_to_title(paste(res, collapse = ', ')))}
    
    if(length(grep('is_a:', line)) > 0){
      if(is.na(OBO$is_a[ID])){OBO$is_a[ID] <- str_split(str_split(line, ' ! ')[[1]][1], ': ')[[1]][2]}}
    if(length(grep(': part_of', line)) > 0){
      if(is.na(OBO$part_of[ID])){OBO$part_of[ID] <- str_split(str_split(line, ' ! ')[[1]][1], 'part_of ')[[1]][2]}}
    if(length(grep(': regulates', line)) > 0){
      if(is.na(OBO$part_of[ID])){OBO$part_of[ID] <- str_split(str_split(line, ' ! ')[[1]][1], 'regulates ')[[1]][2]}}
  };  close(con)
  for(i in 1:length(OBO$ID)){
    gene <- OBO$ID[i]
    for(j in 1:6){
      if(!(is.na(OBO$Resistance[OBO$part_of[gene]])) & is.na(OBO$Resistance[gene])){
        OBO$Resistance[gene] <- OBO$Resistance[OBO$part_of[gene]]
      }
      if(!(is.na(OBO$Resistance[OBO$is_a[gene]])) & is.na(OBO$Resistance[gene])){
        OBO$Resistance[gene] <- OBO$Resistance[OBO$is_a[gene]]
      }
      if(is.na(OBO$Resistance[gene])){
        OBO$part_of[gene] <- OBO$part_of[OBO$part_of[gene]]}
      if(is.na(OBO$Resistance[gene])){
        OBO$is_a[gene] <- OBO$is_a[OBO$is_a[gene]]}
      if(!(is.na(OBO$Resistance[gene]))){break}
    }
  }
  return(OBO)
}

# CARD <- processOBO('data/aro.obo')
# CARD <- processOBO('data/testset.obo')
# unique(classify_ARG_genes(test))
# save(CARD, file = 'R/sysdata.rda')
# 
# OBO_filepath <- 'data/testset.obo'
# OBO_filepath <- 'data/aro.obo'
