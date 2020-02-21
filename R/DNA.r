#' Reverse Compliment
#'
#' produces the reverse compliment sequence of a DNA strand
#' @usage rev_comp(DNA)
#' @param DNA A string or vector of strings of DNA
#' @return phyloseq-object
#' @export
#' 
rev_comp <- function(DNA){
  DNA <- gsub('A', 'Z', DNA)
  DNA <- gsub('C', 'Y', DNA)
  DNA <- gsub('T', 'A', DNA)
  DNA <- gsub('G', 'C', DNA)
  DNA <- gsub('Z', 'T', DNA)
  DNA <- gsub('Y', 'G', DNA)
  DNA <- unname(sapply(DNA, FUN=function(dna){intToUtf8(rev(utf8ToInt(dna)))}))
  return(DNA)
}


#' Compliment
#'
#' produces the compliment sequence of a DNA strand
#' @usage dna_comp(DNA)
#' @param DNA A string or vector of strings of DNA
#' @return phyloseq-object
#' @export
#' 
dna_comp <- function(DNA){
  DNA <- gsub('A', 'Z', DNA)
  DNA <- gsub('C', 'Y', DNA)
  DNA <- gsub('T', 'A', DNA)
  DNA <- gsub('G', 'C', DNA)
  DNA <- gsub('Z', 'T', DNA)
  DNA <- gsub('Y', 'G', DNA)
  return(DNA)
}