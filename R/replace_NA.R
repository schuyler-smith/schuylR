#' replace_NA
#'
#' Function to replace NA values in numeric columns of a data.table
#' @usage replace_NA(DT, replace_with = 0)
#' @param DT A data.table.
#' @param replace_with value to replace NA with.
#' @export

replace_NA <- function(DT, replace_with = 0){
  for(j in seq_along(DT)){
    set(DT, i = which(is.na(DT[[j]]) & is.numeric(DT[[j]])), j = j, value = replace_with)
  }
  return(DT)
}
