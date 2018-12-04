#' row_match
#'
#' This function checks if the rows in a dataframe are contained in the other.
#' Generally most useful for testing if the outputs of 2 functions are =.
#' @usage row_match(dataframe_1, dataframe_2, results = "in")
#' @param dataframe_1 A dataframe.
#' @param dataframe_2 A dataframe..
#' @param results what the function should return; 'in', 'not_in', 'num_in', 'num_not_in'
#' @keywords row_match
#' @export

row_match <- function(dataframe_1, dataframe_2, results = "in"){
	if(results == 'in'){
		return(do.call(paste0, dataframe_1) %in% do.call(paste0, dataframe_2))
	}
	if(results == 'not_in'){
		return(!(do.call(paste0, dataframe_1) %in% do.call(paste0, dataframe_2)))
	}
	if(results == 'num_in'){
		sum(return(do.call(paste0, dataframe_1) %in% do.call(paste0, dataframe_2)))
	}
	if(results == 'num_not_in'){
		return(sum(!(do.call(paste0, dataframe_1) %in% do.call(paste0, dataframe_2))))
	}
}