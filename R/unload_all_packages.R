#' unload_all_packages
#'
#' Function to unload all library'd packages from your environment.
#' @usage unload_all_packages()
#' @import utils
#' @export

unload_all_packages <- function(){
	options(warn=-1)
	lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
	options(warn=0)
}