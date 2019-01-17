#' update_packages
#'
#' This function updates all CRAN packages installed in your package library.
#' @usage update_packages()
#' @import utils
#' @export

update_packages <- function(){
	options(warn=-1)
  update.packages(ask = FALSE, instlib = .libPaths()[1], repos = "https://cloud.r-project.org")
  options(warn=0)
}