#' read_file
#'
#' reads a file into the console line-by-line.
#' @usage read_file(filepath)
#' @param filepath path to file to be read.
#' @export

read_file <- function(filepath){
  con <- file(filepath, "r")
  while(TRUE){
    line <- readLines(con, n = 1)
    if (length(line) == 0){break}
    print(line)
  };close(con)
}