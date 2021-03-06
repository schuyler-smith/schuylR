#' Converts numeric values to column names in sample_data.
#'
#' Converts numeric values to column names in sample_data.
#' @usage check_numeric_treatment(phyloseq_obj, ...)
#' @param phyloseq_obj A \code{\link[phyloseq]{phyloseq-class}} object. It
#' must contain \code{\link[phyloseq:sample_data]{sample_data()}}) with
#' information about each sample, and it must contain
#' \code{\link[phyloseq:tax_table]{tax_table()}}) with information about each
#' taxa/gene.
#' @param ... Column name as a \code{string} or \code{numeric} in the
#' \code{\link[phyloseq:sample_data]{sample_data}}. This can be any number of
#' multiple columns and they will be combined into a new column.
#' @import phyloseq
#' @return string
#' @export

check_numeric_treatment <- function(phyloseq_obj, ...) {
  treatments <- list(...)
  if (any(unlist(lapply(treatments, is.null))) |
      any(unlist(lapply(treatments, is.na)))) {
    return(NULL)
  } else {
    return(tryCatch({
      unlist(lapply(
        treatments,
        FUN = function(treatment) {
          colnames(access(phyloseq_obj, 'sam_data')[, treatment])
        }
      ))
    }, error = function(e) {
      stop(
        "`treatment` must be at least one column name, or index, from the sample_data()",
        call. = FALSE
      )
    }))
  }
}

#' Converts numeric values to column names in tax_table
#'
#' Converts numeric values to column names in tax_table.
#' @usage check_numeric_classification(phyloseq_obj, ...)
#' @param phyloseq_obj A \code{\link[phyloseq]{phyloseq-class}} object. It
#' must contain \code{\link[phyloseq:sample_data]{sample_data()}}) with
#' information about each sample, and it must contain
#' \code{\link[phyloseq:tax_table]{tax_table()}}) with information about each
#' taxa/gene.
#' @param ... Column name as a \code{string} or \code{numeric} in the
#' \code{\link[phyloseq:tax_table]{tax_table}} for the factor to conglomerate
#' by.
#' @import phyloseq
#' @return string
#' @export

check_numeric_classification <- function(phyloseq_obj, ...) {
  classifications <- list(...)
  if (any(unlist(lapply(classifications, is.null))) |
      any(unlist(lapply(classifications, is.na)))) {
    return(NULL)
  } else {
    return(tryCatch({
      unlist(lapply(
        classifications,
        FUN = function(classification) {
          colnames(access(phyloseq_obj, 'tax_table')[, classification])
        }
      ))
    }, error = function(e) {
      stop(
        "taxa_filter(): `classification` must be at least one column name, or index, from the tax_table()",
        call. = FALSE
      )
    }))
  }
}