#' Export a PGS Catalog object to xlsx
#'
#' This function exports a PGS Catalog object to Microsoft Excel xlsx file. Each
#' table (slot) is saved in its own sheet.
#'
#' @param x A \linkS4class{scores}, \linkS4class{publications},
#'   \linkS4class{traits}, \linkS4class{performance_metrics},
#'   \linkS4class{sample_sets}, \linkS4class{cohorts},
#'   \linkS4class{trait_categories} or \linkS4class{releases} object.
#' @param file A file name to write to.
#'
#' @return No return value, called for its side effect.
#'
#' @export
write_xlsx <- function(x, file = stop('`file` must be specified')) {
  lst <- s4_to_list(x)
  writexl::write_xlsx(lst, path = file)
}
