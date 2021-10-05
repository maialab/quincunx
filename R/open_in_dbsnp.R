#' Browse dbSNP from SNP identifiers.
#'
#' This function launches the web browser at dbSNP and opens a tab for each SNP
#' identifier.
#'
#' @param variant_id A variant identifier, a character vector.
#'
#' @return Returns \code{TRUE} if successful. Note however that this
#' function is run for its side effect.
#' @examplesIf interactive()
#' open_in_dbsnp('rs56261590')
#'
#' @export
open_in_dbsnp <- function(variant_id) {

  if (!(rlang::is_character(variant_id) ))
    stop("variant_id must be a character vector.")

  if (interactive()) {
    urls <-
      glue::glue("https://www.ncbi.nlm.nih.gov/snp/{variant_id}")

    purrr::walk(urls, utils::browseURL)

    return(invisible(TRUE))
  } else {
    return(invisible(TRUE))
  }
}
