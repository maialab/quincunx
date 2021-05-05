get_ancestry_category <- function(resource,
                               limit = 20L,
                               verbose = FALSE,
                               warnings = TRUE,
                               progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_ancestry_categories(tbl_json)

  return(tidy_tbls)
}

get_ancestry_categories_all <- function(limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/ancestry_categories/'

  get_ancestry_category(resource = resource,
                     limit = limit,
                     verbose = verbose,
                     warnings = warnings,
                     progress_bar = progress_bar)
}

#' Get PGS Catalog Ancestry Symbol Mappings
#'
#' Retrieves the mappings between the ancestry class symbols and ancestry class
#' via the PGS Catalog REST API. Note: this function is not exported and should
#' only be used for debugging reasons. Use in alternative
#' \code{\link{get_ancestry_categories}}.
#'
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param progress_bar Whether to show a progress bar indicating download
#'   progress from the REST API server.
#'
#' @return Return a tibble of mappings between the ancestry symbols and their
#'   name, e.g. EUR and European, respectively.
#'
#' @keywords internal
get_ancestry_symbol_mappings <- function(verbose = FALSE,
                                    warnings = TRUE,
                                    progress_bar = TRUE) {
  if (!(rlang::is_scalar_logical(verbose) &&
        verbose %in% c(TRUE, FALSE)))
    stop("verbose must be either TRUE or FALSE")

  if (!(rlang::is_scalar_logical(warnings) &&
        warnings %in% c(TRUE, FALSE)))
    stop("warnings must be either TRUE or FALSE")

  get_ancestry_categories_all(verbose = verbose,
                              warnings = warnings,
                              progress_bar = progress_bar) %>%
    return()
}

#' Get PGS Catalog Ancestry Categories and Classes
#'
#' Retrieves Ancestry Categories and Classes. This function simply returns the
#' object \code{\link{ancestry_categories}}.
#'
#' @return A tibble with ancestry categories, classes and associated
#'   information. See  \code{\link{ancestry_categories}} for details about each
#'   column.
#'
#' @export
get_ancestry_categories <- function() {
  ancestry_categories
}
