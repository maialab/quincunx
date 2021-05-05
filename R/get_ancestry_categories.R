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

#' Get PGS Catalog Ancestry Categories
#'
#' Retrieves ancestry categories via the PGS Catalog REST API.
#'
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param progress_bar Whether to show a progress bar indicating download
#'   progress from the REST API server.
#'
#' @return Return a tibble of mappings between the ancestry symbols and their
#'   name, e.g. EUR and European, respectively.
#' @examples
#' get_ancestry_categories()
#'
#' @export
get_ancestry_categories <- function(verbose = FALSE,
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
