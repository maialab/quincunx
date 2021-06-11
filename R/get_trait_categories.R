get_trait_category <- function(resource,
                      limit = 20L,
                      verbose = FALSE,
                      warnings = TRUE,
                      progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_trait_categories(tbl_json)

  return(tidy_tbls)
}

get_trait_category_all <- function(limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/trait_category/all'

  get_trait_category(resource = resource,
            limit = limit,
            verbose = verbose,
            warnings = warnings,
            progress_bar = progress_bar)
}

#' Get PGS Catalog Trait Categories
#'
#' Retrieves trait categories via the PGS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all trait categories that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' associations that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search.
#'
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param progress_bar Whether to show a progress bar indicating download
#'   progress from the REST API server.
#'
#' @return An \linkS4class{trait_categories} object.
#' @examples
#' # Coming soon...
#'
#' @export
get_trait_categories <- function(verbose = FALSE,
                                 warnings = TRUE,
                                 progress_bar = TRUE) {
  if (!(rlang::is_scalar_logical(verbose) &&
        verbose %in% c(TRUE, FALSE)))
    stop("verbose must be either TRUE or FALSE")

  if (!(rlang::is_scalar_logical(warnings) &&
        warnings %in% c(TRUE, FALSE)))
    stop("warnings must be either TRUE or FALSE")

  get_trait_category_all(verbose = verbose,
                         warnings = warnings,
                         progress_bar = progress_bar) %>%
    coerce_to_s4_trait_categories() %>%
    return()

}

