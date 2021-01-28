get_sample_set <- function(resource,
                            limit = 20L,
                            verbose = FALSE,
                            warnings = TRUE,
                            progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_sample_sets(tbl_json)

  return(tidy_tbls)
}

get_sample_sets_by_pss_id <- function(pss_id, limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/sample_set'
  resource_urls <- sprintf("%s/%s", resource, pss_id)

  purrr::map(
    resource_urls,
    get_sample_set,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

get_sample_sets_by_pgs_id <- function(pgs_id, limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/sample_set/search'
  resource_urls <- sprintf("%s?pgs_id=%s", resource, pgs_id)

  purrr::map(
    resource_urls,
    get_sample_set,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}



#' Get PGS Catalog Sample Sets
#'
#' Retrieves sample sets via the PGS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all sample sets that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' associations that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search.
#'
#' @param pss_id A character vector of PGS Catalog sample sets accession
#'   identifiers.
#' @param pgs_id A \code{character} vector of PGS Catalog score accession
#'   identifiers.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how performance metrics retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same sample sets found with different
#'   criteria.
#' @param interactive A logical. If all sample sets are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param progress_bar Whether to show a progress bar indicating download
#'   progress from the REST API server.
#'
#' @return An \linkS4class{sample_sets} object.
#' @examples
#' # TODO
#'
#' @export
get_sample_sets <- function(
  pss_id = NULL,
  pgs_id = NULL,
  set_operation = 'union',
  interactive = TRUE,
  verbose = FALSE,
  warnings = TRUE,
  progress_bar = TRUE) {

  if(!(rlang::is_scalar_character(set_operation) && set_operation %in% c('union', 'intersection')))
    stop("set_operation must be either 'union' or 'intersection'")

  if(!(rlang::is_scalar_logical(verbose) && verbose %in% c(TRUE, FALSE)))
    stop("verbose must be either TRUE or FALSE")

  if(!(rlang::is_scalar_logical(warnings) && warnings %in% c(TRUE, FALSE)))
    stop("warnings must be either TRUE or FALSE")

  list_of_pss <- list()

  if (!rlang::is_null(pss_id))
    list_of_pss[['get_sample_sets_by_pss_id']] <-
    get_sample_sets_by_pss_id(pss_id = pss_id,
                              verbose = verbose,
                              warnings = warnings,
                              progress_bar = progress_bar) %>%
    coerce_to_s4_sample_sets()

  if (!rlang::is_null(pgs_id))
    list_of_pss[['get_sample_sets_by_pgs_id']] <-
    get_sample_sets_by_pgs_id(pgs_id = pgs_id,
                              verbose = verbose,
                              warnings = warnings,
                              progress_bar = progress_bar) %>%
    coerce_to_s4_sample_sets()

  # If no criteria have been passed, i.e. all are NULL then got fetch all
  # Sample Sets.
  if(rlang::is_empty(list_of_pss)) {
      return(coerce_to_s4_sample_sets(NULL))
  } else {

    if (identical(set_operation, "union")) {
      return(purrr::reduce(list_of_pss, union))
    }

    if (identical(set_operation, "intersection")) {
      return(purrr::reduce(list_of_pss, intersect))
    }
  }
}
