get_cohort <- function(resource,
                           limit = 20L,
                           verbose = FALSE,
                           warnings = TRUE,
                           progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_cohorts(tbl_json)

  return(tidy_tbls)
}

get_cohort_by_cohort_symbol <- function(cohort_symbol, limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/cohort'
  resource_urls <- sprintf("%s/%s", resource, cohort_symbol)

  purrr::map(
    resource_urls,
    get_cohort,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}


#' Get PGS Catalog Cohorts
#'
#' Retrieves cohorts via the PGS Catalog REST API. The REST API is queried
#' multiple times with the criteria passed as arguments (see below). Please note
#' that all search criteria are vectorised, thus allowing for batch mode search.
#'
#' @param cohort_symbol A cohort symbol.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param progress_bar Whether to show a progress bar indicating download
#'   progress from the REST API server.
#'
#' @return An \linkS4class{cohorts} object.
#' @examples
#' # TODO
#'
#' @export
get_cohorts <- function(
  cohort_symbol = NULL,
  verbose = FALSE,
  warnings = TRUE,
  progress_bar = TRUE) {


  if(!(rlang::is_scalar_logical(verbose) && verbose %in% c(TRUE, FALSE)))
    stop("verbose must be either TRUE or FALSE")

  if(!(rlang::is_scalar_logical(warnings) && warnings %in% c(TRUE, FALSE)))
    stop("warnings must be either TRUE or FALSE")

  if (!rlang::is_null(cohort_symbol)) {
    get_cohort_by_cohort_symbol(
      cohort_symbol = cohort_symbol,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    ) %>%
      coerce_to_s4_cohorts() %>%
      return()

  } else {
    return(coerce_to_s4_cohorts(NULL))
  }
}
