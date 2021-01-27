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

#' @export
get_cohorts <- function(
  cohort_symbol = NULL,
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

  list_of_cohorts <- list()

  if (!rlang::is_null(cohort_symbol))
    list_of_cohorts[['get_cohort_by_cohort_symbol']] <-
    get_cohort_by_cohort_symbol(cohort_symbol = cohort_symbol,
                              verbose = verbose,
                              warnings = warnings,
                              progress_bar = progress_bar) %>%
    coerce_to_s4_cohorts()

  # There is no endpoint to fetch all cohorts at the moment
  if(rlang::is_empty(list_of_cohorts)) {
    return(coerce_to_s4_cohorts(NULL))
  } else {

    if (identical(set_operation, "union")) {
      return(purrr::reduce(list_of_cohorts, union))
    }

    if (identical(set_operation, "intersection")) {
      return(purrr::reduce(list_of_cohorts, intersect))
    }
  }
}
