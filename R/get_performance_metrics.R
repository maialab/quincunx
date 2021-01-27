get_performance <- function(resource,
                      limit = 20L,
                      verbose = FALSE,
                      warnings = TRUE,
                      progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_performance_metrics(tbl_json)

  return(tidy_tbls)
}


get_performance_all <- function(limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/performance/all'

  get_performance(resource = resource,
            limit = limit,
            verbose = verbose,
            warnings = warnings,
            progress_bar = progress_bar)
}


get_performance_by_ppm_id <- function(ppm_id, limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/performance'
  resource_urls <- sprintf("%s/%s", resource, ppm_id)

  purrr::map(
    resource_urls,
    get_performance,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

get_performance_by_pgs_id <- function(pgs_id, limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/performance/search'
  resource_urls <- sprintf("%s?pgs_id=%s", resource, pgs_id)

  purrr::map(
    resource_urls,
    get_performance,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

#' @export
get_performance_metrics <- function(
  ppm_id = NULL,
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

  list_of_ppm <- list()

  if (!rlang::is_null(ppm_id))
    list_of_ppm[['get_performance_by_ppm_id']] <-
    get_performance_by_ppm_id(ppm_id = ppm_id,
                        verbose = verbose,
                        warnings = warnings,
                        progress_bar = progress_bar) %>%
    coerce_to_s4_performance_metrics()

  if (!rlang::is_null(pgs_id))
    list_of_ppm[['get_performance_by_pgs_id']] <-
    get_performance_by_pgs_id(pgs_id = pgs_id,
                          verbose = verbose,
                          warnings = warnings,
                          progress_bar = progress_bar) %>%
    coerce_to_s4_performance_metrics()

  # If no criteria have been passed, i.e. all are NULL then got fetch all
  # Performance Metrics.
  if(rlang::is_empty(list_of_ppm)) {
    msg1 <- "You are about to download all Performance Metrics from the PGS Catalog.\nThis might take a while."
    msg2 <- 'Returning an empty performance_metrics object!'
    msg3 <- 'OK! Getting all Performance Metrics then. This is going to take a while...'
    if(interactive)
      default_answer = NULL  # i.e., use interactive mode.
    else
      default_answer = 'y'
    if (sure(
      before_question = msg1,
      after_saying_no = msg2,
      after_saying_yes = msg3,
      default_answer = default_answer
    ))
      return(
        coerce_to_s4_performance_metrics(
          get_performance_all(
            verbose = verbose,
            warnings = warnings,
            progress_bar = progress_bar
          ))
      )
    else
      return(coerce_to_s4_performance_metrics(NULL))
  } else {

    if (identical(set_operation, "union")) {
      return(purrr::reduce(list_of_ppm, union))
    }

    if (identical(set_operation, "intersection")) {
      return(purrr::reduce(list_of_ppm, intersect))
    }
  }
}
