get_score <- function(resource,
                      limit = 20L,
                      verbose = FALSE,
                      warnings = TRUE,
                      progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_scores(tbl_json)

  return(tidy_tbls)
}

get_score_all <- function(limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/score/all'

  get_score(resource = resource,
            limit = limit,
            verbose = verbose,
            warnings = warnings,
            progress_bar = progress_bar)
}

get_score_by_pgs_id <- function(pgs_id, limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/score'
  resource_urls <- sprintf("%s/%s", resource, pgs_id)

  purrr::map(
    resource_urls,
    get_score,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

get_score_by_pubmed_id <- function(pubmed_id, limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/score/search'
  resource_urls <- sprintf("%s?pmid=%s", resource, pubmed_id)

  purrr::map(
    resource_urls,
    get_score,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

get_score_by_trait_id <- function(efo_id, limit = 20L, verbose = FALSE, warnings = TRUE, progress_bar = TRUE) {

  resource <- '/rest/score/search'
  resource_urls <- sprintf("%s?trait_id=%s", resource, efo_id)

  purrr::map(
    resource_urls,
    get_score,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

#' Get PGS Catalog Scores
#'
#' Retrieves polygenic scores via the PGS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all scores that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' associations that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search.
#'
#' @param pgs_id A \code{character} vector of PGS Catalog score accession
#'   identifiers.
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param pubmed_id An \code{integer} vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how scores retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same scores found with different
#'   criteria.
#' @param interactive A logical. If all scores are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param progress_bar Whether to show a progress bar as the queries are
#'   performed.
#'
#' @return An \linkS4class{scores} object.
#' @examples
#' # TODO
#'
#' @export
get_scores <- function(pgs_id = NULL,
                       efo_id = NULL,
                       pubmed_id = NULL,
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

  list_of_scores <- list()

  if (!rlang::is_null(pgs_id))
    list_of_scores[['get_score_by_pgs_id']] <-
    get_score_by_pgs_id(pgs_id = pgs_id,
                        verbose = verbose,
                        warnings = warnings) %>%
    coerce_to_s4_scores()

  if (!rlang::is_null(efo_id))
    list_of_scores[['get_score_by_trait_id']] <-
    get_score_by_trait_id(efo_id = efo_id,
                        verbose = verbose,
                        warnings = warnings) %>%
    coerce_to_s4_scores()

  if (!rlang::is_null(pubmed_id))
    list_of_scores[['get_score_by_pubmed_id']] <-
    get_score_by_pubmed_id(pubmed_id = pubmed_id,
                          verbose = verbose,
                          warnings = warnings) %>%
    coerce_to_s4_scores()

  # If no criteria have been passed, i.e. all are NULL then got fetch all
  # scores.
  if(rlang::is_empty(list_of_scores)) {
    msg1 <- "You are about to download all scores from the PGS Catalog.\nThis might take a while."
    msg2 <- 'Returning an empty scores object!'
    msg3 <- 'OK! Getting all scores then. This is going to take a while...'
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
      return(coerce_to_s4_scores(
        get_score_all(verbose = verbose, warnings = warnings)))
    else
      return(coerce_to_s4_scores(NULL))
  } else {

    if (identical(set_operation, "union")) {
      return(purrr::reduce(list_of_scores, union))
    }

    if (identical(set_operation, "intersection")) {
      return(purrr::reduce(list_of_scores, intersect))
    }
  }

}


