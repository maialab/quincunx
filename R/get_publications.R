get_publication <- function(resource,
                      limit = 20L,
                      verbose = FALSE,
                      warnings = TRUE,
                      progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_publications(tbl_json)

  return(tidy_tbls)
}


get_publication_all <-
  function(limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {


  resource <- '/rest/publication/all'

  get_publication(resource = resource,
            limit = limit,
            verbose = verbose,
            warnings = warnings,
            progress_bar = progress_bar)
}


get_publication_by_pgp_id <-
  function(pgp_id,
           limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {


  resource <- '/rest/publication'
  resource_urls <- sprintf("%s/%s", resource, pgp_id)

  purrr::map(
    resource_urls,
    get_publication,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}


get_publication_by_pgs_id <-
  function(pgs_id,
           limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {

  resource <- '/rest/publication/search'
  resource_urls <- sprintf("%s?pgs_id=%s", resource, pgs_id)

  purrr::map(
    resource_urls,
    get_publication,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}


get_publication_by_pubmed_id <-
  function(pubmed_id,
           limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {


  resource <- '/rest/publication/search'
  resource_urls <- sprintf("%s?pmid=%s", resource, pubmed_id)

  purrr::map(
    resource_urls,
    get_publication,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}


get_publication_by_author <-
  function(author,
           limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {


  author <- purrr::map_chr(author, utils::URLencode, reserved = TRUE)
  resource <- '/rest/publication/search'
  resource_urls <- sprintf("%s?author=%s", resource, author)

  purrr::map(
    resource_urls,
    get_publication,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

#' Get PGS Catalog Publications
#'
#' Retrieves polygenic publications via the PGS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all publications that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' associations that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search.
#'
#' @param pgp_id A character vector of PGS Catalog publication accession
#'   identifiers.
#' @param pgs_id A \code{character} vector of PGS Catalog score accession
#'   identifiers.
#' @param pubmed_id An \code{integer} vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param author A character vector of author names, any author in the list of
#'   authors in a publication, .e.g. \code{'Mavaddat'}.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how publications retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same publications found with different
#'   criteria.
#' @param interactive A logical. If all publications are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#'
#' @return An \linkS4class{publications} object.
#' @examples
#' # TODO
#'
#' @export
get_publications <- function(pgp_id = NULL,
                             pgs_id = NULL,
                             pubmed_id = NULL,
                             author = NULL,
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

  list_of_publications <- list()

  if (!rlang::is_null(pgp_id))
    list_of_publications[['get_publication_by_pgp_id']] <-
    get_publication_by_pgp_id(pgp_id = pgp_id,
                               verbose = verbose,
                               warnings = warnings) %>%
    coerce_to_s4_publications()

  if (!rlang::is_null(pgs_id))
    list_of_publications[['get_publication_by_pgs_id']] <-
    get_publication_by_pgs_id(pgs_id = pgs_id,
                               verbose = verbose,
                               warnings = warnings) %>%
    coerce_to_s4_publications()

  if (!rlang::is_null(pubmed_id))
    list_of_publications[['get_publication_by_pubmed_id']] <-
    get_publication_by_pubmed_id(pubmed_id = pubmed_id,
                                 verbose = verbose,
                                 warnings = warnings) %>%
    coerce_to_s4_publications()

  if (!rlang::is_null(author))
    list_of_publications[['get_publication_by_author']] <-
    get_publication_by_author(author = author,
                              verbose = verbose,
                              warnings = warnings) %>%
    coerce_to_s4_publications()

  # If no criteria have been passed, i.e. all are NULL then got fetch all
  # publications.
  if(rlang::is_empty(list_of_publications)) {
    msg1 <- "You are about to download all publications from the PGS Catalog.\nThis might take a while."
    msg2 <- 'Returning an empty publications object!'
    msg3 <- 'OK! Getting all publications then. This is going to take a while...'
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
      return(coerce_to_s4_publications(
        get_publication_all(verbose = verbose, warnings = warnings)))
    else
      return(coerce_to_s4_publications(NULL))
  } else {

    if (identical(set_operation, "union")) {
      return(purrr::reduce(list_of_publications, union))
    }

    if (identical(set_operation, "intersection")) {
      return(purrr::reduce(list_of_publications, intersect))
    }
  }

}

