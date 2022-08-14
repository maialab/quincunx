get_trait <- function(resource,
                      limit = 20L,
                      verbose = FALSE,
                      warnings = TRUE,
                      progress_bar = TRUE) {


  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_traits(tbl_json)

  return(tidy_tbls)
}

get_trait_all <-
  function(limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {


  resource <- '/rest/trait/all'

  get_trait(resource = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)
}

get_trait_by_efo_id <-
  function(efo_id,
           include_children = FALSE,
           limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {


  efo_id <- purrr::map_chr(efo_id, utils::URLencode, reserved = TRUE)

  resource <- '/rest/trait'
  flag <- ifelse(include_children, '1', '0')
  resource_urls <- sprintf("%s/%s?include_children=%s", resource, efo_id, flag)

  purrr::map(
    resource_urls,
    get_trait,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

get_trait_by_trait_term <-
  function(trait_term,
           include_children = FALSE,
           exact_term = TRUE,
           limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {

  trait_term <- purrr::map_chr(trait_term, utils::URLencode, reserved = TRUE)
  include_children_flag <- ifelse(include_children, '1', '0')
  exact_term_flag <- ifelse(exact_term, '1', '0')

  resource <- '/rest/trait/search'
  resource_urls <-
    sprintf(
      "%s?term=%s&include_children=%s&exact=%s",
      resource,
      trait_term,
      include_children_flag,
      exact_term_flag
    )

  purrr::map(
    resource_urls,
    get_trait,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>%
    purrr::pmap(dplyr::bind_rows)
}

#' Get PGS Catalog Traits
#'
#' Retrieves traits via the PGS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all traits that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' traits that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search.
#'
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param trait_term A character vector of terms to be matched against trait
#'   identifiers (\code{efo_id}), trait descriptions, synonyms thereof,
#'   externally mapped terms, or even trait categories.
#' @param exact_term A logical value, indicating whether to match the
#'   \code{trait_term} exactly (\code{TRUE}) or not (\code{FALSE}).
#' @param include_children A logical value, indicating whether to include child
#'   traits or not.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how performance metrics retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same sample sets found with different
#'   criteria.
#' @param interactive A logical. If all traits are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param progress_bar Whether to show a progress bar indicating download
#'   progress from the REST API server.
#'
#' @return A \linkS4class{traits} object.
#' @examples
#' \dontrun{
#' # Get a trait by its EFO identifier
#' get_traits(efo_id = 'EFO_0004631')
#'
#' # Get a trait by matching a term in EFO identifier (`efo_id`), label,
#' # description synonyms, categories, or external mapped terms
#' get_traits(trait_term = 'stroke', exact_term = FALSE)
#'
#' # Get a trait matching its name (`trait`) exactly (default)
#' get_traits(trait_term = 'stroke', exact_term = TRUE)
#'
#' # Get traits, excluding its children traits (default)
#' get_traits(trait_term = 'breast cancer')
#'
#' # Get traits, including its children traits (check column `is_child` for
#' # child traits)
#' get_traits(trait_term = 'breast cancer', include_children = TRUE)
#' }
#' @export
get_traits <- function(efo_id = NULL,
                       trait_term = NULL,
                       exact_term = TRUE,
                       include_children = FALSE,
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

  list_of_traits <- list()

  if (!rlang::is_null(efo_id))
    list_of_traits[['get_trait_by_efo_id']] <-
    get_trait_by_efo_id(
      efo_id = efo_id,
      include_children = include_children,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    ) %>%
    coerce_to_s4_traits()

  list_of_traits[['get_trait_by_trait_term']] <-
    if (!rlang::is_null(trait_term)) {
      traits_wo_children <- get_trait_by_trait_term(
        trait_term = trait_term,
        include_children = FALSE,
        exact_term = exact_term,
        verbose = verbose,
        warnings = warnings,
        progress_bar = progress_bar
      ) %>%
        coerce_to_s4_traits()

      if (identical(include_children, FALSE)) {
        traits_wo_children
      } else {
        # Now get all traits, one by one, including children
        get_trait_by_efo_id(
          efo_id = traits_wo_children@traits$efo_id,
          include_children = TRUE,
          verbose = verbose,
          warnings = warnings,
          progress_bar = progress_bar
        ) %>%
          coerce_to_s4_traits()
      }
    }

  # If no criteria have been passed, i.e. all are NULL then gotta fetch all
  # traits.
  if(rlang::is_empty(list_of_traits)) {
    msg1 <- "You are about to download all traits from the PGS Catalog.\nThis might take a while."
    msg2 <- 'Returning an empty traits object!'
    msg3 <- 'OK! Getting all traits then. This is going to take a while...'
    if(interactive)
      default_answer = NULL  # i.e., use interactive mode.
    else
      default_answer = 'y'
    if (sure(
      before_question = msg1,
      after_saying_no = msg2,
      after_saying_yes = msg3,
      default_answer = default_answer
    )) {
      if (identical(include_children, FALSE)) {
        return(coerce_to_s4_traits(
          get_trait_all(verbose = verbose,
                         warnings = warnings,
                         progress_bar = progress_bar)))
      } else {
        # Get first all traits without children
        all_traits_wo_children <- get_trait_all(verbose = verbose, warnings = warnings)
        # Now get all traits, one by one, including children
        get_trait_by_efo_id(efo_id = all_traits_wo_children$traits$efo_id,
                            include_children = TRUE,
                            verbose = verbose,
                            warnings = warnings,
                            progress_bar = progress_bar) %>%
          coerce_to_s4_traits() %>%
          return()
      }
    }
    else
      return(coerce_to_s4_traits(NULL))
  } else {

    if (identical(set_operation, "union")) {
      return(purrr::reduce(list_of_traits, union))
    }

    if (identical(set_operation, "intersection")) {
      return(purrr::reduce(list_of_traits, intersect))
    }
  }

}
