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

  if (!rlang::is_null(trait_term))
    list_of_traits[['get_trait_by_trait_term']] <-
    get_trait_by_trait_term(
      trait_term = trait_term,
      include_children = include_children,
      exact_term = exact_term,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    ) %>%
    coerce_to_s4_traits()

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
