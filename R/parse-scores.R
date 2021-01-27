as_tidy_tables_scores <- function(tbl_json) {

  tbl_json2 <-
    tbl_json %>%
    tidyjson::gather_array()

  scores <- unwrap_score(tbl_json2)

  publications <- tbl_json2 %>%
    enter('publication', iterable = FALSE) %>%
    unwrap_publication()

  samples <- tbl_json2 %>%
    unwrap_scores_samples()

  demographics <- tbl_json2 %>%
    unwrap_scores_demographics()

  cohorts <- tbl_json2 %>%
    unwrap_scores_cohorts()

  traits <- tbl_json2 %>%
    enter('trait_efo') %>%
    dplyr::select(-'trait_efo_id') %>%
    unwrap_efotrait()

  tidy_scores_tables <- list(
    scores = scores,
    publications = publications,
    samples = samples,
    demographics = demographics,
    cohorts = cohorts,
    traits = traits
  ) %>%
    remap_id(old = 'id', new = 'pgs_id') %>%
    relocate_metadata_cols()


  return(tidy_scores_tables)
}

#' @importFrom rlang .data
unwrap_score <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(id = tidyjson::jstring('id'),
                            pgs_name = tidyjson::jstring('name'),
                            scoring_file = tidyjson::jstring('ftp_scoring_file'),
                            matches_publication = tidyjson::jlogical('matches_publication'),
                            reported_trait = tidyjson::jstring('trait_reported'),
                            trait_additional_description = tidyjson::jstring('trait_additional'),
                            pgs_method_name = tidyjson::jstring('method_name'),
                            pgs_method_params = tidyjson::jstring('method_params'),
                            n_variants = tidyjson::jnumber('variants_number'),
                            n_variants_interactions = tidyjson::jnumber('variants_interactions'),
                            assembly = tidyjson::jstring('variants_genomebuild'),
                            license = tidyjson::jstring('license')
    ) %>%
    # Coerce json number (default is R's double) to integer.
    dplyr::mutate(n_variants = as.integer(.data$n_variants),
                  n_variants_interactions = as.integer(.data$n_variants_interactions)) %>%
    tidyjson::as_tibble()
}

unwrap_scores_samples <- function(tbl_json) {

  samples_variants <- tbl_json %>%
    enter('samples_variants', column_id_name = 'sample_id') %>%
    tibble::add_column(stage = 'gwas') %>%
    unwrap_sample()

  samples_training <- tbl_json %>%
    enter('samples_training', column_id_name = 'sample_id') %>%
    tibble::add_column(stage = 'development') %>%
    unwrap_sample()

  samples <-
    dplyr::bind_rows(samples_variants,
                     samples_training)

  return(samples)
}

unwrap_scores_demographics <- function(tbl_json) {

  demographics_variants <- tbl_json %>%
    enter('samples_variants', column_id_name = 'sample_id') %>%
    tibble::add_column(stage = 'gwas') %>%
    unwrap_demographics()

  demographics_training <- tbl_json %>%
    enter('samples_training', column_id_name = 'sample_id') %>%
    tibble::add_column(stage = 'development') %>%
    unwrap_demographics()

  demographics <-
    dplyr::bind_rows(demographics_variants,
                     demographics_training)

  return(demographics)
}

unwrap_scores_cohorts <- function(tbl_json) {

  cohorts_variants <- tbl_json %>%
    enter('samples_variants', column_id_name = 'sample_id') %>%
    tibble::add_column(stage = 'gwas') %>%
    tidyjson::enter_object('cohorts') %>%
    tidyjson::gather_array(column.name = 'cohort_id') %>%
    dplyr::select(-'cohort_id') %>%
    unwrap_cohort()

  cohorts_training <- tbl_json %>%
    enter('samples_training', column_id_name = 'sample_id') %>%
    tibble::add_column(stage = 'development') %>%
    tidyjson::enter_object('cohorts') %>%
    tidyjson::gather_array(column.name = 'cohort_id') %>%
    dplyr::select(-'cohort_id') %>%
    unwrap_cohort()

  cohorts <-
    dplyr::bind_rows(cohorts_variants,
                     cohorts_training)

  return(cohorts)
}

