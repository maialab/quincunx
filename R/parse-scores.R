as_tidy_tables_scores <- function(tbl_json) {

  tbl_json2 <-
    tbl_json %>%
    tidyjson::gather_array()

  scores <- unwrap_score(tbl_json2)

  publications <- tbl_json2 %>%
    tidyjson::enter_object('publication') %>%
    unwrap_publication()

  samples <- tbl_json2 %>%
    unwrap_scores_samples()

  demographics <- tbl_json2 %>%
    unwrap_scores_demographics()

  cohorts <- tbl_json2 %>%
    unwrap_scores_cohorts()

  traits <- tbl_json2 %>%
    tidyjson::enter_object('trait_efo') %>%
    tidyjson::gather_array(column.name = 'trait_efo_id') %>%
    dplyr::select(-'trait_efo_id') %>%
    unwrap_efotrait()

  ancestries_json <- collect_ancestries(tbl_json2)
  stages_tally <- unwrap_ancestry_count(ancestries_json)
  ancestry_frequencies <- unwrap_ancestry_frequencies(ancestries_json)
  multi_ancestry_composition <- unwrap_multi_ancestry_composition(ancestries_json)

  tidy_scores_tables <- list(
    scores = scores,
    publications = publications,
    samples = samples,
    demographics = demographics,
    cohorts = cohorts,
    traits = traits,
    stages_tally = stages_tally,
    ancestry_frequencies = ancestry_frequencies,
    multi_ancestry_composition = multi_ancestry_composition
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

#' @importFrom rlang .data
unwrap_scores_samples <- function(tbl_json) {

  samples_variants <- tbl_json %>%
    tidyjson::enter_object('samples_variants') %>%
    tibble::add_column(stage = 'gwas') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    dplyr::select(-'sample_id') %>%
    unwrap_sample()

  samples_training <- tbl_json %>%
    tidyjson::enter_object('samples_training') %>%
    tibble::add_column(stage = 'dev') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    dplyr::select(-'sample_id') %>%
    unwrap_sample()

  samples <-
    dplyr::bind_rows(samples_variants,
                     samples_training) %>%
    dplyr::group_by(.data$..page, .data$array.index) %>%
    dplyr::mutate(sample_id = seq_len(dplyr::n()), .after = 'array.index') %>%
    dplyr::arrange('sample_id', .by_group = TRUE) %>%
    dplyr::ungroup()

  return(samples)
}

#' @importFrom rlang .data
unwrap_scores_demographics <- function(tbl_json) {

  # demographics_variants <- tbl_json %>%
  #   tidyjson::enter_object('samples_variants') %>%
  #   tidyjson::gather_array(column.name = 'sample_id') %>%
  #   dplyr::select(-'sample_id')
  #
  # demographics_training <- tbl_json %>%
  #   tidyjson::enter_object('samples_training') %>%
  #   tidyjson::gather_array(column.name = 'sample_id') %>%
  #   dplyr::select(-'sample_id')

  all_samples <- collect_samples(tbl_json)
  demographics <- unwrap_demographics(all_samples)

  return(demographics)
}

#' @importFrom rlang .data
unwrap_scores_cohorts <- function(tbl_json) {

  # Collects together samples annotated as 'discovery' and as 'training' that
  # come in separate objects, namely, "samples_variants" and "samples_training".
  all_samples <- collect_samples(tbl_json)

  cohorts <- all_samples %>%
    tidyjson::enter_object('cohorts') %>%
    tidyjson::gather_array(column.name = 'cohort_id') %>%
    dplyr::select(-'cohort_id') %>%
    unwrap_cohort()

  return(cohorts)
}

#' Collect samples_variants and samples_training
#'
#' Collect samples_variants and samples_training and reference them by a unique
#' id (sample_id).
#'
#' @keywords internal
#'
#' @importFrom rlang .data
collect_samples <- function(tbl_json) {

  samples_variants <- tbl_json %>%
    tidyjson::enter_object('samples_variants') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    dplyr::select(-'sample_id')

  samples_training <- tbl_json %>%
    tidyjson::enter_object('samples_training') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    dplyr::select(-'sample_id')

  all_samples <-
    tidyjson::bind_rows(samples_variants, samples_training) %>%
    dplyr::group_by(.data$..page, .data$array.index) %>%
    dplyr::mutate(sample_id = seq_len(dplyr::n()), .after = 'array.index') %>%
    dplyr::arrange('sample_id', .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    tidyjson::as.tbl_json(json.column = '..JSON') # Needed because of https://github.com/colearendt/tidyjson/issues/135.

  return(all_samples)
}

collect_ancestries <- function(tbl_json) {

  tbl_json2 <-
    tidyjson::spread_values(tbl_json, id = tidyjson::jstring('id'))

  gwas_ancestry <- tbl_json2 %>%
    tidyjson::enter_object('ancestry_distribution', 'gwas') %>%
    tibble::add_column(stage = 'gwas')

  dev_ancestry <- tbl_json2 %>%
    tidyjson::enter_object('ancestry_distribution', 'dev') %>%
    tibble::add_column(stage = 'dev')

  eval_ancestry <- tbl_json2 %>%
    tidyjson::enter_object('ancestry_distribution', 'eval') %>%
    tibble::add_column(stage = 'eval')

  all_ancestries <-
    tidyjson::bind_rows(gwas_ancestry, dev_ancestry, eval_ancestry) %>%
    dplyr::arrange(.data$..page, .data$array.index)

  return(all_ancestries)
}

unwrap_multi_ancestry_composition <- function(tbl_json) {

  tbl_json %>%
    tidyjson::enter_object('multi') %>%
    tidyjson::gather_array('array.index.2') %>%
    tidyjson::append_values_string('multi_ancestry_composition') %>%
    tidyjson::as_tibble() %>%
    tidyr::separate(col = 'multi_ancestry_composition', into = c('multi_ancestry_class_symbol', 'ancestry_class_symbol'), sep = '_') %>%
    dplyr::select(-'array.index.2') %>%
    tidyjson::as_tibble()
}

#' @importFrom rlang .data
unwrap_ancestry_count <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(count = tidyjson::jnumber('count')) %>%
    tidyjson::as_tibble() %>%
    dplyr::mutate(count = as.integer(count)) %>%
    dplyr::mutate(sample_size = dplyr::if_else(.data$stage == 'gwas' | .data$stage == 'dev', .data$count, NA_integer_),
                  n_sample_sets = dplyr::if_else(.data$stage == 'eval', .data$count, NA_integer_)) %>%
    dplyr::select(-'count')
}

unwrap_ancestry_frequencies <- function(tbl_json) {

  tbl_json %>%
    tidyjson::enter_object('dist') %>%
    tidyjson::gather_object('ancestry_class_symbol') %>%
    tidyjson::append_values_number(column.name = 'frequency') %>%
    tidyjson::as_tibble()
}
