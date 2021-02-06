as_tidy_tables_performance_metrics <- function(tbl_json) {

  lst_tbl <-
    tbl_json %>%
    tidyjson::gather_array() %>%
    unpack_performance_metric() %>%
    remap_id(old = 'id', new = 'ppm_id') %>%
    relocate_metadata_cols()

  tidy_performance_metrics_tables <- lst_tbl

  return(tidy_performance_metrics_tables)
}

unpack_performance_metric <- function(tbl_json) {

  performance_metrics <- unwrap_performance_metric(tbl_json)

  publications <- tbl_json %>%
    tidyjson::enter_object('publication') %>%
    unwrap_publication()

  sample_sets <-
    tbl_json %>%
    tidyjson::enter_object('sampleset') %>%
    unwrap_sample_set()

  samples <-
    tbl_json %>%
    tidyjson::enter_object('sampleset') %>%
    tidyjson::spread_values(pss_id = tidyjson::jstring('id')) %>%
    tidyjson::enter_object('samples') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    tibble::add_column(stage = 'evaluation') %>% # All PPM samples in evaluation stage.
    unwrap_sample()

  demographics <-
    tbl_json %>%
    tidyjson::enter_object('sampleset') %>%
    tidyjson::spread_values(pss_id = tidyjson::jstring('id')) %>%
    tidyjson::enter_object('samples') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    unwrap_demographics()

  cohorts <-
    tbl_json %>%
    tidyjson::enter_object('sampleset') %>%
    tidyjson::spread_values(pss_id = tidyjson::jstring('id')) %>%
    tidyjson::enter_object('samples') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    tidyjson::enter_object('cohorts') %>%
    tidyjson::gather_array(column.name = 'cohort_id') %>%
    dplyr::select(-'cohort_id') %>%
    unwrap_cohort()

  pgs_effect_sizes <- tbl_json %>%
    tidyjson::enter_object('performance_metrics') %>%
    tidyjson::enter_object('effect_sizes') %>%
    tidyjson::gather_array(column.name = 'effect_size_id') %>%
    unwrap_metric()

  pgs_classification_metrics <- tbl_json %>%
    tidyjson::enter_object('performance_metrics') %>%
    tidyjson::enter_object('class_acc') %>%
    tidyjson::gather_array(column.name = 'classification_metrics_id') %>%
    unwrap_metric()

  pgs_other_metrics <- tbl_json %>%
    tidyjson::enter_object('performance_metrics') %>%
    tidyjson::enter_object('othermetrics') %>%
    tidyjson::gather_array(column.name = 'other_metrics_id') %>%
    unwrap_metric()

  list(
    performance_metrics = performance_metrics,
    publications = publications,
    sample_sets = sample_sets,
    samples = samples,
    demographics = demographics,
    cohorts = cohorts,
    pgs_effect_sizes = pgs_effect_sizes,
    pgs_classification_metrics = pgs_classification_metrics,
    pgs_other_metrics = pgs_other_metrics
  )

}

unwrap_metric <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      estimate_type_long = tidyjson::jstring('name_long'),
      estimate_type = tidyjson::jstring('name_short'),
      estimate = tidyjson::jnumber('estimate'),
      variability = tidyjson::jnumber('se'),
      interval_lower = tidyjson::jnumber('ci_lower'),
      interval_upper = tidyjson::jnumber('ci_upper')
    ) %>%
    tibble::add_column(variability_type = 'se', .before = 'variability') %>%
    tibble::add_column(interval_type = 'ci', .before = 'interval_lower') %>%
    tibble::add_column(unit = NA_character_, .after = 'estimate') %>%
    tidyjson::as_tibble()

}

unwrap_performance_metric <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      id = tidyjson::jstring('id'),
      pgs_id = tidyjson::jstring('associated_pgs_id'),
      reported_trait = tidyjson::jstring('phenotyping_reported'),
      covariates = tidyjson::jstring('covariates'),
      comments = tidyjson::jstring('performance_comments')
    ) %>%
    tidyjson::as_tibble()

}

unwrap_sample_set <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(pss_id = tidyjson::jstring('id')) %>%
    tidyjson::as_tibble()

}
