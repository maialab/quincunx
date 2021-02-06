as_tidy_tables_sample_sets <- function(tbl_json) {

  tbl_json2 <-
    tbl_json %>%
    tidyjson::gather_array() %>%
    dplyr::select(-'array.index')

  sample_sets <-
    tbl_json2 %>%
    tidyjson::spread_values(pss_id = tidyjson::jstring('id')) %>%
    tidyjson::as_tibble()

  samples <-
    tbl_json2 %>%
    tidyjson::spread_values(pss_id = tidyjson::jstring('id')) %>%
    tidyjson::enter_object('samples') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    tibble::add_column(stage = 'evaluation') %>% # All samples in sample sets are in evaluation stage.
    unwrap_sample()

  demographics <-
    tbl_json2 %>%
    tidyjson::spread_values(pss_id = tidyjson::jstring('id')) %>%
    tidyjson::enter_object('samples') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    unwrap_demographics()

  cohorts <-
    tbl_json2 %>%
    tidyjson::spread_values(pss_id = tidyjson::jstring('id')) %>%
    tidyjson::enter_object('samples') %>%
    tidyjson::gather_array(column.name = 'sample_id') %>%
    tidyjson::enter_object('cohorts') %>%
    tidyjson::gather_array(column.name = 'cohort_id') %>%
    dplyr::select(-'cohort_id') %>%
    unwrap_cohort()

  tidy_sample_sets_tables <-
    list(
      sample_sets = sample_sets,
      samples = samples,
      demographics = demographics,
      cohorts = cohorts
    ) %>%
    relocate_metadata_cols()

  return(tidy_sample_sets_tables)
}

