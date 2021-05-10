as_tidy_tables_cohorts <- function(tbl_json) {

  tidy_cohorts_tables <-
    tbl_json %>%
    tidyjson::gather_array() %>%
    unwrap_cohort_extended() %>%
    relocate_metadata_cols()

  return(tidy_cohorts_tables)
}

#' @importFrom rlang .data
unwrap_cohort_extended <- function(tbl_json) {

  cohorts <-
    tbl_json %>%
    tidyjson::spread_values(
      cohort_symbol = tidyjson::jstring('name_short'),
      cohort_name = tidyjson::jstring('name_full')) %>%
    tidyjson::as_tibble() %>%
    dplyr::select(-'array.index')

  pgs_ids_at_development <-
    tbl_json %>%
    tidyjson::spread_values(cohort_symbol = tidyjson::jstring('name_short')) %>%
    tidyjson::enter_object('associated_pgs_ids') %>%
    tidyjson::enter_object('development') %>%
    tidyjson::gather_array(column.name = 'development_id') %>%
    dplyr::select(-'development_id') %>%
    tidyjson::append_values_string(column.name = 'pgs_id') %>%
    tibble::add_column(stage = 'gwas/dev')

  pgs_ids_at_evaluation <-
    tbl_json %>%
    tidyjson::spread_values(cohort_symbol = tidyjson::jstring('name_short')) %>%
    tidyjson::enter_object('associated_pgs_ids') %>%
    tidyjson::enter_object('evaluation') %>%
    tidyjson::gather_array(column.name = 'evaluation_id') %>%
    dplyr::select(-'evaluation_id') %>%
    tidyjson::append_values_string(column.name = 'pgs_id') %>%
    tibble::add_column(stage = 'eval')

  pgs_ids <- dplyr::bind_rows(
    pgs_ids_at_development,
    pgs_ids_at_evaluation
    )  %>%
    tidyjson::as_tibble() %>%
    dplyr::group_by(.data$..page, .data$array.index) %>%
    dplyr::arrange('pgs_id', .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-'array.index')

  list(
    cohorts = cohorts,
    pgs_ids = pgs_ids
  )

}
