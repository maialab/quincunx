unwrap_cohort <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      cohort_symbol = tidyjson::jstring('name_short'),
      cohort_name = tidyjson::jstring('name_full')) %>%
    # Convert from tbl_json to plain and simple tibble
    tidyjson::as_tibble()
}
