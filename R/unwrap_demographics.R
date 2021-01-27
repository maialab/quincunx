unwrap_demographics <- function(tbl_json) {

  sample_age <- tbl_json %>%
    tidyjson::enter_object('sample_age') %>%
    unwrap_demographic() %>%
    tibble::add_column(variable = 'age', .before = 'estimate_type')

  followup_time <- tbl_json %>%
    tidyjson::enter_object('followup_time') %>%
    unwrap_demographic() %>%
    tibble::add_column(variable = 'follow_up_time', .before = 'estimate_type')

  demographics <-
    dplyr::bind_rows(sample_age,
                     followup_time)

  return(demographics)

}

unwrap_demographic <- function(tbl_json) {

  tbl1 <-
    tbl_json %>%
    tidyjson::spread_values(
      estimate_type = tidyjson::jstring('estimate_type'),
      estimate = tidyjson::jnumber('estimate'),
      unit = tidyjson::jstring('unit'),
      variability_type = tidyjson::jstring('variability_type'),
      variability = tidyjson::jnumber('variability')
    ) %>%
    tidyjson::enter_object('interval') %>%
    unwrap_interval()

  return(tbl1)
}
