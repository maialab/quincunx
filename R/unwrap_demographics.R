unwrap_demographics <- function(tbl_json) {

  sample_age <- tbl_json %>%
    enter('sample_age', iterable = FALSE) %>%
    unwrap_demographic() %>%
    tibble::add_column(variable = 'age', .before = 'estimate_type')

  followup_time <- tbl_json %>%
    enter('followup_time', iterable = FALSE) %>%
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
    enter('interval', iterable = FALSE) %>%
    unwrap_interval()

  return(tbl1)
}
