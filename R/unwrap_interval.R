unwrap_interval <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      interval_type = tidyjson::jstring('type'),
      interval_lower = tidyjson::jnumber('lower'),
      interval_upper = tidyjson::jnumber('upper')
    ) %>%
    tidyjson::as_tibble()
}
