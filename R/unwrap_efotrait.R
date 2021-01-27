unwrap_efotrait <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      efo_id = tidyjson::jstring('id'),
      trait = tidyjson::jstring('label'),
      description = tidyjson::jstring('description'),
      url = tidyjson::jstring('url')) %>%
    tidyjson::as_tibble()
}
