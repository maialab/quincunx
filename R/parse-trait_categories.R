as_tidy_tables_trait_categories <- function(tbl_json) {

  tbl_json2 <-
    tbl_json %>%
    tidyjson::gather_array()

  trait_categories <-
    tbl_json2 %>%
    tidyjson::spread_values(trait_category = tidyjson::jstring('label')) %>%
    dplyr::select(-'array.index') %>%
    tidyjson::as_tibble()

  traits <-
    tbl_json2 %>%
    tidyjson::spread_values(trait_category = tidyjson::jstring('label')) %>%
    dplyr::select(-'array.index') %>%
    tidyjson::enter_object('efotraits') %>%
    tidyjson::gather_array(column.name = 'efotraits_id') %>%
    unwrap_efotrait() %>%
    dplyr::select(-'efotraits_id') %>%
    tidyjson::as_tibble()

  tidy_trait_categories_tables <-
    list(trait_categories = trait_categories,
         traits = traits) %>%
    relocate_metadata_cols()

  return(tidy_trait_categories_tables)
}

