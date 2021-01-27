as_tidy_tables_traits <- function(tbl_json) {

  tbl_json2 <-
    tbl_json %>%
    tidyjson::gather_array()

  tidy_traits_tables <- unwrap_traits2(tbl_json2)

  return(tidy_traits_tables)
}

# unwrap_traits_tbl <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(
#       efo_id = tidyjson::jstring(id),
#       trait = tidyjson::jstring(label),
#       description = tidyjson::jstring(description),
#       url = tidyjson::jstring(url)
#     ) %>%
#     tidyjson::as_tibble()
# }

unwrap_traits <- function(tbl_json) {

  traits <- unwrap_efotrait(tbl_json)

  pgs_ids <- tbl_json %>%
    tidyjson::spread_values(efo_id = tidyjson::jstring('id')) %>%
    enter('associated_pgs_ids') %>%
    dplyr::select(-'associated_pgs_ids_id') %>%
    tidyjson::append_values_string('pgs_id') %>%
    tidyjson::as_tibble()

  child_pgs_ids <- tbl_json %>%
    tidyjson::spread_values(efo_id = tidyjson::jstring('id')) %>%
    enter('child_associated_pgs_ids') %>%
    dplyr::select(-'child_associated_pgs_ids_id') %>%
    tidyjson::append_values_string('child_pgs_id') %>%
    tidyjson::as_tibble()

  trait_categories <- tbl_json %>%
    tidyjson::spread_values(efo_id = tidyjson::jstring('id')) %>%
    enter('trait_categories') %>%
    dplyr::select(-'trait_categories_id') %>%
    tidyjson::append_values_string('trait_categories') %>%
    tidyjson::as_tibble()

  trait_synonyms <- tbl_json %>%
    tidyjson::spread_values(efo_id = tidyjson::jstring('id')) %>%
    enter('trait_synonyms') %>%
    dplyr::select(-'trait_synonyms_id') %>%
    tidyjson::append_values_string('trait_synonyms') %>%
    tidyjson::as_tibble()

  trait_mapped_terms <- tbl_json %>%
    tidyjson::spread_values(efo_id = tidyjson::jstring('id')) %>%
    enter('trait_mapped_terms') %>%
    dplyr::select(-'trait_mapped_terms_id') %>%
    tidyjson::append_values_string('trait_mapped_terms') %>%
    tidyjson::as_tibble()

  list(traits = traits,
       pgs_ids = pgs_ids,
       child_pgs_ids = child_pgs_ids,
       trait_categories = trait_categories,
       trait_synonyms = trait_synonyms,
       trait_mapped_terms = trait_mapped_terms
       )
}

unwrap_traits2 <- function(tbl_json) {

  lst_tbl1 <-
    tbl_json %>%
    tibble::add_column(parent_efo_id = NA_character_,
                       is_child = FALSE) %>%
    unwrap_traits()

  lst_tbl2 <-
    tbl_json %>%
    tidyjson::spread_values(parent_efo_id = tidyjson::jstring('id')) %>%
    tibble::add_column(is_child = TRUE) %>%
    enter('child_traits') %>%
    dplyr::select(-'child_traits_id') %>%
    unwrap_traits()

  # list(lst_tbl1, lst_tbl2)
  purrr::pmap(list(lst_tbl1, lst_tbl2), dplyr::bind_rows) %>%
    purrr::map(dplyr::relocate, c('efo_id', 'parent_efo_id', 'is_child'), .before = 1L) %>%
    purrr::map(~ dplyr::select(., -'array.index')) %>%
    relocate_metadata_cols()

}

