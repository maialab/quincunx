as_tidy_tables_publications <- function(tbl_json) {

  tbl_json2 <-
    tbl_json %>%
    tidyjson::gather_array()

  publication_ext <- unwrap_publication_extended(tbl_json2)
  publication_pgs_ids <- unwrap_publication_pgs_ids(tbl_json2)

  tidy_publications_tables <-
    list(publications = publication_ext,
         pgs_ids = publication_pgs_ids) %>%
    remap_id(old = 'id', new = 'pgp_id') %>%
    relocate_metadata_cols()

  return(tidy_publications_tables)
}

#' @importFrom rlang .data
unwrap_publication_extended <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      id = tidyjson::jstring('id'),
      pubmed_id = tidyjson::jstring('PMID'),
      publication_date = tidyjson::jstring('date_publication'),
      publication = tidyjson::jstring('journal'),
      title = tidyjson::jstring('title'),
      author_fullname = tidyjson::jstring('firstauthor'),
      doi = tidyjson::jstring('doi'),
      authors = tidyjson::jstring('authors')
    ) %>%
    dplyr::mutate(publication_date = lubridate::ymd(nr_to_na(.data$publication_date))) %>%
    tidyjson::as_tibble()
}

unwrap_publication_pgs_ids <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(id = tidyjson::jstring('id')) %>%
    enter('associated_pgs_ids') %>%
    dplyr::select(-'associated_pgs_ids_id') %>%
    tidyjson::append_values_string('pgs_id') %>%
    tidyjson::as_tibble()
}
