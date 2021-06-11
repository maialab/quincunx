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

# unwrap_publication_pgs_ids <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(id = tidyjson::jstring('id')) %>%
#     tidyjson::enter_object('associated_pgs_ids') %>%
#     tidyjson::gather_array(column.name = 'associated_pgs_ids_id') %>%
#     dplyr::select(-'associated_pgs_ids_id') %>%
#     tidyjson::append_values_string('pgs_id') %>%
#     tidyjson::as_tibble()
# }

unwrap_publication_pgs_ids <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(id = tidyjson::jstring('id')) %>%
    collect_pgs_ids()
}

#' Collect development and evaluation pgs_ids
#'
#' Collect development and evaluation pgs_ids.
#'
#' @keywords internal
#' @importFrom rlang .data
collect_pgs_ids <- function(tbl_json) {

  development_pgs_ids <- tbl_json %>%
    tidyjson::enter_object('associated_pgs_ids', 'development')%>%
    tidyjson::gather_array(column.name = 'dummy') %>%
    dplyr::select(-'dummy') %>%
    tidyjson::append_values_string(column.name = 'pgs_id') %>%
    tibble::add_column(stage = 'gwas/dev')


  evaluation_pgs_ids <- tbl_json %>%
    tidyjson::enter_object('associated_pgs_ids', 'evaluation')%>%
    tidyjson::gather_array(column.name = 'dummy') %>%
    dplyr::select(-'dummy') %>%
    tidyjson::append_values_string(column.name = 'pgs_id') %>%
    tibble::add_column(stage = 'eval')

  all_pgs_ids <-
    tidyjson::bind_rows(development_pgs_ids, evaluation_pgs_ids) %>%
    tidyjson::as_tibble() %>%
    dplyr::group_by(.data$..page, .data$array.index) %>%
    dplyr::arrange('pgs_id', .by_group = TRUE) %>%
    dplyr::ungroup()

  return(all_pgs_ids)
}
