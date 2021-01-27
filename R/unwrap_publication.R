#' @importFrom rlang .data
unwrap_publication <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      id = tidyjson::jstring('id'),
      pubmed_id = tidyjson::jstring('PMID'),
      publication_date = tidyjson::jstring('date_publication'),
      publication = tidyjson::jstring('journal'),
      title = tidyjson::jstring('title'),
      author_fullname = tidyjson::jstring('firstauthor'),
      doi = tidyjson::jstring('doi')
    ) %>%
    dplyr::mutate(
      publication_date = lubridate::ymd(nr_to_na(.data$publication_date)),
    ) %>%
    tidyjson::as_tibble()
}
