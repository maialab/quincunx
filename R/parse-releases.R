as_tidy_tables_releases <- function(tbl_json) {

  tbl_json2 <-
    tbl_json %>%
    tidyjson::gather_array()

  releases <- unwrap_release(tbl_json2)

  pgs_ids <- unwrap_release_pgs_ids(tbl_json2)
  ppm_ids <- unwrap_release_ppm_ids(tbl_json2)
  pgp_ids <- unwrap_release_pgp_ids(tbl_json2)

  tidy_releases_tables <-
    list(releases = relocate_metadata_cols(releases),
         pgs_ids = relocate_metadata_cols(pgs_ids),
         ppm_ids = relocate_metadata_cols(ppm_ids),
         pgp_ids = relocate_metadata_cols(pgp_ids))

  return(tidy_releases_tables)

}

#' @importFrom rlang .data
unwrap_release <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      date = tidyjson::jstring('date'),
      n_pgs = tidyjson::jnumber('score_count'),
      n_ppm = tidyjson::jnumber('performance_count'),
      n_pgp = tidyjson::jnumber('publication_count'),
      notes = tidyjson::jstring('notes')) %>%
    # Coerce json number (default is R's double) to integer.
    dplyr::mutate(n_pgs = as.integer(.data$n_pgs),
                  n_ppm = as.integer(.data$n_ppm),
                  n_pgp = as.integer(.data$n_pgp)) %>%
    dplyr::select(-'array.index') %>%
    tidyjson::as_tibble()
}

#' @importFrom rlang .data
unwrap_release_pgs_ids <- function(tbl_json) {

  tbl_json %>%
    dplyr::select(-'array.index') %>%
    tidyjson::spread_values(date = tidyjson::jstring('date')) %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
    tidyjson::enter_object('released_score_ids') %>%
    tidyjson::gather_array() %>%
    dplyr::select(-'array.index') %>%
    tidyjson::append_values_string('pgs_id') %>%
    tidyjson::as_tibble()

}

#' @importFrom rlang .data
unwrap_release_ppm_ids <- function(tbl_json) {

    tbl_json %>%
    dplyr::select(-'array.index') %>%
    tidyjson::spread_values(date = tidyjson::jstring('date')) %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
    tidyjson::enter_object('released_performance_ids') %>%
    tidyjson::gather_array() %>%
    dplyr::select(-'array.index') %>%
    tidyjson::append_values_string('ppm_id') %>%
    tidyjson::as_tibble()

}

#' @importFrom rlang .data
unwrap_release_pgp_ids <- function(tbl_json) {

  tbl_json %>%
    dplyr::select(-'array.index') %>%
    tidyjson::spread_values(date = tidyjson::jstring('date')) %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
    tidyjson::enter_object('released_publication_ids') %>%
    tidyjson::gather_array() %>%
    dplyr::select(-'array.index') %>%
    tidyjson::append_values_string('pgp_id') %>%
    tidyjson::as_tibble()

}
