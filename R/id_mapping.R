get_gwas_get_score_ids <- function(resource,
                                   study_id,
                                   limit = 20L,
                                   verbose = FALSE,
                                   warnings = TRUE,
                                   progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tbl_json %>%
    tidyjson::gather_array(column.name = 'dummy') %>%
    tidyjson::gather_array(column.name = 'dummy2') %>%
    tibble::add_column(study_id = study_id) %>%
    tidyjson::append_values_string(column.name = 'pgs_id') %>%
    dplyr::select(-c('dummy', 'dummy2')) %>%
    tidyjson::as_tibble() %>%
    relocate_metadata_cols()
}


get_gwas_get_score_ids_by_gcst_id <- function(study_id,
                                              limit = 20L,
                                              verbose = FALSE,
                                              warnings = TRUE,
                                              progress_bar = TRUE) {

  resource <- '/rest/gwas/get_score_ids'
  resource_urls <- sprintf("%s/%s", resource, study_id)

  purrr::map2(
    resource_urls,
    study_id,
    get_gwas_get_score_ids,
    limit = limit,
    warnings = warnings,
    verbose = verbose,
    progress_bar = progress_bar
  ) %>% dplyr::bind_rows()

}

#' Map GWAS study to PGS identifiers
#'
#' Map GWAS study to PGS identifiers
#'
#'
#' @export
study_to_pgs <- function(study_id,
                               verbose = FALSE,
                               warnings = TRUE,
                               progress_bar = TRUE) {


  tbl <- get_gwas_get_score_ids_by_gcst_id(study_id,
                                    limit = 20L,
                                    verbose = verbose,
                                    warnings = warnings,
                                    progress_bar = progress_bar)

  tbl <- drop_metadata_cols(tbl)
  return(tbl)
}
