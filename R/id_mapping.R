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

#' Map GWAS studies identifiers to PGS identifiers
#'
#' Map GWAS studies identifiers to PGS identifiers.
#'
#' @param study_id A character vector of GWAS Catalog study accession
#'   identifiers, e.g., "GCST001937".
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if
#'   any.
#' @param progress_bar Whether to show a progress bar as the queries are
#'   performed.
#'
#' @return A data frame of two columns: \code{study_id} and \code{pgs_id}.
#'
#' @examples
#' study_to_pgs('GCST001937')
#'
#' study_to_pgs(c('GCST000998', 'GCST000338'))
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

  tbl <- drop_metadata_cols(tbl) %>%
    dplyr::distinct() %>%
    tidyr::drop_na()

  return(tbl)
}

#' Map PGS identifiers to GWAS study identifiers
#'
#' Map PGS identifiers to GWAS study identifiers. Retrieves GWAS study
#' identifiers associated with samples used in the discovery stage of queried
#' PGS identifiers.
#'
#' @param pgs_id A character vector of PGS Catalog score accession identifiers.,
#'   e.g., "PGS000001". If \code{NULL} then returns results for all PGS
#'   identifiers in the Catalog.
#' @param interactive A logical. If all scores are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if
#'   any.
#' @param progress_bar Whether to show a progress bar as the queries are
#'   performed.
#'
#' @return A data frame of two columns: \code{pgs_id} and \code{study_id}.
#'
#' @examples
#' pgs_to_study('PGS000001')
#'
#' # Unmappable pgs ids will be missing, e.g., PGS000023
#' pgs_to_study(c('PGS000013', 'PGS000023'))
#'
#' @export
pgs_to_study <- function(pgs_id = NULL,
                         interactive = TRUE,
                         verbose = FALSE,
                         warnings = TRUE,
                         progress_bar = TRUE) {

  my_scores <-
    get_scores(
      pgs_id = pgs_id,
      interactive = interactive,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  my_scores@samples[c('pgs_id', 'study_id')] %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    return()

}

#' Map PGP identifiers to PGS identifiers
#'
#' Map PGP identifiers to PGS identifiers.
#'
#' @param pgp_id A character vector of PGS Catalog Publication identifiers,
#'   e.g., "PGP000001". If \code{NULL} then returns results for all PGP
#'   identifiers in the Catalog.
#' @param interactive A logical. If all scores are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if
#'   any.
#' @param progress_bar Whether to show a progress bar as the queries are
#'   performed.
#'
#' @return A data frame of two columns: \code{pgp_id} and \code{pgs_id}.
#'
#' @examples
#' pgp_to_pgs('PGP000001')
#'
#' pgp_to_pgs(c('PGP000017', 'PGP000042'))
#'
#' @export
pgp_to_pgs <- function(pgp_id = NULL,
                         interactive = TRUE,
                         verbose = FALSE,
                         warnings = TRUE,
                         progress_bar = TRUE) {

  my_pub <-
    get_publications(
      pgp_id = pgp_id,
      interactive = interactive,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  my_pub@pgs_ids %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    return()
}

#' Map PGS identifiers to PGP identifiers
#'
#' Map PGS identifiers to PGP identifiers.
#'
#' @param pgs_id A character vector of PGS identifiers,
#'   e.g., "PGS000001". If \code{NULL} then returns results for all PGS
#'   identifiers in the Catalog.
#' @param interactive A logical. If all scores are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if
#'   any.
#' @param progress_bar Whether to show a progress bar as the queries are
#'   performed.
#'
#' @return A data frame of two columns: \code{pgs_id} and \code{pgp_id}.
#'
#' @examples
#' pgs_to_pgp('PGS000001')
#'
#' pgs_to_pgp(c('PGS000017', 'PGS000042'))
#'
#' @export
pgs_to_pgp <- function(pgs_id = NULL,
                       interactive = TRUE,
                       verbose = FALSE,
                       warnings = TRUE,
                       progress_bar = TRUE) {

  my_scores <-
    get_scores(
      pgs_id = pgs_id,
      interactive = interactive,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  my_scores@publications[c('pgs_id', 'pgp_id')] %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    return()
}

#' Map PGS identifiers to PSS identifiers
#'
#' Map PGS identifiers to PSS identifiers.
#'
#' @param pgs_id A character vector of PGS identifiers,
#'   e.g., "PGS000001". If \code{NULL} then returns results for all PGS
#'   identifiers in the Catalog.
#' @param interactive A logical. If all scores are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if
#'   any.
#' @param progress_bar Whether to show a progress bar as the queries are
#'   performed.
#'
#' @return A data frame of two columns: \code{pgs_id} and \code{pss_id}.
#'
#' @examples
#' pgs_to_pss('PGS000001')
#'
#' pgs_to_pss(c('PGS000017', 'PGS000042'))
#'
#' @export
pgs_to_pss <- function(pgs_id = NULL,
                       interactive = TRUE,
                       verbose = FALSE,
                       warnings = TRUE,
                       progress_bar = TRUE) {

  pgs_id <- purrr::set_names(pgs_id)

  get_pss <- function(pgs_id, ...) {

    get_sample_sets(pgs_id = pgs_id, ...)@sample_sets['pss_id']
  }

  purrr::map_dfr(
    pgs_id,
    get_pss,
    interactive = interactive,
    verbose = verbose,
    warnings = warnings,
    progress_bar = progress_bar,
    .id = 'pgs_id'
  ) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    return()
}

#' Map PSS identifiers to PGP identifiers
#'
#' Map PSS identifiers to PGP identifiers. This is a slow function because it
#' starts by downloading first all Performance Metrics, as this is the linkage
#' between PSS and PGS.
#'
#' @param pss_id A character vector of PSS identifiers,
#'   e.g., "PSS000001".
#' @param interactive A logical. If all scores are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if
#'   any.
#' @param progress_bar Whether to show a progress bar as the queries are
#'   performed.
#'
#' @return A data frame of two columns: \code{pss_id} and \code{pgs_id}.
#'
#' @examples
#' pss_to_pgs('PSS000001', interactive = FALSE)
#'
#' pss_to_pgs(c('PSS000017', 'PSS000042'), interactive = FALSE)
#'
#' @export
pss_to_pgs <- function(pss_id,
                       interactive = TRUE,
                       verbose = FALSE,
                       warnings = TRUE,
                       progress_bar = TRUE) {

  all_metrics <- get_performance_metrics(interactive = interactive,
                                         verbose = verbose,
                                         warnings = warnings,
                                         progress_bar = progress_bar)

  pss_ids <- pss_id
  pss_of_interest <- dplyr::filter(all_metrics@sample_sets, pss_id %in% pss_ids)

  dplyr::left_join(pss_of_interest,
                   all_metrics@performance_metrics[c('ppm_id', 'pgs_id')],
                   by = 'ppm_id') %>%
    dplyr::select(-'ppm_id') %>%
    dplyr::distinct() %>%
    tidyr::drop_na()

}
