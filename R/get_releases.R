get_release <- function(resource,
                            limit = 20L,
                            verbose = FALSE,
                            warnings = TRUE,
                            progress_bar = TRUE) {

  tbl_json <- get(resource_url = resource,
                  limit = limit,
                  verbose = verbose,
                  warnings = warnings,
                  progress_bar = progress_bar)

  tidy_tbls <- as_tidy_tables_releases(tbl_json)

  return(tidy_tbls)
}

get_release_by_release_date <-
  function(release_date,
           limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {

    resource <- '/rest/release'
    resource_urls <- sprintf("%s/%s", resource, release_date)

    purrr::map(
      resource_urls,
      get_release,
      limit = limit,
      warnings = warnings,
      verbose = verbose,
      progress_bar = progress_bar
    ) %>%
      purrr::pmap(dplyr::bind_rows)
}

get_release_current <-
  function(limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {

    resource_url <- '/rest/release/current'

    get_release(resource_url,
                limit = limit,
                warnings = warnings,
                verbose = verbose,
                progress_bar = progress_bar)
}

get_release_all <-
  function(limit = 20L,
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {

    resource_url <- '/rest/release/all'

    get_release(resource_url,
                limit = limit,
                warnings = warnings,
                verbose = verbose,
                progress_bar = progress_bar)
}

#' Get PGS Catalog Releases
#'
#' This function retrieves PGS Catalog release information. Note that the
#' columns \code{pgs_id}, \code{ppm_id} and \code{pgp_id} contain in each
#' element a vector. These columns can be unnested using
#' \code{\link[tidyr]{unnest_longer}} (see Examples).
#'
#' @param date One or more dates formatted as \code{"YYYY-MM-DD"} for respective
#'   releases, \code{"latest"} for the latest release, or \code{"all"} for all
#'   releases.
#' @param verbose Whether to print information about the underlying requests to
#'   the REST API server.
#' @param warnings Whether to print warnings about the underlying requests to
#'   the REST API server.
#' @param progress_bar Whether to show a progress bar indicating download
#'   progress from the REST API server.
#'
#' @return A data frame where each row is a release. Columns are:
#' \describe{
#' \item{date}{Release date.}
#' \item{n_pgs}{Number of released Polygenic Score (PGS) identifiers
#' (\code{pgs_id}).}
#' \item{n_ppm}{Number of released Performance Metric (PPM) identifiers
#' (\code{ppm_id}).}
#' \item{n_pgp}{Number of released PGS Catalog Publication (PGP) identifiers
#' (\code{pgp_id}).}
#' \item{pgs_id}{Released Polygenic Score (PGS) identifiers.}
#' \item{ppm_id}{Released Performance Metric (PPM) identifiers.}
#' \item{pgp_id}{Released PGS Catalog Publication (PGP) identifiers.}
#' \item{notes}{News about the release.}
#' }
#' @examples
#' # Get the latest release
#' get_releases()
#' get_releases(date = 'latest')
#'
#' # Get all releases
#' get_releases(date = 'all')
#'
#' # Get a specific release by date
#' get_releases(date = '2020-08-19')
#'
#'
#' @export
get_releases <- function(date = 'latest',
                         verbose = FALSE,
                         warnings = TRUE,
                         progress_bar = TRUE) {

  if (identical(date, 'latest')) {
    tbl <- get_release_current(verbose = verbose,
                               warnings = warnings,
                               progress_bar = progress_bar) %>%
      coerce_to_s4_releases()
    return(tbl)
  }

  if (identical(date, 'all')) {
    tbl <- get_release_all(verbose = verbose,
                           warnings = warnings,
                           progress_bar = progress_bar) %>%
      coerce_to_s4_releases()

    return(tbl)
  }

  if (all(stringr::str_detect(date, pattern = '^\\d{4}-\\d{2}-\\d{2}$'))) {
    tbl <- get_release_by_release_date(
      release_date = date,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    ) %>%
      coerce_to_s4_releases()

    return(tbl)
  }

  stop('Argument `date` must be one of:\n',
       '    - "all":         for all releases;\n',
       '    - "latest":      for the most up-to-date release;\n',
       '    - "YYYY-MM-DD":  for a release of a specific date, e.g., "2020-10-19".\n'
       )
}
