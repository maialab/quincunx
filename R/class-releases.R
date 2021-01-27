setOldClass(c("tbl_df", "tbl", "data.frame"))


#' An S4 class to represent a set of PGS Catalog Releases
#'
#' The releases object consists of four slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of PGS Catalog releases. Each score is an observation (row) in
#' the \code{scores} table --- main table. All tables have the column
#' \code{date} as primary key.
#'
#' @slot releases
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot pgs_ids A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot ppm_ids A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot pgp_ids A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @export
setClass(
  "releases",
  slots = c(
    releases = "tbl_df",
    pgs_ids = "tbl_df",
    ppm_ids = "tbl_df",
    pgp_ids = "tbl_df"
  )
)

#' Constructor for the S4 releases object.
#'
#' Constructor for the S4 \linkS4class{releases} object.
#'
#' @param releases A \code{\link{s4releases_releases_tbl}} tibble.
#' @param pgs_ids A \code{\link{s4releases_pgs_ids_tbl}} tibble.
#' @param ppm_ids A \code{\link{s4releases_ppm_ids_tbl}} tibble.
#' @param pgp_ids A \code{\link{s4releases_pgp_ids_tbl}} tibble.
#'
#' @return An object of class \linkS4class{releases}.
#' @keywords internal
releases <-
  function(releases = s4releases_releases_tbl(),
           pgs_ids = s4releases_pgs_ids_tbl(),
           ppm_ids = s4releases_ppm_ids_tbl(),
           pgp_ids = s4releases_pgp_ids_tbl()) {

    s4_releases <- methods::new(
      "releases",
      releases = releases,
      pgs_ids = pgs_ids,
      ppm_ids = ppm_ids,
      pgp_ids = pgp_ids
    )

    return(s4_releases)
  }

s4releases_releases_tbl <- function(date = lubridate::ymd(),
                                    n_pgs = n_pgs,
                                    n_ppm = n_ppm,
                                    n_pgp = n_pgp,
                                    notes = notes) {
  tbl <- tibble::tibble(
    date = date,
    n_pgs = n_pgs,
    n_ppm = n_ppm,
    n_pgp = n_pgp,
    notes = notes
  )

  return(tbl)
}

s4releases_pgs_ids_tbl <- function(date = lubridate::ymd(),
                                   pgs_id = character()) {
  tbl <- tibble::tibble(date = date,
                        pgs_id = pgs_id)

  return(tbl)
}

s4releases_ppm_ids_tbl <- function(date = lubridate::ymd(),
                                   ppm_id = character()) {
  tbl <- tibble::tibble(date = date,
                        ppm_id = ppm_id)

  return(tbl)
}

s4releases_pgp_ids_tbl <- function(date = lubridate::ymd(),
                                   pgp_id = character()) {
  tbl <- tibble::tibble(date = date,
                        pgp_id = pgp_id)

  return(tbl)
}


coerce_to_s4_releases <- function(lst_tbl = NULL) {

  if (is.null(lst_tbl)) {
    s4_releases <- releases()
    return(s4_releases)
  }

  s4_releases <- releases(
    releases = lst_tbl$releases,
    pgs_ids = lst_tbl$pgs_ids,
    ppm_ids = lst_tbl$ppm_ids,
    pgp_ids = lst_tbl$pgp_ids
  )

  s4_releases@releases <- drop_metadata_cols(s4_releases@releases)
  s4_releases@pgs_ids <- drop_metadata_cols(s4_releases@pgs_ids)
  s4_releases@ppm_ids <- drop_metadata_cols(s4_releases@ppm_ids)
  s4_releases@pgp_ids <- drop_metadata_cols(s4_releases@pgp_ids)

  return(s4_releases)
}
