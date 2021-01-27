setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Publications
#'
#' The publications object consists of two slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of PGS Catalog Publications. Each publication is an observation (row) in
#' the \code{publications} table --- main table. All tables have the column
#' \code{pgp_id} as primary key.
#'
#' @slot publications
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot pgs_ids A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @export
setClass(
  "publications",
  slots = c(
    publications = "tbl_df",
    pgs_ids = "tbl_df"
  )
)

#' Constructor for the S4 publications object.
#'
#' Constructor for the S4 \linkS4class{publications} object.
#'
#' @param publications A \code{\link{publications_tbl}} tibble.
#' @param pgs_ids A \code{\link{pgs_ids_tbl}} tibble.
#'
#' @return An object of class \linkS4class{publications}.
#' @keywords internal
publications <- function(publications = publications_tbl(),
                         pgs_ids = pgs_ids_tbl()) {

  s4_publications <- methods::new("publications",
                                  publications = publications,
                                  pgs_ids = pgs_ids)

  return(s4_publications)
}

publications_tbl <- function(pgp_id = character(),
                             pubmed_id = character(),
                             publication_date = lubridate::ymd(),
                             publication = character(),
                             title = character(),
                             author_fullname = character(),
                             doi = character(),
                             authors = character()) {

  tbl <- tibble::tibble(
    pgp_id = pgp_id,
    pubmed_id = pubmed_id,
    publication_date = publication_date,
    publication = publication,
    title = title,
    author_fullname = author_fullname,
    doi = doi,
    authors = authors
  )

  return(tbl)
}

pgs_ids_tbl <- function(pgp_id = character(),
                        pgs_id = character()) {

  tbl <- tibble::tibble(
    pgp_id = pgp_id,
    pgs_id = pgs_id
  )

  return(tbl)
}

coerce_to_s4_publications <- function(lst_tbl = NULL, drop_request_cols = TRUE) {

  if (is.null(lst_tbl)) {
    s4_publications <- publications()
    return(s4_publications)
  }

    s4_publications <- publications(
      publications = lst_tbl$publications,
      pgs_ids = lst_tbl$pgs_ids
    )

    s4_publications@publications <- drop_metadata_cols(s4_publications@publications)
    s4_publications@pgs_ids <- drop_metadata_cols(s4_publications@pgs_ids)

  return(s4_publications)
}
