setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Publications
#'
#' The publications object consists of two tables (slots), each a table that
#' combined form a relational database of a subset of PGS Catalog Publications.
#' Each publication is an observation (row) in the \code{publications} table
#' (first table).
#'
#' @slot publications  A table of publications. Each publication (row) is
#'   uniquely identified by the \code{pgp_id} column. Columns:
#' \describe{
#' \item{pgp_id}{PGS Publication identifier. Example: \code{"PGP000001"}.}
#' \item{pubmed_id}{\href{https://en.wikipedia.org/wiki/PubMed}{PubMed}
#' identifier. Example: \code{"25855707"}.}
#' \item{publication_date}{Publication date. Example: \code{"2020-09-28"}. Note
#' that the class of \code{publication_date} is \code{\link[base]{Date}}.}
#' \item{publication}{Abbreviated name of the journal. Example: \code{"Am J Hum
#' Genet"}.}
#' \item{title}{Publication title.}
#' \item{author_fullname}{First author of the publication. Example:
#' \code{'Mavaddat N'}.}
#' \item{doi}{Digital Object Identifier (DOI). This variable is also curated to
#' allow unpublished work (e.g. pre-prints) to be added to the catalog. Example:
#' \code{"10.1093/jnci/djv036"}.}
#' \item{authors}{Concatenated list of all the publication authors.}
#' }
#' @slot pgs_ids A table of publication and associated PGS identifiers. Columns:
#' \describe{
#' \item{pgp_id}{PGS Publication identifier. Example: \code{"PGP000001"}.}
#' \item{pgs_id}{Polygenic Score (PGS) identifier.}
#' \item{stage}{PGS stage: either "development" or "evaluation".}
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
                        pgs_id = character(),
                        stage = character()) {

  tbl <- tibble::tibble(
    pgp_id = pgp_id,
    pgs_id = pgs_id,
    stage = stage
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
