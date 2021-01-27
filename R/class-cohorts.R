setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of cohorts
#'
#' The cohorts object consists of two slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of PGS Catalog cohorts. Each cohort is an observation (row) in
#' the \code{cohorts} table --- main table.
#'
#' @slot cohorts
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
  "cohorts",
  slots = c(
    cohorts = "tbl_df",
    pgs_ids = "tbl_df"
  )
)

#' Constructor for the S4 cohorts object.
#'
#' Constructor for the S4 \linkS4class{cohorts} object.
#'
#' @param cohorts A \code{\link{s4cohorts_cohorts_tbl}} tibble.
#' @param pgs_ids A \code{\link{s4cohorts_pgs_ids_tbl}} tibble.
#'
#' @return An object of class \linkS4class{cohorts}.
#' @keywords internal
cohorts <-
  function(cohorts = s4cohorts_cohorts_tbl(),
           pgs_ids = s4cohorts_pgs_ids_tbl()) {

    s4_cohorts <- methods::new(
      "cohorts",
      cohorts = cohorts,
      pgs_ids = pgs_ids)

    return(s4_cohorts)
  }

s4cohorts_cohorts_tbl <- function(cohort_symbol = character(),
                                  cohort_name = character()) {

  tbl <- tibble::tibble(
    cohort_symbol = cohort_symbol,
    cohort_name = cohort_name
  )

  return(tbl)
}

s4cohorts_pgs_ids_tbl <- function(cohort_symbol = character(),
                                  pgs_id = character(),
                                  stage = character()) {



  tbl <- tibble::tibble(
    cohort_symbol = cohort_symbol,
    pgs_id = pgs_id,
    stage = stage
  )

  return(tbl)
}

coerce_to_s4_cohorts <- function(lst_tbl = NULL) {

  if (is.null(lst_tbl)) {
    s4_cohorts <- cohorts()
    return(s4_cohorts)
  }

  s4_cohorts <- cohorts(
    cohorts = lst_tbl$cohorts,
    pgs_ids = lst_tbl$pgs_ids
  )

  s4_cohorts@cohorts <- drop_metadata_cols(s4_cohorts@cohorts)
  s4_cohorts@pgs_ids <- drop_metadata_cols(s4_cohorts@pgs_ids)

  return(s4_cohorts)
}
