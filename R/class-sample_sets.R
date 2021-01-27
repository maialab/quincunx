setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Sample Sets
#'
#' The sample_sets object consists of four slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of PGS Catalog sample sets. Each sample set is an observation (row) in
#' the \code{sample_sets} table --- main table. All tables have the column
#' \code{pss_id} as primary key.
#'
#' @slot sample_sets
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot samples A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot demographics A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot cohorts A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @export
setClass(
  "sample_sets",
  slots = c(
    sample_sets = "tbl_df",
    samples = "tbl_df",
    demographics = "tbl_df",
    cohorts = "tbl_df"
  )
)

#' Constructor for the S4 sample_sets object.
#'
#' Constructor for the S4 \linkS4class{sample_sets} object.
#'
#' @param sample_sets A \code{\link{s4pss_sample_sets_tbl}} tibble.
#' @param samples A \code{\link{s4pss_samples_tbl}} tibble.
#' @param demographics A \code{\link{s4pss_demographics_tbl}} tibble.
#' @param cohorts A \code{\link{s4pss_pgs_cohorts_tbl}} tibble.
#'
#' @return An object of class \linkS4class{sample_sets}.
#' @keywords internal
sample_sets <-
  function(
           sample_sets = s4pss_sample_sets_tbl(),
           samples = s4pss_samples_tbl(),
           demographics = s4pss_demographics_tbl(),
           cohorts = s4pss_pgs_cohorts_tbl()) {

    s4_sample_sets <- methods::new(
      "sample_sets",
      sample_sets = sample_sets,
      samples = samples,
      demographics = demographics,
      cohorts = cohorts
    )

    return(s4_sample_sets)
  }

s4pss_sample_sets_tbl <- function(pss_id = character()) {

  tbl <- tibble::tibble(pss_id = pss_id)

  return(tbl)
}

s4pss_samples_tbl <- function(pss_id = character(),
                              sample_id = integer(),
                              sample_size = integer(),
                              sample_cases = integer(),
                              sample_controls = integer(),
                              sample_percent_male = double(),
                              phenotype_description = character(),
                              ancestry = character(),
                              ancestry_description = character(),
                              ancestry_country = character(),
                              ancestry_additional_description = character(),
                              study_id = character(),
                              pubmed_id = character(),
                              cohorts_additional_description = character()) {

  tbl <- tibble::tibble(
    pss_id = pss_id,
    sample_id = sample_id,
    sample_size = sample_size,
    sample_cases = sample_cases,
    sample_controls = sample_controls,
    sample_percent_male = sample_percent_male,
    phenotype_description = phenotype_description,
    ancestry = ancestry,
    ancestry_description = ancestry_description,
    ancestry_country = ancestry_country,
    ancestry_additional_description = ancestry_additional_description,
    study_id = study_id,
    pubmed_id = pubmed_id,
    cohorts_additional_description = cohorts_additional_description
  )

  return(tbl)
}

s4pss_demographics_tbl <- function(pss_id = character(),
                                 sample_id = integer(),
                                 variable = character(),
                                 estimate_type = character(),
                                 estimate = double(),
                                 unit = character(),
                                 variability_type = character(),
                                 variability = double(),
                                 interval_type = character(),
                                 interval_lower = double(),
                                 interval_upper = double()) {


  tbl <- tibble::tibble(
    pss_id = pss_id,
    sample_id = sample_id,
    variable = variable,
    estimate_type = estimate_type,
    estimate = estimate,
    unit = unit,
    variability_type = variability_type,
    variability = variability,
    interval_type = interval_type,
    interval_lower = interval_lower,
    interval_upper = interval_upper
  )

  return(tbl)
}

s4pss_pgs_cohorts_tbl  <- function(pss_id = character(),
                                   sample_id = integer(),
                                   cohort_symbol = character(),
                                   cohort_name = character()) {

  tbl <- tibble::tibble(
    pss_id = pss_id,
    sample_id = sample_id,
    cohort_symbol = cohort_symbol,
    cohort_name = cohort_name
  )

  return(tbl)
}

coerce_to_s4_sample_sets <- function(lst_tbl = NULL) {

  if (is.null(lst_tbl)) {
    s4_sample_sets <- sample_sets()
    return(s4_sample_sets)
  }

    s4_sample_sets <- sample_sets(
      sample_sets = lst_tbl$sample_sets,
      samples = lst_tbl$samples,
      demographics = lst_tbl$demographics,
      cohorts = lst_tbl$cohorts
    )

    s4_sample_sets@sample_sets <- drop_metadata_cols(s4_sample_sets@sample_sets)
    s4_sample_sets@samples <- drop_metadata_cols(s4_sample_sets@samples)
    s4_sample_sets@demographics <- drop_metadata_cols(s4_sample_sets@demographics)
    s4_sample_sets@cohorts <- drop_metadata_cols(s4_sample_sets@cohorts)

  return(s4_sample_sets)
}
