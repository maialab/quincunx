setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Performance Metrics
#'
#' The performance_metrics object consists of nine slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of performance metrics. Each performance metric is an observation
#' (row) in the \code{scores} table --- main table. All tables have the column
#' \code{ppm_id} as primary key.
#'
#' @slot performance_metrics
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot publications A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot sample_sets A \code{\link[tibble]{tibble}} listing TODO. Columns:
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
#' @slot pgs_effect_sizes A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot pgs_classification_metrics A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot pgs_other_metrics A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @export
setClass(
  "performance_metrics",
  slots = c(
    performance_metrics = "tbl_df",
    publications = "tbl_df",
    sample_sets = "tbl_df",
    samples = "tbl_df",
    demographics = "tbl_df",
    cohorts = "tbl_df",
    pgs_effect_sizes = "tbl_df",
    pgs_classification_metrics = "tbl_df",
    pgs_other_metrics = "tbl_df"
  )
)

#' Constructor for the S4 performance_metrics object.
#'
#' Constructor for the S4 \linkS4class{performance_metrics} object.
#'
#' @param performance_metrics A \code{\link{s4ppm_performance_metrics_tbl}} tibble.
#' @param publications A \code{\link{s4ppm_publications_tbl}} tibble.
#' @param sample_sets A \code{\link{s4ppm_sample_sets_tbl}} tibble.
#' @param samples A \code{\link{s4ppm_samples_tbl}} tibble.
#' @param demographics A \code{\link{s4ppm_demographics_tbl}} tibble.
#' @param cohorts A \code{\link{s4ppm_pgs_cohorts_tbl}} tibble.
#' @param pgs_effect_sizes A \code{\link{s4ppm_pgs_effect_sizes_tbl}} tibble.
#' @param pgs_classification_metrics A \code{\link{s4ppm_pgs_classification_metrics_tbl}} tibble.
#' @param pgs_other_metrics A \code{\link{s4ppm_pgs_other_metrics_tbl}} tibble.
#'
#' @return An object of class \linkS4class{performance_metrics}.
#' @keywords internal
performance_metrics <-
  function(performance_metrics = s4ppm_performance_metrics_tbl(),
           publications = s4ppm_publications_tbl(),
           sample_sets = s4ppm_sample_sets_tbl(),
           samples = s4ppm_samples_tbl(),
           demographics = s4ppm_demographics_tbl(),
           cohorts = s4ppm_pgs_cohorts_tbl(),
           pgs_effect_sizes = s4ppm_pgs_effect_sizes_tbl(),
           pgs_classification_metrics = s4ppm_pgs_classification_metrics_tbl(),
           pgs_other_metrics = s4ppm_pgs_other_metrics_tbl()) {

    s4_performance_metrics <- methods::new(
      "performance_metrics",
      performance_metrics = performance_metrics,
      publications = publications,
      sample_sets = sample_sets,
      samples = samples,
      demographics = demographics,
      cohorts = cohorts,
      pgs_effect_sizes = pgs_effect_sizes,
      pgs_classification_metrics = pgs_classification_metrics,
      pgs_other_metrics = pgs_other_metrics
    )

    return(s4_performance_metrics)
  }

s4ppm_performance_metrics_tbl <- function(ppm_id = character(),
                                          pgs_id = character(),
                                          reported_trait = character(),
                                          covariates = character(),
                                          comments = character()) {

  tbl <- tibble::tibble(
    ppm_id = ppm_id,
    pgs_id = pgs_id,
    reported_trait = reported_trait,
    covariates = covariates,
    comments = comments
  )

  return(tbl)
}

s4ppm_publications_tbl <- function(ppm_id = character(),
                                   pubmed_id = character(),
                                   publication_date = character(),
                                   publication = character(),
                                   title = character(),
                                   author_fullname = character(),
                                   doi = character()) {
  tbl <- tibble::tibble(
    ppm_id = ppm_id,
    pubmed_id = pubmed_id,
    publication_date = publication_date,
    publication = publication,
    title = title,
    author_fullname = author_fullname,
    doi = doi
  )

  return(tbl)
}

s4ppm_sample_sets_tbl <- function(ppm_id = character(),
                                  pss_id = character()) {


  tbl <- tibble::tibble(
    ppm_id = ppm_id,
    pss_id = pss_id
  )

  return(tbl)
}

s4ppm_samples_tbl <- function(ppm_id = character(),
                              pss_id = character(),
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
    ppm_id = ppm_id,
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

s4ppm_demographics_tbl <- function(ppm_id = character(),
                                 pss_id = character(),
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
    ppm_id = ppm_id,
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

s4ppm_pgs_cohorts_tbl  <- function(ppm_id = character(),
                                   pss_id = character(),
                                   sample_id = integer(),
                                   cohort_symbol = character(),
                                   cohort_name = character()) {

  tbl <- tibble::tibble(
    ppm_id = ppm_id,
    pss_id = pss_id,
    sample_id = sample_id,
    cohort_symbol = cohort_symbol,
    cohort_name = cohort_name
  )

  return(tbl)
}

s4ppm_pgs_effect_sizes_tbl <- function(ppm_id = character(),
                                       effect_size_id = integer(),
                                       estimate_type_long = character(),
                                       estimate_type = character(),
                                       estimate = double(),
                                       unit = character(),
                                       variability_type = character(),
                                       variability = double(),
                                       interval_type = character(),
                                       interval_lower = double(),
                                       interval_upper = double()) {

  tbl <- tibble::tibble(
    ppm_id = ppm_id,
    effect_size_id = effect_size_id,
    estimate_type_long = estimate_type_long,
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

s4ppm_pgs_classification_metrics_tbl <-
  function(ppm_id = character(),
           classification_metrics_id = integer(),
           estimate_type_long = character(),
           estimate_type = character(),
           estimate = double(),
           unit = character(),
           variability_type = character(),
           variability = double(),
           interval_type = character(),
           interval_lower = double(),
           interval_upper = double()) {


    tbl <- tibble::tibble(
      ppm_id = ppm_id,
      classification_metrics_id = classification_metrics_id,
      estimate_type_long = estimate_type_long,
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

s4ppm_pgs_other_metrics_tbl <- function(ppm_id = character(),
                                        other_metrics_id = integer(),
                                        estimate_type_long = character(),
                                        estimate_type = character(),
                                        estimate = double(),
                                        unit = character(),
                                        variability_type = character(),
                                        variability = double(),
                                        interval_type = character(),
                                        interval_lower = double(),
                                        interval_upper = double()) {

  tbl <- tibble::tibble(
    ppm_id = ppm_id,
    other_metrics_id = other_metrics_id,
    estimate_type_long = estimate_type_long,
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

coerce_to_s4_performance_metrics <- function(lst_tbl = NULL) {

  if (is.null(lst_tbl)) {
    s4_performance_metrics <- performance_metrics()
    return(s4_performance_metrics)
  }

    s4_performance_metrics <- performance_metrics(
      performance_metrics = lst_tbl$performance_metrics,
      publications = lst_tbl$publications,
      sample_sets = lst_tbl$sample_sets,
      samples = lst_tbl$samples,
      demographics = lst_tbl$demographics,
      cohorts = lst_tbl$cohorts,
      pgs_effect_sizes = lst_tbl$pgs_effect_sizes,
      pgs_classification_metrics = lst_tbl$pgs_classification_metrics,
      pgs_other_metrics = lst_tbl$pgs_other_metrics
    )

    s4_performance_metrics@performance_metrics <- drop_metadata_cols(s4_performance_metrics@performance_metrics)
    s4_performance_metrics@publications <- drop_metadata_cols(s4_performance_metrics@publications)
    s4_performance_metrics@sample_sets <- drop_metadata_cols(s4_performance_metrics@sample_sets)
    s4_performance_metrics@samples <- drop_metadata_cols(s4_performance_metrics@samples)
    s4_performance_metrics@demographics <- drop_metadata_cols(s4_performance_metrics@demographics)
    s4_performance_metrics@cohorts <- drop_metadata_cols(s4_performance_metrics@cohorts)
    s4_performance_metrics@pgs_effect_sizes <- drop_metadata_cols(s4_performance_metrics@pgs_effect_sizes)
    s4_performance_metrics@pgs_classification_metrics <- drop_metadata_cols(s4_performance_metrics@pgs_classification_metrics)
    s4_performance_metrics@pgs_other_metrics <- drop_metadata_cols(s4_performance_metrics@pgs_other_metrics)

  return(s4_performance_metrics)
}
