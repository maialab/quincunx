setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Polygenic Scores
#'
#' The scores object consists of eleven slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of PGS Catalog polygenic scores. Each score is an observation (row) in
#' the \code{scores} table --- main table. All tables have the column
#' \code{pgs_id} as primary key.
#'
#' @slot scores
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @slot publications A \code{\link[tibble]{tibble}} listing TODO. Columns:
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
#' @slot traits A \code{\link[tibble]{tibble}} listing TODO. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' \item{TODO}{TODO}
#' }
#' @export
setClass(
  "scores",
  slots = c(
    scores = "tbl_df",
    publications = "tbl_df",
    samples = "tbl_df",
    demographics = "tbl_df",
    cohorts = "tbl_df",
    traits = "tbl_df"
  )
)

#' Constructor for the S4 scores object.
#'
#' Constructor for the S4 \linkS4class{scores} object.
#'
#' @param scores A \code{\link{s4scores_scores_tbl}} tibble.
#' @param publications A \code{\link{s4scores_publications_tbl}} tibble.
#' @param samples A \code{\link{s4scores_samples_tbl}} tibble.
#' @param demographics A \code{\link{s4scores_demographics_tbl}} tibble.
#' @param cohorts A \code{\link{s4scores_cohorts_tbl}} tibble.
#' @param traits A \code{\link{s4scores_traits_tbl}} tibble.
#'
#' @return An object of class \linkS4class{scores}.
#' @keywords internal
scores <-
  function(scores = s4scores_scores_tbl(),
           publications = s4scores_publications_tbl(),
           samples = s4scores_samples_tbl(),
           demographics = s4scores_demographics_tbl(),
           cohorts = s4scores_cohorts_tbl(),
           traits = s4scores_traits_tbl()) {

    s4_scores <- methods::new(
      "scores",
      scores = scores,
      publications = publications,
      samples = samples,
      demographics = demographics,
      cohorts = cohorts,
      traits = traits
    )

  return(s4_scores)
}

s4scores_scores_tbl <- function(pgs_id = character(),
                                pgs_name = character(),
                                scoring_file = character(),
                                matches_publication = logical(),
                                reported_trait = character(),
                                trait_additional_description = character(),
                                pgs_method_name = character(),
                                pgs_method_params = character(),
                                n_variants = integer(),
                                n_variant_interactions = integer(),
                                assembly = character(),
                                license = character()) {

  tbl <- tibble::tibble(
    pgs_id = pgs_id,
    pgs_name = pgs_name,
    scoring_file = scoring_file,
    matches_publication = matches_publication,
    reported_trait = reported_trait,
    trait_additional_description = trait_additional_description,
    pgs_method_name = pgs_method_name,
    pgs_method_params = pgs_method_params,
    n_variants = n_variants,
    n_variant_interactions = n_variant_interactions,
    assembly = assembly,
    license = license
  )

  return(tbl)
}

s4scores_publications_tbl <- function(
  pgs_id = character(),
  pubmed_id = character(),
  publication_date = lubridate::ymd(),
  publication = character(),
  title = character(),
  author_fullname = character(),
  doi = character()
) {

  tbl <- tibble::tibble(
    pgs_id = pgs_id,
    pubmed_id = pubmed_id,
    publication_date = publication_date,
    publication = publication,
    title = title,
    author_fullname = author_fullname,
    doi = doi
  )

  return(tbl)
}

s4scores_samples_tbl <- function(
  pgs_id = character(),
  sample_id = integer(),
  stage = character(),
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
  cohorts_additional_description = character()
) {

  tbl <- tibble::tibble(
    pgs_id = pgs_id,
    sample_id = sample_id,
    stage = stage,
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

s4scores_demographics_tbl <- function(
  pgs_id = character(),
  sample_id = integer(),
  stage = character(),
  estimate_type = character(),
  estimate = double(),
  interval_type = character(),
  interval_lower = double(),
  interval_upper = double(),
  variability_type = character(),
  variability = double(),
  unit = character()
) {

  tbl <- tibble::tibble(
    pgs_id = pgs_id,
    sample_id = sample_id,
    stage = stage,
    estimate_type = estimate_type,
    estimate = estimate,
    interval_type = interval_type,
    interval_lower = interval_lower,
    interval_upper = interval_upper,
    variability_type = variability_type,
    variability = variability,
    unit = unit
  )

  return(tbl)

}

s4scores_cohorts_tbl <- function(
  pgs_id = character(),
  sample_id = integer(),
  stage = character(),
  cohort_symbol = character(),
  cohort_name = character()
) {

  tbl <- tibble::tibble(
    pgs_id = pgs_id,
    sample_id = sample_id,
    stage = stage,
    cohort_symbol = cohort_symbol,
    cohort_name = cohort_name
  )

  return(tbl)

}

s4scores_traits_tbl <- function(
  pgs_id = character(),
  efo_id = character(),
  trait = character(),
  description = character(),
  url = character()
) {

  tbl <- tibble::tibble(
    pgs_id = pgs_id,
    efo_id = efo_id,
    trait = trait,
    description = description,
    url = url
  )

  return(tbl)

}

coerce_to_s4_scores <- function(lst_tbl = NULL) {

  if (is.null(lst_tbl)) {
    s4_scores <- scores()
    return(s4_scores)
  }

    s4_scores <- scores(
      scores = lst_tbl$scores,
      publications = lst_tbl$publications,
      samples = lst_tbl$samples,
      demographics = lst_tbl$demographics,
      cohorts = lst_tbl$cohorts,
      traits = lst_tbl$traits
    )

    s4_scores@scores <- drop_metadata_cols(s4_scores@scores)
    s4_scores@publications <- drop_metadata_cols(s4_scores@publications)
    s4_scores@samples <- drop_metadata_cols(s4_scores@samples)
    s4_scores@demographics <- drop_metadata_cols(s4_scores@demographics)
    s4_scores@cohorts <- drop_metadata_cols(s4_scores@cohorts)
    s4_scores@traits <- drop_metadata_cols(s4_scores@traits)

  return(s4_scores)
}
