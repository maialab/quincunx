setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Sample Sets
#'
#' The sample_sets object consists of four tables (slots) that combined form a
#' relational database of a subset of PGS Catalog sample sets. Each sample set
#' is an observation (row) in the \code{sample_sets} table (first table).
#'
#' @slot sample_sets A table of sample sets. Each sample set (row) is uniquely
#'   identified by the column \code{pss_id}. Columns:
#' \describe{
#' \item{pss_id}{A PGS Sample Set identifier. Example: \code{"PSS000042"}.}
#' }
#' @slot samples A table of samples. Each sample (row) is uniquely identified by
#'   the combination of values from the columns: \code{pss_id} and
#'   \code{sample_id}. Columns:
#' \describe{
#' \item{pss_id}{A PGS Sample Set identifier. Example: \code{"PSS000042"}.}
#' \item{sample_id}{Sample identifier. This is a surrogate key to identify each sample.}
#' \item{stage}{Sample stage: should be always \code{"evaluation"}.}
#' \item{sample_size}{Number of individuals included in the sample.}
#' \item{sample_cases}{Number of cases.}
#' \item{sample_controls}{Number of controls.}
#' \item{sample_percent_male}{Percentage of male participants.}
#' \item{phenotype_description}{Detailed phenotype description.}
#' \item{ancestry_category}{Author reported ancestry is mapped to the best matching
#' ancestry category from the NHGRI-EBI GWAS Catalog framework (see
#' \code{\link[quincunx]{ancestry_categories}}) for possible values.}
#' \item{ancestry}{A more detailed description of sample ancestry
#' that usually matches the most specific description described by the authors
#' (e.g. French, Chinese).}
#' \item{country}{Author reported countries of recruitment (if available).}
#' \item{ancestry_additional_description}{Any additional description not
#' captured in the other columns (e.g. founder or genetically isolated
#' populations, or further description of admixed samples).}
#' \item{study_id}{Associated GWAS Catalog study accession identifier, e.g.,
#' \code{"GCST002735"}.}
#' \item{pubmed_id}{\href{https://en.wikipedia.org/wiki/PubMed}{PubMed}
#' identifier.}
#' \item{cohorts_additional_description}{Any additional description about the
#' samples (e.g. sub-cohort information).}
#' }
#' @slot demographics A table of sample demographics' variables. Each
#'   demographics' variable (row) is uniquely identified by the combination of
#'   values from the columns: \code{pss_id}, \code{sample_id}, and
#'   \code{variable}. Columns:
#' \describe{
#' \item{pss_id}{A PGS Sample Set identifier. Example: \code{"PSS000042"}.}
#' \item{sample_id}{Sample identifier. This is a surrogate identifier to
#' identify each sample.}
#' \item{variable}{Demographics variable. Following columns report about the
#' indicated variable.}
#' \item{estimate_type}{Type of statistical estimate for variable.}
#' \item{estimate}{The variable's statistical value.}
#' \item{unit}{Unit of the variable.}
#' \item{variability_type}{Measure of statistical dispersion for variable, e.g.
#' standard error (se) or standard deviation (sd).}
#' \item{variability}{The value of the measure of dispersion.}
#' \item{interval_type}{Type of statistical interval for variable: range, iqr
#' (interquartile), ci (confidence interval).}
#' \item{interval_lower}{Interval lower bound.}
#' \item{interval_upper}{Interval upper bound.}
#' }
#' @slot cohorts A table of cohorts. Each cohort (row) is uniquely identified by
#'   the combination of values from the columns: \code{pss_id}, \code{sample_id}
#'   and \code{cohort_symbol}. Columns:
#' \describe{
#' \item{pss_id}{A PGS Sample Set identifier. Example: \code{"PSS000042"}.}
#' \item{sample_id}{Sample identifier. This is a surrogate key to identify each sample.}
#' \item{cohort_symbol}{Cohort symbol.}
#' \item{cohort_name}{Cohort full name.}
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
#' @param sample_sets A \code{s4pss_sample_sets_tbl} tibble.
#' @param samples A \code{s4pss_samples_tbl} tibble.
#' @param demographics A \code{s4pss_demographics_tbl} tibble.
#' @param cohorts A \code{s4pss_pgs_cohorts_tbl} tibble.
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
                              stage = character(),
                              sample_size = integer(),
                              sample_cases = integer(),
                              sample_controls = integer(),
                              sample_percent_male = double(),
                              phenotype_description = character(),
                              ancestry_category = character(),
                              ancestry = character(),
                              country = character(),
                              ancestry_additional_description = character(),
                              study_id = character(),
                              pubmed_id = character(),
                              cohorts_additional_description = character()) {

  tbl <- tibble::tibble(
    pss_id = pss_id,
    sample_id = sample_id,
    stage = stage,
    sample_size = sample_size,
    sample_cases = sample_cases,
    sample_controls = sample_controls,
    sample_percent_male = sample_percent_male,
    phenotype_description = phenotype_description,
    ancestry_category = ancestry_category,
    ancestry = ancestry,
    country = country,
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
