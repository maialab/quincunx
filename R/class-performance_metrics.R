setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Performance Metrics
#'
#' The performance_metrics object consists of nine tables (slots) that combined
#' form a relational database of a subset of performance metrics. Each
#' performance metric is an observation (row) in the \code{scores} table (first
#' table).
#'
#' @slot performance_metrics A table of PGS Performance Metrics (PPM). Each PPM (row) is
#'   uniquely identified by the \code{ppm_id} column. Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
#' \item{pgs_id}{Polygenic Score (PGS) identifier.}
#' \item{reported_trait}{The author-reported trait that the PGS has been
#' developed to predict. Example: \code{"Breast Cancer"}.}
#' \item{covariates}{Comma-separated list of covariates used in the prediction
#' model to evaluate the PGS.}
#' \item{comments}{Any other information relevant to the understanding of the
#' performance metrics.}
#' }
#' @slot publications A table of publications. Each publication (row) is
#'   uniquely identified by the column \code{pgp_id}. Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
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
#' }
#' @slot sample_sets A table of sample sets. Each sample set (row) is uniquely
#'   identified by the column \code{pss_id}. Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
#' \item{pss_id}{A PGS Sample Set identifier. Example: \code{"PSS000042"}.}
#' }
#' @slot samples A table of samples. Each sample (row) is uniquely identified by
#'   the combination of values from the columns: \code{ppm_id}, \code{pss_id},
#'   and \code{sample_id}. Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
#' \item{pss_id}{A PGS Sample Set identifier. Example: \code{"PSS000042"}.}
#' \item{sample_id}{Sample identifier. This is a surrogate key to identify each sample.}
#' \item{stage}{Sample stage: should be always \code{"evaluation"}.}
#' \item{sample_size}{Number of individuals included in the sample.}
#' \item{sample_cases}{Number of cases.}
#' \item{sample_controls}{Number of controls.}
#' \item{sample_percent_male}{Percentage of male participants.}
#' \item{phenotype_description}{Detailed phenotype description.}
#' \item{ancestry}{Author reported ancestry is mapped to the best matching
#' ancestry category from the NHGRI-EBI GWAS Catalog framework (see
#' \code{\link[quincunx]{ancestry_categories}}) for possible values.}
#' \item{ancestry_description}{A more detailed description of sample ancestry
#' that usually matches the most specific description described by the authors
#' (e.g. French, Chinese).}
#' \item{ancestry_country}{Author reported countries of recruitment (if available).}
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
#'   values from the columns: \code{ppm_id}, \code{pss_id}, \code{sample_id},
#'   and \code{variable}. Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
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
#'   the combination of values from the columns: \code{ppm_id}, \code{sample_id}
#'   and \code{cohort_symbol}. Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
#' \item{sample_id}{Sample identifier. This is a surrogate key to identify each sample.}
#' \item{cohort_symbol}{Cohort symbol.}
#' \item{cohort_name}{Cohort full name.}
#' }
#' @slot pgs_effect_sizes A table of effect sizes per standard deviation change
#'   in PGS. Examples include regression coefficients (betas) for continuous
#'   traits, odds ratios (OR) and/or hazard ratios (HR) for dichotomous traits
#'   depending on the availability of time-to-event data. Each effect size is
#'   uniquely identified by the combination of values from the columns:
#'   \code{ppm_id} and \code{effect_size_id}. Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
#' \item{effect_size_id}{Effect size identifier. This is a surrogate identifier to
#' identify each effect size.}
#' \item{estimate_type_long}{Long notation of the effect size (e.g. Odds Ratio).}
#' \item{estimate_type}{Short notation of the effect size (e.g. OR).}
#' \item{estimate}{The estimate's value.}
#' \item{unit}{Unit of the estimate.}
#' \item{variability_type}{Measure of statistical dispersion for variable, e.g.
#' standard error (se) or standard deviation (sd).}
#' \item{variability}{The value of the measure of dispersion.}
#' \item{interval_type}{Type of statistical interval for variable: range, iqr
#' (interquartile), ci (confidence interval).}
#' \item{interval_lower}{Interval lower bound.}
#' \item{interval_upper}{Interval upper bound.}
#' }
#' @slot pgs_classification_metrics A table of classification metrics. Examples
#'   include the Area under the Receiver Operating Characteristic (AUROC) or
#'   Harrell's C-index (Concordance statistic). Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
#' \item{classification_metrics_id}{Classification metric identifier. This is a
#' surrogate identifier to identify each classification metric.}
#' \item{estimate_type_long}{Long notation of the classification metric (e.g.
#' Concordance Statistic).}
#' \item{estimate_type}{Short notation classification metric (e.g. C-index).}
#' \item{estimate}{The estimate's value.}
#' \item{unit}{Unit of the estimate.}
#' \item{variability_type}{Measure of statistical dispersion for variable, e.g.
#' standard error (se) or standard deviation (sd).}
#' \item{variability}{The value of the measure of dispersion.}
#' \item{interval_type}{Type of statistical interval for variable: range, iqr
#' (interquartile), ci (confidence interval).}
#' \item{interval_lower}{Interval lower bound.}
#' \item{interval_upper}{Interval upper bound.}
#' }
#' @slot pgs_other_metrics A table of other metrics that are neither effect
#'   sizes nor classification metrics. Examples include: R² (proportion of the
#'   variance explained), or reclassification metrics. Columns:
#' \describe{
#' \item{ppm_id}{A PGS Performance Metrics identifier. Example: \code{"PPM000001"}.}
#' \item{other_metrics_id}{Other metric identifier. This is a
#' surrogate identifier to identify each metric.}
#' \item{estimate_type_long}{Long notation of the metric. Example: "Proportion of the variance explained".}
#' \item{estimate_type}{Short notation metric. Example: "R²".}
#' \item{estimate}{The estimate's value.}
#' \item{unit}{Unit of the estimate.}
#' \item{variability_type}{Measure of statistical dispersion for variable, e.g.
#' standard error (se) or standard deviation (sd).}
#' \item{variability}{The value of the measure of dispersion.}
#' \item{interval_type}{Type of statistical interval for variable: range, iqr
#' (interquartile), ci (confidence interval).}
#' \item{interval_lower}{Interval lower bound.}
#' \item{interval_upper}{Interval upper bound.}
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
#' @param performance_metrics A \code{s4ppm_performance_metrics_tbl} tibble.
#' @param publications A \code{s4ppm_publications_tbl} tibble.
#' @param sample_sets A \code{s4ppm_sample_sets_tbl} tibble.
#' @param samples A \code{s4ppm_samples_tbl} tibble.
#' @param demographics A \code{s4ppm_demographics_tbl} tibble.
#' @param cohorts A \code{s4ppm_pgs_cohorts_tbl} tibble.
#' @param pgs_effect_sizes A \code{s4ppm_pgs_effect_sizes_tbl} tibble.
#' @param pgs_classification_metrics A \code{s4ppm_pgs_classification_metrics_tbl} tibble.
#' @param pgs_other_metrics A \code{s4ppm_pgs_other_metrics_tbl} tibble.
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
                              cohorts_additional_description = character()) {

  tbl <- tibble::tibble(
    ppm_id = ppm_id,
    pss_id = pss_id,
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
