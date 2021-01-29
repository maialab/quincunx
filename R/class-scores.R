setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Polygenic Scores
#'
#' The scores object consists of six tables (slots) that combined form a
#' relational database of a subset of PGS Catalog polygenic scores. Each score
#' is an observation (row) in the \code{scores} table (the first table).
#'
#' @slot scores A table of polygenic scores. Each polygenic score (row) is
#'   uniquely identified by the \code{pgs_id} column. Columns:
#' \describe{
#' \item{pgs_id}{Polygenic Score (PGS) identifier. Example: \code{"PGS000001"}.}
#' \item{pgs_name}{This may be the name that the authors describe the PGS with
#' in the source publication, or a name that a curator of the PGS Catalog has
#' assigned to identify the score during the curation process (before a PGS
#' identifier has been given). Example: \code{PRS77_BC}.}
#' \item{scoring_file}{URL to the scoring file on the PGS FTP server. Example:
#' \code{"http://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS000001/ScoringFiles/PGS000001.txt.gz"}.}
#' \item{matches_publication}{Indicate if the PGS data matches the published
#' polygenic score (\code{TRUE}). If not (\code{FALSE}), the authors have
#' provided an alternative polygenic for the Catalog and some other data, such
#' as performance metrics, may differ from the publication.}
#' \item{reported_trait}{The author-reported trait that the PGS has been
#' developed to predict. Example: \code{"Breast Cancer"}.}
#' \item{trait_additional_description}{Any additional description not captured
#' in the other columns. Example: \code{"Femoral neck BMD (g/cm2)"}.}
#' \item{pgs_method_name}{The name or description of the method or computational
#' algorithm used to develop the PGS.}
#' \item{pgs_method_params}{A description of the relevant inputs and parameters
#' relevant to the PGS development method/process.}
#' \item{n_variants}{Number of variants used to calculate the PGS.}
#' \item{n_variants_interactions}{Number of higher-order variant interactions
#' included in the PGS.}
#' \item{assembly}{The version of the genome assembly that the variants present
#' in the PGS are associated with. Example: \code{GRCh37}.}
#' \item{license}{The PGS Catalog distributes its data according to EBI’s
#' standard Terms of Use. Some PGS have specific terms, licenses, or
#' restrictions (e.g. non-commercial use) that we highlight in this field, if
#' known.}
#' }
#' @slot publications A table of publications. Each publication (row) is
#'   uniquely identified by the \code{pgp_id} column. Columns:
#' \describe{
#' \item{pgs_id}{Polygenic Score (PGS) identifier.}
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
#' @slot samples A table of samples. Each sample (row) is uniquely identified by
#'   the combination of values from the columns: \code{pgs_id}, \code{stage} and
#'   \code{sample_id}. Columns:
#' \describe{
#' \item{pgs_id}{Polygenic score identifier. An identifier that starts with
#' \code{'PGS'} and is followed by six digits, e.g. \code{'PGS000001'}.}
#' \item{stage}{PGS lifecycle stage: gwas, development or evaluation.}
#' \item{sample_id}{Sample identifier. This is a surrogate key to identify each sample.}
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
#'   values from the columns: \code{pgs_id}, \code{stage}, \code{sample_id} and
#'   \code{variable}. Columns:
#' \describe{
#' \item{pgs_id}{Polygenic Score (PGS) identifier.}
#' \item{stage}{PGS lifecycle stage: gwas, development or evaluation.}
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
#'   the combination of values from the columns: \code{pgs_id}, \code{stage},
#'   \code{sample_id} and \code{cohort_symbol}. Columns:
#' \describe{
#' \item{pgs_id}{Polygenic Score (PGS) identifier.}
#' \item{stage}{PGS lifecycle stage: gwas, development or evaluation.}
#' \item{sample_id}{Sample identifier. This is a surrogate key to identify each sample.}
#' \item{cohort_symbol}{Cohort symbol.}
#' \item{cohort_name}{Cohort full name.}
#' }
#' @slot traits A table of EFO traits. Each trait (row) is uniquely identified
#'   by the combination of the columns \code{pgs_id} and \code{efo_id}. Columns:
#' \describe{
#' \item{pgs_id}{Polygenic Score (PGS) identifier.}
#' \item{efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{trait}{Trait name.}
#' \item{description}{Detailed description of the trait from EFO.}
#' \item{url}{External link to the EFO entry.}
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
  pgp_id = character(),
  pubmed_id = character(),
  publication_date = lubridate::ymd(),
  publication = character(),
  title = character(),
  author_fullname = character(),
  doi = character()
) {

  tbl <- tibble::tibble(
    pgs_id = pgs_id,
    pgp_id = pgp_id,
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
  stage = character(),
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
  cohorts_additional_description = character()
) {

  tbl <- tibble::tibble(
    pgs_id = pgs_id,
    stage = stage,
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

s4scores_demographics_tbl <- function(
  pgs_id = character(),
  stage = character(),
  sample_id = integer(),
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
    stage = stage,
    sample_id = sample_id,
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
  stage = character(),
  sample_id = integer(),
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
