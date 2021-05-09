#' @importFrom rlang .data
unwrap_sample <- function(tbl_json) {

  tbl_json %>%
    tidyjson::spread_values(
      sample_size = tidyjson::jnumber('sample_number'),
      sample_cases = tidyjson::jnumber('sample_cases'),
      sample_controls = tidyjson::jnumber('sample_controls'),
      sample_percent_male = tidyjson::jnumber('sample_percent_male'),
      phenotype_description = tidyjson::jstring('phenotyping_free'),
      ancestry_category = tidyjson::jstring('ancestry_broad'),
      ancestry = tidyjson::jstring('ancestry_free'),
      country = tidyjson::jstring('ancestry_country'),
      ancestry_additional_description = tidyjson::jstring('ancestry_additional'),
      study_id = tidyjson::jstring('source_GWAS_catalog'),
      pubmed_id = tidyjson::jstring('source_PMID'),
      cohorts_additional_description = tidyjson::jstring('cohorts_additional')
    ) %>%
    # Coerce json number (default is R's double) to integer.
    dplyr::mutate(sample_size = as.integer(.data$sample_size),
                  sample_cases = as.integer(.data$sample_cases),
                  sample_controls = as.integer(.data$sample_controls)) %>%
    tidyjson::as_tibble()
}
