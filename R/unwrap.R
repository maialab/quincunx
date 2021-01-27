# unwrap <- function(tbl_json, obj) {
#
#   fn <- FALSE
#
#   if(identical(obj, 'Score')) fn <- unwrap_score
#   if(identical(obj, 'Sample')) fn <- unwrap_sample
#   if(identical(obj, 'SampleSet')) fn <- unwrap_sample_set
#   if(identical(obj, 'sample_age')) fn <- unwrap_sample_age
#   if(identical(obj, 'Cohort')) fn <- unwrap_cohort
#   if(identical(obj, 'Cohort_extended')) fn <- unwrap_cohort_extended
#   # if(identical(obj, 'Demographic')) fn <- unwrap_demographic
#   # if(identical(obj, 'Demographic2')) fn <- unwrap_demographic2
#   # if(identical(obj, 'Demographic3')) fn <- unwrap_demographic3
#   if(identical(obj, 'EFOtrait')) fn <- unwrap_efotrait
#   if(identical(obj, 'EFOTrait_extended')) fn <- unwrap_efotrait_extended
#   if(identical(obj, 'Publication')) fn <- unwrap_publication
#   # if(identical(obj, 'Publication_extended')) fn <- unwrap_publication_extended
#   # if(identical(obj, 'TraitCategory')) fn <- unwrap_trait_category
#   if(identical(obj, 'Release')) fn <- unwrap_release
#   # if(identical(obj, 'PerformanceMetric')) fn <- unwrap_performance_metric
#   if(identical(obj, 'released_score_ids')) fn <- unwrap_released_score_ids
#   if(identical(obj, 'released_performance_ids')) fn <- unwrap_released_performance_ids
#   if(identical(obj, 'released_publication_ids')) fn <- unwrap_released_publication_ids
#   if(identical(obj, 'trait_categories')) fn <- unwrap_trait_categories
#   if(identical(obj, 'trait_synonyms')) fn <- unwrap_trait_synonyms
#   if(identical(obj, 'trait_mapped_terms')) fn <- unwrap_trait_mapped_terms
#   if(identical(obj, 'associated_pgs_ids')) fn <- unwrap_associated_pgs_ids
#
#   if(!is.function(fn)) stop('No unwrap function for object `obj`: ', obj)
#   else return(fn(tbl_json))
#
# }

# unwrap_score <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(id = tidyjson::jstring('id'),
#                   name = tidyjson::jstring('name'),
#                   ftp_scoring_file = tidyjson::jstring('ftp_scoring_file'),
#                   matches_publication = tidyjson::jlogical('matches_publication'),
#                   trait_reported = tidyjson::jstring('trait_reported'),
#                   trait_additional = tidyjson::jstring('trait_additional'),
#                   method_name = tidyjson::jstring('method_name'),
#                   method_params = tidyjson::jstring('method_params'),
#                   variants_number = tidyjson::jnumber('variants_number'),
#                   variants_interactions = tidyjson::jnumber('variants_interactions'),
#                   variants_genomebuild = tidyjson::jstring('variants_genomebuild'),
#                   license = tidyjson::jstring('license')
#     ) %>%
#     # Coerce json number (default is R's double) to integer.
#     dplyr::mutate(variants_number = as.integer(variants_number),
#                   variants_interactions = as.integer(variants_interactions)) %>%
#     # Convert NR to NA in character columns
#     dplyr::mutate(id = nr_to_na(id),
#                   name = nr_to_na(name),
#                   ftp_scoring_file = nr_to_na(ftp_scoring_file),
#                   trait_reported = nr_to_na(trait_reported),
#                   trait_additional = nr_to_na(trait_additional),
#                   method_name = nr_to_na(method_name),
#                   method_params = nr_to_na(method_params),
#                   variants_genomebuild = nr_to_na(variants_genomebuild),
#                   license = nr_to_na(license)) %>%
#     tidyjson::as_tibble()
# }

# unwrap_sample <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(
#       sample_number = tidyjson::jnumber(sample_number),
#       sample_cases = tidyjson::jnumber(sample_cases),
#       sample_controls = tidyjson::jnumber(sample_controls),
#       sample_percent_male = tidyjson::jnumber(sample_percent_male),
#       phenotyping_free = tidyjson::jstring(phenotyping_free),
#       ancestry_broad = tidyjson::jstring(ancestry_broad),
#       ancestry_free = tidyjson::jstring(ancestry_free),
#       ancestry_country = tidyjson::jstring(ancestry_country),
#       ancestry_additional = tidyjson::jstring(ancestry_additional),
#       source_GWAS_catalog = tidyjson::jstring(source_GWAS_catalog),
#       source_PMID = tidyjson::jstring(source_PMID),
#       cohorts_additional = tidyjson::jstring(cohorts_additional)
#     ) %>%
#     # Coerce json number (default is R's double) to integer.
#     dplyr::mutate(sample_number = as.integer(sample_number),
#                   sample_cases = as.integer(sample_cases),
#                   sample_controls = as.integer(sample_controls)) %>%
#     # Convert NR to NA in character columns
#     dplyr::mutate(phenotyping_free = nr_to_na(phenotyping_free),
#                   ancestry_broad = nr_to_na(ancestry_broad),
#                   ancestry_free = nr_to_na(ancestry_free),
#                   ancestry_country = nr_to_na(ancestry_country),
#                   ancestry_additional = nr_to_na(ancestry_additional),
#                   source_GWAS_catalog = nr_to_na(source_GWAS_catalog),
#                   source_PMID = nr_to_na(source_PMID),
#                   cohorts_additional = nr_to_na(cohorts_additional)) %>%
#     tidyjson::as_tibble()
# }

# unwrap_cohort <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(
#       name_short = tidyjson::jstring(name_short),
#       name_full = tidyjson::jstring(name_full)) %>%
#     # Convert NR to NA in character columns
#     dplyr::mutate(name_short = nr_to_na(name_short),
#                   name_full = nr_to_na(name_full)) %>%
#     # Convert from tbl_json to plain and simple tibble
#     tidyjson::as_tibble()
# }

# unwrap_trait <- function(tbl_json) {
#
#   main_tbl <-
#     tbl_json %>%
#     tidyjson::spread_values(
#       id = tidyjson::jstring(id),
#       trait = tidyjson::jstring(label),
#       description = tidyjson::jstring(description),
#       url = tidyjson::jstring(url)
#     )
#
#   tbl_trait_categories <-
#     main_tbl %>%
#     enter('trait_categories', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'trait_categories') %>%
#     dplyr::mutate(trait_categories = purrr::map(trait_categories, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, trait_categories) %>%
#     tidyjson::as_tibble()
#
#
#   tbl_trait_synonyms <-
#     main_tbl %>%
#     enter('trait_synonyms', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'trait_synonyms') %>%
#     dplyr::mutate(trait_synonyms = purrr::map(trait_synonyms, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, trait_synonyms) %>%
#     tidyjson::as_tibble()
#
#   tbl_trait_mapped_terms <-
#     main_tbl %>%
#     enter('trait_mapped_terms', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'trait_mapped_terms') %>%
#     dplyr::mutate(trait_mapped_terms = purrr::map(trait_mapped_terms, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, trait_mapped_terms) %>%
#     tidyjson::as_tibble()
#
#   tbl_associated_pgs_ids <-
#     main_tbl %>%
#     enter('associated_pgs_ids', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'associated_pgs_ids') %>%
#     dplyr::mutate(associated_pgs_ids = purrr::map(associated_pgs_ids, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, associated_pgs_ids) %>%
#     tidyjson::as_tibble()
#
#   tbl_child_associated_pgs_ids <-
#     main_tbl %>%
#     enter('child_associated_pgs_ids', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'child_associated_pgs_ids') %>%
#     dplyr::mutate(child_associated_pgs_ids = purrr::map(child_associated_pgs_ids, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, child_associated_pgs_ids) %>%
#     tidyjson::as_tibble()
#
#   main_tbl %>%
#     dplyr::left_join(tbl_trait_categories, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_trait_synonyms, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_trait_mapped_terms, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_associated_pgs_ids, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_child_associated_pgs_ids, by = c('..page', 'array.index')) %>%
#     tidyjson::as_tibble()
# }

# unwrap_trait2 <- function(tbl_json) {
#
#   tbl_trait <- unwrap_trait(tbl_json)
#
#   tbl_child_traits <-
#     tbl_json %>%
#     tidyjson::spread_values(child_of = tidyjson::jstring(id)) %>%
#     enter('child_traits') %>%
#     dplyr::select(-child_traits_id) %>%
#     unwrap_trait() %>%
#     tidyjson::as_tibble()
#
#   dplyr::bind_rows(tbl_trait, tbl_child_traits)
# }

# unwrap_efotrait_extended <- function(tbl_json) {
#
#   main_tbl <-
#     tbl_json %>%
#     tidyjson::spread_values(
#       id = tidyjson::jstring(id),
#       label = tidyjson::jstring(label),
#       description = tidyjson::jstring(description),
#       url = tidyjson::jstring(url)
#     ) %>%
#     # Convert NR to NA in character columns
#     dplyr::mutate(
#       id = nr_to_na(id),
#       label = nr_to_na(label),
#       description = nr_to_na(description),
#       url = nr_to_na(url)
#     )
#
#   tbl_trait_categories <-
#     main_tbl %>%
#     enter('trait_categories', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'trait_categories') %>%
#     dplyr::mutate(trait_categories = purrr::map(trait_categories, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, trait_categories) %>%
#     tidyjson::as_tibble()
#
#
#   tbl_trait_synonyms <-
#     main_tbl %>%
#     enter('trait_synonyms', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'trait_synonyms') %>%
#     dplyr::mutate(trait_synonyms = purrr::map(trait_synonyms, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, trait_synonyms) %>%
#     tidyjson::as_tibble()
#
#   tbl_trait_mapped_terms <-
#     main_tbl %>%
#     enter('trait_mapped_terms', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'trait_mapped_terms') %>%
#     dplyr::mutate(trait_mapped_terms = purrr::map(trait_mapped_terms, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, trait_mapped_terms) %>%
#     tidyjson::as_tibble()
#
#   tbl_associated_pgs_ids <-
#     main_tbl %>%
#     enter('associated_pgs_ids', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'associated_pgs_ids') %>%
#     dplyr::mutate(associated_pgs_ids = purrr::map(associated_pgs_ids, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, associated_pgs_ids) %>%
#     tidyjson::as_tibble()
#
#   tbl_child_associated_pgs_ids <-
#     main_tbl %>%
#     enter('child_associated_pgs_ids', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'child_associated_pgs_ids') %>%
#     dplyr::mutate(child_associated_pgs_ids = purrr::map(child_associated_pgs_ids, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, child_associated_pgs_ids) %>%
#     tidyjson::as_tibble()
#
#   tbl_child_traits <-
#     main_tbl %>%
#     enter('child_traits', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'child_traits') %>%
#     dplyr::mutate(child_traits = purrr::map(child_traits, ~ nr_to_na(as.character(.)))) %>%
#     dplyr::select(..page, array.index, child_traits) %>%
#     tidyjson::as_tibble()
#
#   main_tbl %>%
#     dplyr::left_join(tbl_trait_categories, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_trait_synonyms, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_trait_mapped_terms, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_associated_pgs_ids, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_child_associated_pgs_ids, by = c('..page', 'array.index')) %>%
#     dplyr::left_join(tbl_child_traits, by = c('..page', 'array.index')) %>%
#     tidyjson::as_tibble()
#
# }



# unwrap_trait_categories <- function(tbl_json) {
#   tbl_json %>%
#     tidyjson::append_values_string(column.name = 'trait_categories') %>%
#     dplyr::mutate(trait_categories = nr_to_na(trait_categories)) %>%
#     tidyjson::as_tibble()
# }
#
# unwrap_trait_synonyms <- function(tbl_json) {
#   tbl_json %>%
#     tidyjson::append_values_string(column.name = 'trait_synonyms') %>%
#     dplyr::mutate(trait_synonyms = nr_to_na(trait_synonyms)) %>%
#     tidyjson::as_tibble()
# }
#
# unwrap_trait_mapped_terms <- function(tbl_json) {
#   tbl_json %>%
#     tidyjson::append_values_string(column.name = 'trait_mapped_terms') %>%
#     dplyr::mutate(trait_mapped_terms = nr_to_na(trait_mapped_terms)) %>%
#     tidyjson::as_tibble()
# }
#
# unwrap_associated_pgs_ids <- function(tbl_json) {
#   tbl_json %>%
#     tidyjson::append_values_string(column.name = 'associated_pgs_ids') %>%
#     dplyr::mutate(associated_pgs_ids = nr_to_na(associated_pgs_ids)) %>%
#     tidyjson::as_tibble()
# }


# unwrap_trait_category <- function(tbl_json) {
#   tbl_json %>%
#     tidyjson::spread_values(trait_category = tidyjson::jstring(label)) %>%
#     tibble::rowid_to_column('id') %>%
#     # Convert from tbl_json to plain and simple tibble
#     tidyjson::as_tibble()
# }

# unwrap_publication_extended <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(
#       id = tidyjson::jstring(id),
#       pubmed_id = tidyjson::jstring(PMID),
#       publication_date = tidyjson::jstring(date_publication),
#       publication = tidyjson::jstring(journal),
#       title = tidyjson::jstring(title),
#       author_fullname = tidyjson::jstring(firstauthor),
#       doi = tidyjson::jstring(doi),
#       authors = tidyjson::jstring(authors)
#     ) %>%
#     # Convert NR to NA in character columns
#     dplyr::mutate(
#       id = nr_to_na(id),
#       pubmed_id = nr_to_na(pubmed_id),
#       publication_date = lubridate::ymd(nr_to_na(publication_date)),
#       publication = nr_to_na(publication),
#       title = nr_to_na(title),
#       author_fullname = nr_to_na(author_fullname),
#       doi = nr_to_na(doi),
#       authors = nr_to_na(authors)
#     ) %>%
#     enter('associated_pgs_ids', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'associated_pgs_ids') %>%
#     dplyr::mutate(associated_pgs_ids = purrr::map(associated_pgs_ids, as.character)) %>%
#     tidyjson::as_tibble()
# }

# unwrap_released_score_ids <- function(tbl_json) {
#   tbl_json %>%
#     tidyjson::append_values_string(column.name = 'released_score_ids') %>%
#     dplyr::mutate(released_score_ids = nr_to_na(released_score_ids)) %>%
#     tidyjson::as_tibble()
# }
#
# unwrap_released_performance_ids <- function(tbl_json) {
#   tbl_json %>%
#     tidyjson::append_values_string(column.name = 'released_performance_ids') %>%
#     dplyr::mutate(released_performance_ids = nr_to_na(released_performance_ids)) %>%
#     tidyjson::as_tibble()
# }
#
# unwrap_released_publication_ids <- function(tbl_json) {
#   tbl_json %>%
#     tidyjson::append_values_string(column.name = 'released_publication_ids') %>%
#     dplyr::mutate(released_publication_ids = nr_to_na(released_publication_ids)) %>%
#     tidyjson::as_tibble()
# }

# unwrap_array_pgs_ids <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::gather_array() %>%
#     tidyjson::append_values_string(column.name = 'pgs_id') %>%
#     dplyr::select(-document.id, -array.index) %>%
#     dplyr::mutate(pgs_id = nr_to_na(pgs_id)) %>%
#     tidyjson::as_tibble()
#
# }

# unwrap_sample_set <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(pss_id = tidyjson::jstring(id)) %>%
#     tidyjson::as_tibble()
#
# }

# unwrap_performance_metric <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(
#       id = tidyjson::jstring(id),
#       pgs_id = tidyjson::jstring(associated_pgs_id),
#       phenotyping_reported = tidyjson::jstring(phenotyping_reported),
#       covariates = tidyjson::jstring(covariates),
#       performance_comments = tidyjson::jstring(performance_comments)
#     ) %>%
#     tidyjson::as_tibble()
#
# }

# unwrap_labels_value <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(value = tidyjson::jstring(value)) %>%
#     enter('labels', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'labels') %>%
#     dplyr::mutate(labels = purrr::map(labels, as.character)) %>%
#     dplyr::relocate(value, .after = labels) %>%
#     tidyjson::as_tibble()
#
# }

# unwrap_cohort_extended <- function(tbl_json) {
#
#   main_tbl <-
#     tbl_json %>%
#     tidyjson::spread_values(
#       name_short = tidyjson::jstring(name_short),
#       name_full = tidyjson::jstring(name_full)
#     ) %>%
#     # Convert NR to NA in character columns
#     dplyr::mutate(name_short = polygenic:::nr_to_na(name_short),
#                   name_full = polygenic:::nr_to_na(name_full)) %>%
#     tibble::rowid_to_column('id')
#
#   development_tbl <-
#     main_tbl %>%
#     dplyr::select(..JSON, id) %>%
#     polygenic:::enter('associated_pgs_ids', iterable = FALSE) %>%
#     polygenic:::enter('development', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'associated_pgs_id_in_development') %>%
#     dplyr::mutate(associated_pgs_id_in_development = purrr::map(associated_pgs_id_in_development, ~ polygenic:::nr_to_na(as.character(.)))) %>%
#     tidyjson::as_tibble()
#
#
#   evaluation_tbl <-
#     main_tbl %>%
#     dplyr::select(..JSON, id) %>%
#     polygenic:::enter('associated_pgs_ids', iterable = FALSE) %>%
#     polygenic:::enter('evaluation', iterable = FALSE) %>%
#     tidyjson::json_get_column(column_name = 'associated_pgs_id_in_evaluation') %>%
#     dplyr::mutate(associated_pgs_id_in_evaluation = purrr::map(associated_pgs_id_in_evaluation, ~ polygenic:::nr_to_na(as.character(.)))) %>%
#     tidyjson::as_tibble()
#
#   tidyjson::as_tibble(main_tbl) %>%
#     dplyr::left_join(development_tbl, by = 'id') %>%
#     dplyr::left_join(evaluation_tbl, by = 'id')
# }


#
# interval: Interval values
#
# - type:	 string
#          example: range
#          Type of interval: range, iqr (Interquartile), ci (Confidence Interval)
#
# - lower: number
#          example: 51
#          Lower bound of the interval
#
# - upper: number
#          example: 57
#          Upper bound of the interval
#

# unwrap_interval <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(
#       interval_type = tidyjson::jstring('type'),
#       interval_lower = tidyjson::jnumber('lower'),
#       interval_upper = tidyjson::jnumber('upper')
#     )
# }

#
# unwrap_demographic(): used for unwrapping sample_age and followup_time objects.
#

# unwrap_demographic <- function(tbl_json, parent_id) {
#
#   tbl1 <-
#     tbl_json %>%
#     tidyjson::spread_values(
#       estimate_type = tidyjson::jstring('estimate_type'),
#       estimate = tidyjson::jnumber('estimate'),
#       variability_type = tidyjson::jstring('variability_type'),
#       variability = tidyjson::jnumber('variability'),
#       unit = tidyjson::jstring('unit')
#     ) %>%
#     enter('interval', iterable = FALSE) %>%
#     unwrap_interval()
#
#   # tbl2 <-
#   #   tbl_json %>%
#   #   enter('interval', iterable = FALSE) %>%
#   #   unwrap_interval() %>%
#   #   dplyr::select(-`..resource`, -`..timestamp`)
#   #
#   # tbl3 <- dplyr::left_join(tbl1, tbl2, by = c('..page', 'array.index', parent_id)) %>%
#   #   dplyr::relocate(c('interval_type', 'interval_lower', 'interval_upper'), .after = 'estimate')
#
#   return(tbl1)
# }

# unwrap_metric <- function(tbl_json) {
#
#   tbl_json %>%
#     tidyjson::spread_values(
#       name_long = tidyjson::jstring('name_long'),
#       name_short = tidyjson::jstring('name_short'),
#       estimate = tidyjson::jnumber('estimate'),
#       ci_lower = tidyjson::jnumber('ci_lower'),
#       ci_upper = tidyjson::jnumber('ci_upper'),
#       se = tidyjson::jnumber('se')
#     )
# }
