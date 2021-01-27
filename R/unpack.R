# unpack <- function(tbl_json, obj, ...) {
#
#   fn <- FALSE
#
#   if(identical(obj, 'Score')) fn <- unpack_score
#   # if(identical(obj, 'Sample')) fn <- unpack_sample
#   # if(identical(obj, 'SampleSet')) fn <- unpack_sample_set
#   if(identical(obj, 'Cohort_extended')) fn <- unpack_cohort_extended
#   # if(identical(obj, 'Demographic')) fn <- unpack_demographic
#   # if(identical(obj, 'EFOtrait')) fn <- unpack_efotrait
#   # if(identical(obj, 'Trait')) fn <- unpack_trait
#   if(identical(obj, 'Publication')) fn <- unpack_publication
#   # if(identical(obj, 'Publication_extended')) fn <- unpack_publication_extended
#   # if(identical(obj, 'TraitCategory')) fn <- unpack_trait_category
#   if(identical(obj, 'Release')) fn <- unpack_release
#   if(identical(obj, 'PerformanceMetric')) fn <- unpack_performance_metric
#   if(identical(obj, 'EFOTrait_extended')) fn <- unpack_efo_trait_extended
#
#   if(!is.function(fn)) stop('No unpack function for object `obj`: ', obj)
#   else return(fn(tbl_json, ...))
#
# }

# unpack_score <- function(tbl_json) {
#
#   scores <- unwrap(tbl_json, 'Score')
#
#   publication <- tbl_json %>%
#     enter('publication', iterable = FALSE) %>%
#     unwrap('Publication')
#
#   samples_variants <-
#     tbl_json %>%
#     enter('samples_variants') %>%
#     unpack_sample(
#       parent_id = 'samples_variants_id',
#       samples_name = 'samples_variants',
#       sample_age_name = 'sample_age_variants',
#       followup_time_name = 'followup_time_variants',
#       cohorts_name = 'cohorts_variants'
#     )
#
#   samples_training <-
#     tbl_json %>%
#     enter('samples_training') %>%
#     unpack_sample(
#       parent_id = 'samples_training_id',
#       samples_name = 'samples_training',
#       sample_age_name = 'sample_age_training',
#       followup_time_name = 'followup_time_training',
#       cohorts_name = 'cohorts_training'
#     )
#
#   trait_efo <- tbl_json %>%
#     enter('trait_efo') %>%
#     unwrap('EFOtrait')
#
#   c(
#     list(scores = scores, publication = publication),
#     samples_variants,
#     samples_training,
#     list(trait_efo = trait_efo)
#   )
# }

# unpack_publication <- function(tbl_json) {
#
#   publication <- unwrap(tbl_json, 'Publication')
#
#   list(publication = publication)
#
# }

# unpack_publication_extended <- function(tbl_json) {
#
#   publication <- unwrap(tbl_json, 'Publication_extended')
#
#   list(publication = publication)
#
# }


# unpack_performance_metric <- function(tbl_json) {
#
#   performance_metrics <- unwrap(tbl_json, 'PerformanceMetric')
#
#   publication <- tbl_json %>%
#     enter('publication', iterable = FALSE) %>%
#     unwrap('Publication')
#
#   effect_sizes <- tbl_json %>%
#     enter('performance_metrics', iterable = FALSE) %>%
#     enter('effect_sizes') %>%
#     unwrap_metric()
#
#   class_acc <- tbl_json %>%
#     enter('performance_metrics', iterable = FALSE) %>%
#     enter('class_acc') %>%
#     unwrap_metric()
#
#   othermetrics <- tbl_json %>%
#     enter('performance_metrics', iterable = FALSE) %>%
#     enter('othermetrics') %>%
#     unwrap_metric()
#
#   c(
#     list(performance_metrics = performance_metrics,
#          publication = publication),
#     unpack(enter(tbl_json, 'sampleset', iterable = FALSE), 'SampleSet'),
#     list(
#       effect_sizes = effect_sizes,
#       class_acc = class_acc,
#       othermetrics = othermetrics
#     )
#   )
# }

# unpack_sample <- function(tbl_json,
#                           parent_id,
#                           samples_name = 'samples',
#                           sample_age_name = 'sample_age',
#                           followup_time_name = 'followup_time',
#                           cohorts_name = 'cohorts') {
#
#   samples <- tbl_json %>%
#     unwrap_sample()
#
#   sample_age <- tbl_json %>%
#     enter('sample_age', iterable = FALSE) %>%
#     unwrap_demographic(parent_id = parent_id)
#
#   followup_time <- tbl_json %>%
#     enter('followup_time', iterable = FALSE) %>%
#     unwrap_demographic(parent_id = parent_id)
#
#   cohorts <- tbl_json %>%
#     enter('cohorts') %>%
#     unwrap('Cohort')
#
#   lst <- list(
#     samples = samples,
#     sample_age = sample_age,
#     followup_time = followup_time,
#     cohorts = cohorts
#   )
#
#   nm <- c(samples_name, sample_age_name, followup_time_name, cohorts_name)
#
#   setNames(lst, nm)
# }

# unpack_sample_set <- function(tbl_json) {
#
#   sample_set <- unwrap(tbl_json, 'SampleSet')
#
#   tbl <- tibble::add_column(tbl_json, sample_set['pss_id'], .before = 2)
#   # tbl <- dplyr::relocate(sample_set, 'pss_id', .before = 2)
#   c(list(sample_set = sample_set), unpack_sample(enter(tbl, 'samples'), parent_id = 'samples_id'))
# }

# unpack_release <- function(tbl_json) {
#
#   releases <- unwrap(tbl_json, 'Release')
#
#   released_score_ids <- tbl_json %>%
#     enter('released_score_ids') %>%
#     unwrap('released_score_ids')
#
#   released_performance_ids <- tbl_json %>%
#     enter('released_performance_ids') %>%
#     unwrap('released_performance_ids')
#
#   released_publication_ids <- tbl_json %>%
#     enter('released_publication_ids') %>%
#     unwrap('released_publication_ids')
#
#   list(releases = releases,
#        released_score_ids = released_score_ids,
#        released_performance_ids = released_performance_ids,
#        released_publication_ids = released_publication_ids
#   )
#
# }

# unpack_efo_trait_extended <- function(tbl_json) {
#
#   EFOtrait <- unwrap(tbl_json, 'EFOtrait')
#
#   trait_categories <- tbl_json %>%
#     enter('trait_categories') %>%
#     unwrap('trait_categories')
#
#   trait_synonyms <- tbl_json %>%
#     enter('trait_synonyms') %>%
#     unwrap('trait_synonyms')
#
#   trait_mapped_terms <- tbl_json %>%
#     enter('trait_mapped_terms') %>%
#     unwrap('trait_mapped_terms')
#
#   associated_pgs_ids <- tbl_json %>%
#     enter('associated_pgs_ids') %>%
#     unwrap('associated_pgs_ids')
#
#   list(
#     EFOtrait = EFOtrait,
#     trait_categories = trait_categories,
#     trait_synonyms = trait_synonyms,
#     trait_mapped_terms = trait_mapped_terms,
#     associated_pgs_ids = associated_pgs_ids
#   )
# }

# unpack_trait <- function(tbl_json) {
#
#   list(traits = unwrap_trait2(tbl_json))
# }
#
# unpack_efo_trait_extended <- function(tbl_json) {
#
#   EFOtrait_extended <- unwrap(tbl_json, 'EFOTrait_extended')
#
#   list(EFOtrait_extended = EFOtrait_extended)
# }

# unpack_cohort_extended <- function(tbl_json) {
#
#   cohort <- tbl_json %>%
#     unwrap('Cohort_extended')
#
#   list(cohort = cohort)
# }
