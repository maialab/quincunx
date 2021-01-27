read_pgs_scoring_file_data <- function(file) {

  col_types <- list(
    variant_id = vroom::col_character(),
    rsID = vroom::col_character(),
    chr_name = vroom::col_character(),
    chr_position = vroom::col_integer(),
    effect_allele = vroom::col_character(),
    reference_allele = vroom::col_character(),
    effect_weight = vroom::col_double(),
    locus_name = vroom::col_character(),
    weight_type = vroom::col_character(),
    allelefrequency_effect = vroom::col_double(),
    is_interaction = vroom::col_logical(),
    is_recessive = vroom::col_logical(),
    is_haplotype = vroom::col_logical(),
    is_diplotype = vroom::col_logical(),
    imputation_method = vroom::col_character(),
    variant_description = vroom::col_character(),
    inclusion_criteria = vroom::col_character(),
    OR = vroom::col_character()
  )

  col_names <- read_file_column_names(file)
  col_types2 <- vroom::cols(!!!col_types[col_names])

  tbl <- vroom::vroom(file, comment = '#', col_types = col_types2)

  return(tbl)
}

read_comment_block <- function(file, n_max = 200L) {

  lines <- vroom::vroom_lines(file = file, n_max = n_max)
  comment_block <- grep('^#', lines, perl = TRUE, value = TRUE)

  return(comment_block)
}

extract_from_comment <- function(x, pattern) {

  m <- stringr::str_match(x, pattern)
  match <- m[first_non_na(m[, 1]), 2]

  return(match)
}


read_pgs_scoring_file_metadata <- function(file) {

  comment_block_lines <- read_comment_block(file)

  pgs_id <- extract_from_comment(comment_block_lines, pattern = '^# PGS ID = (PGS\\d{6})')
  reported_trait <- extract_from_comment(comment_block_lines, pattern = '# Reported Trait = (.+)')
  original_genome_build <- extract_from_comment(comment_block_lines, pattern = '# Original Genome Build = (.+)')
  number_of_variants <- extract_from_comment(comment_block_lines, pattern = '# Number of Variants = (\\d+)')
  pgp_id <- extract_from_comment(comment_block_lines, pattern = '# PGP ID = (PGP\\d{6})')
  citation <- extract_from_comment(comment_block_lines, pattern = '# Citation = (.+)')
  license <- extract_from_comment(comment_block_lines, pattern = '# LICENSE = (.+)')

  metadata_tbl <- tibble::tibble(
    pgs_id = pgs_id,
    reported_trait = reported_trait,
    original_genome_build = original_genome_build,
    number_of_variants = as.integer(number_of_variants),
    pgp_id = pgp_id,
    citation = citation,
    license = license
  )

  return(metadata_tbl)
}


read_one_pgs_scoring_file <- function(file) {

  metadata_tbl <- read_pgs_scoring_file_metadata(file)
  data_tbl <- read_pgs_scoring_file_data(file)

  return(list(metadata = metadata_tbl, data = data_tbl))

}

#' @export
get_pgs_scoring <- function(source, protocol = 'http', verbose = FALSE) {

  pgs_ids <- source[is_pgs_id(source)]

  ftp_resources <- glue::glue(
    '{protocol}://ftp.ebi.ac.uk',
    '/pub/databases/spot/pgs/scores/',
    '{pgs_ids}/ScoringFiles/{pgs_ids}.txt.gz'
    )

  # Replace PGS identifiers with their corresponding ftp resource URLs
  # All other sources, e.g., local files or other sources at left untouched.
  source2 <- source
  source2[is_pgs_id(source2)] <- ftp_resources
  source2 <- stats::setNames(object = source2, nm = source)

  purrr::map(source2, read_one_pgs_scoring_file)

}
