read_pgs_scoring_file_data <- function(file) {

  col_types <- list(

    # Variant description columns
    rsID = vroom::col_character(),
    chr_name = vroom::col_character(),
    chr_position = vroom::col_integer(),
    effect_allele = vroom::col_character(),
    other_allele = vroom::col_character(),
    locus_name = vroom::col_character(),
    is_haplotype = vroom::col_logical(),
    is_diplotype = vroom::col_logical(),
    imputation_method = vroom::col_character(),
    variant_description = vroom::col_character(),
    inclusion_criteria = vroom::col_character(),

    # Weight information columns
    effect_weight = vroom::col_double(),
    is_interaction = vroom::col_logical(),
    is_dominant = vroom::col_logical(),
    is_recessive = vroom::col_logical(),

    dosage_0_weight = vroom::col_character(),
    dosage_1_weight = vroom::col_character(),
    dosage_2_weight = vroom::col_character(),

    # Other information
    OR = vroom::col_double(),
    HR = vroom::col_double(),
    allelefrequency_effect = vroom::col_double()

    # TODO: support the column allelefrequency_effect_<Ancestry>. Note that
    # <Ancestry> is variable making the name of this column unpredictable.
)

  col_names <- read_file_column_names(file)
  col_types2 <- vroom::cols(!!!col_types[col_names])

  tbl <- vroom::vroom(file, comment = '#', col_types = col_types2)

  return(tbl)
}

read_comment_block <- function(file, n_max = 200L) {

  lines <- vroom::vroom_lines(file = file, n_max = n_max, progress = FALSE)
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

  pgs_id <- extract_from_comment(comment_block_lines, pattern = '^#pgs_id=(PGS\\d{6})')
  pgs_name <- extract_from_comment(comment_block_lines, pattern = '^#pgs_name=(.+)')

  reported_trait <- extract_from_comment(comment_block_lines, pattern = '^#trait_reported=(.+)')
  mapped_trait <- extract_from_comment(comment_block_lines, pattern = '^#trait_mapped=(.+)')
  efo_trait <- extract_from_comment(comment_block_lines, pattern = '^#trait_efo=(.+)')

  genome_build <- extract_from_comment(comment_block_lines, pattern = '^#genome_build=(.+)')
  weight_type <- extract_from_comment(comment_block_lines, pattern = '^#weight_type=(.+)')
  number_of_variants <- extract_from_comment(comment_block_lines, pattern = '^#variants_number=(\\d+)')
  pgp_id <- extract_from_comment(comment_block_lines, pattern = '^#pgp_id=(PGP\\d{6})')
  citation <- extract_from_comment(comment_block_lines, pattern = '^#citation=(.+)')

  metadata_tbl <- tibble::tibble(
    pgs_id = nr_to_na(pgs_id),
    pgs_name = nr_to_na(pgs_name),
    reported_trait = nr_to_na(reported_trait),
    mapped_trait = nr_to_na(mapped_trait),
    efo_trait = nr_to_na(efo_trait),
    genome_build = nr_to_na(genome_build),
    weight_type = nr_to_na(weight_type),
    number_of_variants = as.integer(nr_to_na(number_of_variants)),
    pgp_id = nr_to_na(pgp_id),
    citation = nr_to_na(citation)
  )

  return(metadata_tbl)
}


read_one_pgs_scoring_file <- function(file, metadata_only = FALSE) {

  metadata_tbl <- read_pgs_scoring_file_metadata(file)
  if (metadata_only) {
    data_tbl <- NULL
  } else {
    data_tbl <- read_pgs_scoring_file_data(file)
  }

  return(list(metadata = metadata_tbl, data = data_tbl))

}

read_one_pgs_scoring_file_safe <- function(file, metadata_only = FALSE) {

tryCatch(expr = read_one_pgs_scoring_file(file = file, metadata_only = metadata_only),
         error = function(cnd) {
           message(glue::glue('Could not download {file} because of: {conditionMessage(cnd)}'))
           list(metadata = tibble::tibble(), data = tibble::tibble())
           }
         )
}

#' Read a polygenic scoring file
#'
#' This function imports a PGS scoring file. For more information about the
#' scoring file schema check \code{vignette("pgs-scoring-file", package =
#' "quincunx")}.
#'
#' @param source PGS scoring file. This can be specified in three forms: (i) a
#'   PGS identifier, e.g. \code{"PGS000001"}, (ii) a path to a local file, e.g.
#'   \code{"~/PGS000001.txt"} or \code{"~/PGS000001.txt.gz"} or (iii) a direct
#'   URL to the PGS Catalog FTP server, e.g.
#'   \code{"http://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS000001/ScoringFiles/PGS000001.txt.gz"}.
#' @param protocol Network protocol for communication with the PGS Catalog FTP
#'   server: either \code{"http"} or \code{"ftp"}.
#' @param metadata_only Whether to read only the comment block (header) from the
#'   scoring file.
#'
#' @return The returned value is a named list. The names are copied from the
#'   arguments passed in \code{source}. Each element of the list contains
#'   another list of two elements: \code{"metadata"} and \code{"data"}. The
#'   "metadata" element contains data parsed from the header of the PGS scoring
#'   file. The "data" element contains a data frame with as many rows as
#'   variants that constitute the PGS score. The columns can vary. There are
#'   mandatory and optional columns. The mandatory columns are those that
#'   identify the variant, effect allele (\code{effect_allele}), and its
#'   respective weight (\code{effect_weight}) in the score. The columns that
#'   identify the variant can either be the \code{rsID} or the combination of
#'   \code{chr_name} and \code{chr_position}. The "data" element will be
#'   \code{NULL} is argument \code{metadata_only} is \code{TRUE}. For more
#'   information about the scoring file schema check
#'   \code{vignette("pgs-scoring-file", package = "quincunx")}.
#'
#' @examples
#' \dontrun{
#' # Read a PGS scoring file by PGS ID
#' # (internally, it translates the PGS ID
#' #  to the corresponding FTP URL)
#' try(read_scoring_file("PGS000655"))
#'
#' # Equivalent to `read_scoring_file("PGS000655")`
#' url <- paste0(
#'   "http://ftp.ebi.ac.uk/",
#'   "pub/databases/spot/pgs/scores/",
#'   "PGS000655/ScoringFiles/",
#'   "PGS000655.txt.gz"
#' )
#' read_scoring_file(url)
#'
#'
#' # Reading from a local file
#' try(read_scoring_file("~/PGS000655.txt.gz"))
#' }
#' @export
read_scoring_file <- function(source, protocol = 'http', metadata_only = FALSE) {

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

  purrr::map(source2, read_one_pgs_scoring_file_safe, metadata_only = metadata_only)

}
