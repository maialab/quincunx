#' Ancestry categories
#'
#' A dataset containing the ancestry categories defined in NHGRI-EBI GWAS
#' Catalog framework (Table 1,
#' \href{https://doi.org/10.1186/s13059-018-1396-2}{Morales et al. (2018)}).
#' Ancestry categories are assigned to samples with distinct and well-defined
#' patterns of genetic variation. You will find these categories in the variable
#' \code{ancestry} of the following objects: \linkS4class{scores},
#' \linkS4class{performance_metrics} and \linkS4class{sample_sets}.
#'
#' @format A data frame with 17 rows and 3 columns:
#' \describe{
#'   \item{ancestry}{Ancestry category.}
#'   \item{definition}{Description of the ancestry category.}
#'   \item{examples}{Examples of detailed descriptions for samples included in
#'   the category.}
#' }
#'
#' @examples
#' ancestry_categories
#'
#' @source \url{https://genomebiology.biomedcentral.com/articles/10.1186/s13059-018-1396-2/tables/1}
"ancestry_categories"
