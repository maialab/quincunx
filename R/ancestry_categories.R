#' Ancestry categories and classes
#'
#'
#' A dataset containing the ancestry categories defined in NHGRI-EBI GWAS
#' Catalog framework (Table 1, \doi{10.1186/s13059-018-1396-2}). Ancestry
#' categories are assigned to samples with distinct and well-defined patterns of
#' genetic variation. You will find these categories in the variable
#' \code{ancestry_category} of the following objects: \linkS4class{scores},
#' \linkS4class{performance_metrics} and \linkS4class{sample_sets}. Ancestry
#' categories (\code{ancestry_category}) are further clustered into ancestry
#' classes (\code{ancestry_class}).
#'
#' @format A data frame with 19 ancestry categories (rows) and 6 columns:
#' \describe{
#'   \item{ancestry_category}{Ancestry category.}
#'   \item{ancestry_class}{To reduce the complexity associated with the many
#'   ancestry categories, some have been merged into higher-level groupings
#'   (\code{ancestry_class}). These groupings represent the current breadth of
#'   data in the PGS Catalog and are likely to change as more data is added.}
#'   \item{ancestry_class_symbol}{3-letter code for the \code{ancestry_class}
#'   e.g. \code{"EUR"} or \code{"MAE"}.}
#'   \item{ancestry_class_colour}{Hexadecimal colour code associated with
#'   ancestry groupings (\code{ancestry_class}). This can be useful when
#'   visually communicating about ancestries.}
#'   \item{definition}{Description of the ancestry category.}
#'   \item{examples}{Examples of detailed descriptions of sample ancestries
#'   included in the category.}
#' }
#'
#' @examples
#' ancestry_categories
#'
#' @source
#' \describe{
#'   \item{Table 1 of Moralles et al. (2018):}{\doi{10.1186/s13059-018-1396-2}}
#'   \item{PGS Catalog Ancestry Documentation:}{\url{http://www.pgscatalog.org/docs/ancestry/}}
#' }
#'
"ancestry_categories"
