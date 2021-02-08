setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Trait Categories
#'
#' The trait_categories object consists of two tables (slots) that combined form
#' a relational database of a subset of PGS Catalog trait categories. Each score
#' is an observation (row) in the \code{trait_categories} table (first table).
#'
#' @slot trait_categories A table of trait categories. Columns:
#' \describe{
#' \item{trait_category}{Trait category name.}
#' }
#' @slot traits A table of associated traits. Columns:
#' \describe{
#' \item{trait_category}{Trait category name.}
#' \item{efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{trait}{Trait name.}
#' \item{description}{Detailed description of the trait from EFO.}
#' \item{url}{External link to the EFO entry.}
#' }
#' @export
setClass(
  "trait_categories",
  slots = c(
    trait_categories = "tbl_df",
    traits = "tbl_df"
  )
)

#' Constructor for the S4 trait_categories object.
#'
#' Constructor for the S4 \linkS4class{trait_categories} object.
#'
#' @param trait_categories A \code{s4trait_categories_trait_categories_tbl} tibble.
#' @param traits A \code{s4trait_categories_traits_tbl} tibble.
#'
#' @return An object of class \linkS4class{trait_categories}.
#' @keywords internal
trait_categories <-
  function(trait_categories = s4trait_categories_trait_categories_tbl(),
           traits = s4trait_categories_traits_tbl()) {

    s4_trait_categories <- methods::new(
      "trait_categories",
      trait_categories = trait_categories,
      traits = traits)

    return(s4_trait_categories)
  }

s4trait_categories_trait_categories_tbl <- function(trait_category = character()) {

  tbl <- tibble::tibble(
    trait_category = trait_category
  )

  return(tbl)
}

s4trait_categories_traits_tbl <- function(trait_category = character(),
                                          efo_id = character(),
                                          trait = character(),
                                          description = character(),
                                          url = character()) {

  tbl <- tibble::tibble(
    trait_category = trait_category,
    efo_id = efo_id,
    trait = trait,
    description = description,
    url = url
  )

  return(tbl)
}

coerce_to_s4_trait_categories <- function(lst_tbl = NULL) {

  if (is.null(lst_tbl)) {
    s4_trait_categories <- trait_categories()
    return(s4_trait_categories)
  }

  s4_trait_categories <- trait_categories(
    trait_categories = lst_tbl$trait_categories,
    traits = lst_tbl$traits
  )

  s4_trait_categories@trait_categories <- drop_metadata_cols(s4_trait_categories@trait_categories)
  s4_trait_categories@traits <- drop_metadata_cols(s4_trait_categories@traits)

  return(s4_trait_categories)
}
