setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of PGS Catalog Traits
#'
#' The traits object consists of six slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of PGS Catalog traits. Each trait is an observation (row) in
#' the \code{traits} table --- main table. All tables have the column
#' \code{efo_id} as primary key.
#'
#' @slot traits A table of traits. Columns:
#' \describe{
#' \item{efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{parent_efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier of
#' the parent trait.}
#' \item{is_child}{Is this trait obtained because it is a child of other trait?}
#' \item{trait}{Trait name.}
#' \item{description}{Detailed description of the trait from EFO.}
#' \item{url}{External link to the EFO entry.}
#' }
#' @slot pgs_ids A table of associated polygenic score identifiers. Columns:
#' \describe{
#' \item{efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{parent_efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier of
#' the parent trait.}
#' \item{is_child}{Is this trait obtained because it is a child of other trait?}
#' \item{pgs_id}{Polygenic Score (PGS) identifier.}
#' }
#' @slot child_pgs_ids A table of polygenic score identifiers associated with
#'   the child traits. Columns:
#' \describe{
#' \item{efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{parent_efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier of
#' the parent trait.}
#' \item{is_child}{Is this trait obtained because it is a child of other trait?}
#' \item{child_pgs_id}{Polygenic Score (PGS) identifiers associated with child traits.}
#' }
#' @slot trait_categories A table of associated trait categories. Columns:
#' \describe{
#' \item{efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{parent_efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier of
#' the parent trait.}
#' \item{is_child}{Is this trait obtained because it is a child of other trait?}
#' \item{trait_category}{Trait category name.}
#' }
#' @slot trait_synonyms A table of associated trait synonyms. Columns:
#' \describe{
#' \item{efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{parent_efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier of
#' the parent trait.}
#' \item{is_child}{Is this trait obtained because it is a child of other trait?}
#' \item{trait_synonyms}{Trait synonyms.}
#' }
#' @slot trait_mapped_terms A table of associated external references,
#'   identifiers or other terms. Columns:
#' \describe{
#' \item{efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{parent_efo_id}{An \href{https://www.ebi.ac.uk/efo/}{EFO} identifier of
#' the parent trait.}
#' \item{is_child}{Is this trait obtained because it is a child of other trait?}
#' \item{trait_mapped_terms}{Trait mapped terms.}
#' }
#' @export
setClass(
  "traits",
  slots = c(
    traits = "tbl_df",
    pgs_ids = "tbl_df",
    child_pgs_ids = "tbl_df",
    trait_categories = "tbl_df",
    trait_synonyms = "tbl_df",
    trait_mapped_terms = "tbl_df"
  )
)

#' Constructor for the S4 traits object.
#'
#' Constructor for the S4 \linkS4class{traits} object.
#'
#' @param traits A \code{\link{s4traits_traits_tbl}} tibble.
#' @param pgs_ids A \code{\link{s4traits_pgs_ids_tbl}} tibble.
#' @param child_pgs_ids A \code{\link{s4traits_child_pgs_ids_tbl}} tibble.
#' @param trait_categories A \code{\link{s4traits_trait_categories_tbl}} tibble.
#' @param trait_synonyms A \code{\link{s4traits_trait_synonyms_tbl}} tibble.
#' @param trait_mapped_terms A \code{\link{s4traits_trait_mapped_terms_tbl}} tibble.
#'
#' @return An object of class \linkS4class{traits}.
#' @keywords internal
traits <-
  function(traits = s4traits_traits_tbl(),
           pgs_ids = s4traits_pgs_ids_tbl(),
           child_pgs_ids = s4traits_child_pgs_ids_tbl(),
           trait_categories = s4traits_trait_categories_tbl(),
           trait_synonyms = s4traits_trait_synonyms_tbl(),
           trait_mapped_terms = s4traits_trait_mapped_terms_tbl()) {

    s4_traits <- methods::new(
      "traits",
      traits = traits,
      pgs_ids = pgs_ids,
      child_pgs_ids = child_pgs_ids,
      trait_categories = trait_categories,
      trait_synonyms = trait_synonyms,
      trait_mapped_terms = trait_mapped_terms)

    return(s4_traits)
  }

s4traits_traits_tbl <- function(efo_id = character(),
                                parent_efo_id = character(),
                                is_child = logical(),
                                trait = character(),
                                description = character(),
                                url = character()) {


  tbl <- tibble::tibble(
    efo_id = efo_id,
    parent_efo_id = parent_efo_id,
    is_child = is_child,
    trait = trait,
    description = description,
    url = url
  )

  return(tbl)
}

s4traits_pgs_ids_tbl <- function(efo_id = character(),
                                 parent_efo_id = character(),
                                 is_child = logical(),
                                 pgs_id = character()) {


  tbl <- tibble::tibble(
    efo_id = efo_id,
    parent_efo_id = parent_efo_id,
    is_child = is_child,
    pgs_id = pgs_id
  )

  return(tbl)
}

s4traits_child_pgs_ids_tbl <- function(efo_id = character(),
                                       parent_efo_id = character(),
                                       is_child = logical(),
                                       child_pgs_id = character()) {


  tbl <- tibble::tibble(
    efo_id = efo_id,
    parent_efo_id = parent_efo_id,
    is_child = is_child,
    child_pgs_id = child_pgs_id
  )

  return(tbl)
}

s4traits_trait_categories_tbl <- function(efo_id = character(),
                                          parent_efo_id = character(),
                                          is_child = logical(),
                                          trait_categories = character()) {


  tbl <- tibble::tibble(
    efo_id = efo_id,
    parent_efo_id = parent_efo_id,
    is_child = is_child,
    trait_categories = trait_categories
  )

  return(tbl)
}

s4traits_trait_synonyms_tbl <- function(efo_id = character(),
                                        parent_efo_id = character(),
                                        is_child = logical(),
                                        trait_synonyms = character()) {


  tbl <- tibble::tibble(
    efo_id = efo_id,
    parent_efo_id = parent_efo_id,
    is_child = is_child,
    trait_synonyms = trait_synonyms
  )

  return(tbl)
}

s4traits_trait_mapped_terms_tbl <- function(efo_id = character(),
                                            parent_efo_id = character(),
                                            is_child = logical(),
                                            trait_mapped_terms = character()) {


  tbl <- tibble::tibble(
    efo_id = efo_id,
    parent_efo_id = parent_efo_id,
    is_child = is_child,
    trait_mapped_terms = trait_mapped_terms
  )

  return(tbl)
}

coerce_to_s4_traits <- function(lst_tbl = NULL) {

  if (is.null(lst_tbl)) {
    s4_traits <- traits()
    return(s4_traits)
  }

    s4_traits <- traits(
      traits = lst_tbl$traits,
      pgs_ids = lst_tbl$pgs_ids,
      child_pgs_ids = lst_tbl$child_pgs_ids,
      trait_categories = lst_tbl$trait_categories,
      trait_synonyms = lst_tbl$trait_synonyms,
      trait_mapped_terms = lst_tbl$trait_mapped_terms
    )

    s4_traits@traits <- drop_metadata_cols(s4_traits@traits)
    s4_traits@pgs_ids <- drop_metadata_cols(s4_traits@pgs_ids)
    s4_traits@child_pgs_ids <- drop_metadata_cols(s4_traits@child_pgs_ids)
    s4_traits@trait_categories <- drop_metadata_cols(s4_traits@trait_categories)
    s4_traits@trait_synonyms <- drop_metadata_cols(s4_traits@trait_synonyms)
    s4_traits@trait_mapped_terms <- drop_metadata_cols(s4_traits@trait_mapped_terms)

  return(s4_traits)
}
